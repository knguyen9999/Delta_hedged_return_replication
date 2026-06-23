library(arrow)
library(tidyverse)
library(fs)
library(duckdb)

#Setting up the data path
setwd("/Users/kainguyen/Desktop/Paper_replication")
# source("delta_hedged_db_database_building.R")  # Comment out to avoid View() issues

con <- dbConnect(
  duckdb(),
  dbdir = "delta_hedged_returns.duckdb",
  read_only = TRUE
)

#Create variable names for available data in duckdb
master_data_sp500 <- tbl(con, "master_data_sp500")
ff_data <- tbl(con, "ff_rates")
distribution_file <- tbl(con, "distribution_file")
sp500_secid <- 108105

#OPTION_FILTERING======================================
atm_options_selected <- master_data_sp500 |>
  filter(
    # Basic liquidity filters (standard in option literature)
    volume > 0,
    best_bid > 0,
    best_offer > best_bid,
    mid_price >= 0.125,  # Minimum $0.125 mid-price (common)
    am_settlement == 0,  # European-style only (standard)
    !is.na(delta)
  ) |>
  mutate(
    spread_ratio = spread / mid_price
  ) |>
  # CONSERVATIVE FILTERS (avoiding over-filtering)
  filter(
    days_to_expiry > 30
  )

  #remove some unused columns
atm_options_selected <- atm_options_selected |>
  select(-c(
      open_interest,
      impl_volatility,
      cfadj,
      am_settlement,
      root,
      suffix,
      forward_price,
      expiry_indicator
  ))

#Calculate ATM distance
atm_options_with_moneyness <- atm_options_selected |>
  mutate(
    atm_distance = abs(strike - stock_price)
  )

#For each date-option type, select the most at-the-money option
selected_options_pre_arb <- atm_options_with_moneyness |>
  group_by(date, cp_flag) |>
  #shortest maturity > 30 days first
  filter(days_to_expiry == min(days_to_expiry)) |>
  #then find closest to atm among those
  slice_min(atm_distance, n = 1, with_ties = FALSE) |>
  ungroup()

#Filter arbitrage limit
selected_options <- selected_options_pre_arb |>
  mutate(
    #Call bounds: max(0, S - K*exp(-r*T)) <= C <= S
    call_lower_bound = pmax(0, stock_price - strike * exp(-rf_daily * days_to_expiry)),
    call_upper_bound = stock_price,
    #Put bounds: max(0, K*exp(-r*T) - S) <= P <= K*exp(-r*T)
    put_lower_bound = pmax(0, strike * exp(-rf_daily * days_to_expiry) - stock_price),
    put_upper_bound = strike * exp(-rf_daily * days_to_expiry),
    # Check arbitrage conditions
    no_arb_cond = case_when(
      cp_flag == "C" ~ (mid_price >= call_lower_bound) & (mid_price <= call_upper_bound),
      cp_flag == "P" ~ (mid_price >= put_lower_bound) & (mid_price <= put_upper_bound),
      TRUE ~ FALSE
    )
  ) |>
  #Keep only options that satisfy no-arbitrage conditions
  filter(no_arb_cond == TRUE) |>
  #Remove temporary columns
  select(-call_lower_bound, -call_upper_bound, -put_lower_bound, -put_upper_bound, 
         -no_arb_cond)

#DELTA-HEDGED RETURNS TABLE===========================

#Filter out the dividends using IvyDB distribution file
#get relevant dividend dates from distribution_file
# #Filter for regular cash dividends only (exclude stock splits, special dividends, etc.)

dividend_dates <- distribution_file |>
  filter(
    secid == sp500_secid,
    !is.na(ex_date),
    # Filter for regular dividends only (types 1 and % per IvyDB documentation)
    distr_type %in% c("1", "%"),
    # Exclude cancelled dividends (cancel_flag = 1 means cancelled)
    is.na(cancel_flag) | cancel_flag != 1,
    # Exclude liquidating dividends (liquid_flag = 1 means liquidating)  
    is.na(liquid_flag) | liquid_flag != 1
  ) |>
  select(secid, ex_date) |>
  distinct()

#Identify options with dividends during their life
#Exclude options where ex-dividend date falls between option start and expiration
options_with_dividends <- selected_options |>
  inner_join(
    dividend_dates,
    by = "secid",
    relationship = "many-to-many"
  ) |>
  filter(
    ex_date > date,        # Dividend after option purchase
    ex_date <= exdate      # Dividend before/at expiration
  ) |>
  select(optionid) |>
  distinct()

#Remove options with dividends from the selected options
selected_options_final <- selected_options |>
  anti_join(options_with_dividends, by = "optionid")


#get all stock prices for selected securities within date ranges (keep in DuckDB)
# Get all daily option data for selected options (keep in DuckDB)
all_option_daily <- master_data_sp500 |>
  # Join with selected options to get relevant options and their date ranges
  inner_join(
    selected_options_final |> 
      select(optionid, start_date = date, end_date = exdate), 
    by = "optionid"
  ) |>
  filter(
    date >= start_date,
    date <= end_date
  ) |>
  select(optionid, date, mid_price, delta, vega, strike, spread_ratio = spread)

#create option metadata (keep in DuckDB)
option_metadata <- selected_options_final |>
  select(optionid, secid, cp_flag, 
          start_date = date, exdate, moneyness, days_to_expiry)

#join everything together and apply all filters (all in DuckDB)
delta_hedged_data <- option_metadata |>
  # Add daily option data
  left_join(all_option_daily, by = "optionid") |>
  # Add stock price and risk-free rate (already in master_data)
  left_join(
    master_data_sp500 |> 
      select(date, stock_price, stock_volume, rf_daily) |> 
      distinct(),
    by = "date"
  ) |>
  # Filter to the specific option's trading period
  filter(
    date >= start_date,
    date <= exdate,
    # Keep only observations with complete data
    !is.na(mid_price),
    !is.na(stock_price),
    !is.na(delta),
    !is.na(vega),
    !is.na(rf_daily)
  ) |>
  # Calculate time-based fields
  mutate(
    days_since_start = as.integer(date - start_date),
    days_to_expiry = as.integer(exdate - date)
  ) |>
  # Remove options too close to expiration (< 1 day)
  filter(days_to_expiry >= 1) |>
  # Arrange for proper time series
  arrange(start_date, cp_flag, optionid, date)
colnames(delta_hedged_data)
delta_hedged_data |> head()

#CALCULATION===============================
delta_hedged_calculation <- delta_hedged_data |>
  group_by(start_date, cp_flag, optionid) |>
  mutate(
    #Lagged values for calculation
    lag_stock = lag(stock_price),
    lag_mid_price = lag(mid_price),
    lag_delta = lag(delta),
    lag_rf_daily = lag(rf_daily),
    lag_date = lag(date),
    
    #Stock price changes
    stock_change = stock_price - lag_stock,
    
    #Option price changes
    option_change = mid_price - lag_mid_price,
    
    #Days between observations
    day_diff = as.numeric(date - lag_date),
    
    #Interest (C - Delta * S)
    interest_component = case_when(
      is.na(lag_rf_daily) ~ 0,
      TRUE ~ lag_rf_daily * day_diff * (lag_mid_price - lag_delta * lag_stock)
    ),
    
    #Delta-hedged gain for each day
    #Pi(t,t+1) = C(t+1) - C(t) - Delta(t)*[S(t+1) - S(t)] - interest
    daily_gain = case_when(
      is.na(stock_change) ~ 0,  # First observation
      TRUE ~ option_change - lag_delta * stock_change - interest_component
    ),
    
    # Cumulative gain
    cumulative_gain = cumsum(daily_gain),
    
    #Initial portfolio value
    #For calls: |Delta*S - C|, for puts: |P - Delta*S| (delta is negative for puts)
    initial_value = first(abs(delta * stock_price - mid_price)),
    
    # Scaled return (as percentage)
    scaled_return = (cumulative_gain / initial_value) * 100,
    
    # Daily scaled return
    daily_scaled_return = (daily_gain / initial_value) * 100
  ) |>
  ungroup() |>
  select(-starts_with("lag_"))

# Get final returns for each option (collect only final results)
delta_hedged_final <- delta_hedged_calculation |>
  group_by(optionid, cp_flag) |>
  filter(date == max(date)) |>  # Get last day only
  select(optionid, cp_flag, 
         total_return_pct = scaled_return,
         cumulative_gain,
         initial_value) |>
  ungroup() |>
  collect()  # ONLY COLLECT THE FINAL RESULTS

print(paste("Final data:", nrow(delta_hedged_final), "option contracts"))

# Display results
cat("=== CAO-HAN DELTA-HEDGED RETURNS - S&P 500 INDEX OPTIONS ===\n\n")

# Call options statistics
call_stats <- delta_hedged_final |>
  filter(cp_flag == "C") |>
  summarise(
    n_obs = n(),
    mean_return = mean(total_return_pct, na.rm = TRUE),
    median_return = median(total_return_pct, na.rm = TRUE),
    std_return = sd(total_return_pct, na.rm = TRUE),
    pct_positive = mean(total_return_pct > 0, na.rm = TRUE) * 100
  )

cat("Call Options Summary:\n")
cat("N =", call_stats$n_obs, "\n")
cat("Mean Return:", round(call_stats$mean_return, 2), "%\n")
cat("Median Return:", round(call_stats$median_return, 2), "%\n")
cat("Std Dev:", round(call_stats$std_return, 2), "%\n")
cat("% Positive:", round(call_stats$pct_positive, 2), "%\n")

# Put options statistics
put_stats <- delta_hedged_final |>
  filter(cp_flag == "P") |>
  summarise(
    n_obs = n(),
    mean_return = mean(total_return_pct, na.rm = TRUE),
    median_return = median(total_return_pct, na.rm = TRUE),
    std_return = sd(total_return_pct, na.rm = TRUE),
    pct_positive = mean(total_return_pct > 0, na.rm = TRUE) * 100
  )

cat("\nPut Options Summary:\n")
cat("N =", put_stats$n_obs, "\n")
cat("Mean Return:", round(put_stats$mean_return, 2), "%\n")
cat("Median Return:", round(put_stats$median_return, 2), "%\n")
cat("Std Dev:", round(put_stats$std_return, 2), "%\n")
cat("% Positive:", round(put_stats$pct_positive, 2), "%\n")

# T-tests for statistical significance
call_ttest <- t.test(delta_hedged_final$total_return_pct[delta_hedged_final$cp_flag == "C"])
put_ttest <- t.test(delta_hedged_final$total_return_pct[delta_hedged_final$cp_flag == "P"])

cat("\n=== STATISTICAL TESTS ===\n")
cat("Call Options - Mean Delta-Hedged Return:\n")
cat("t-stat:", round(call_ttest$statistic, 2), ", p-value:", format(call_ttest$p.value, scientific = TRUE), "\n")

cat("\nPut Options - Mean Delta-Hedged Return:\n")
cat("t-stat:", round(put_ttest$statistic, 2), ", p-value:", format(put_ttest$p.value, scientific = TRUE), "\n")

#==============================================================================#
# CREATE STANDARDIZED METRICS FOR COMPARISON
#==============================================================================#

# Create standardized comparison metrics
comparison_metrics <- list(
  method = "Cao-Han (2009)",
  data = "S&P 500 Index Options",
  
  # Sample size
  n_call_options = call_stats$n_obs,
  n_put_options = put_stats$n_obs,
  
  # Returns - Calls
  call_mean_return = call_stats$mean_return,
  call_median_return = call_stats$median_return,
  call_std_return = call_stats$std_return,
  call_pct_positive = call_stats$pct_positive,
  
  # Returns - Puts
  put_mean_return = put_stats$mean_return,
  put_median_return = put_stats$median_return,
  put_std_return = put_stats$std_return,
  put_pct_positive = put_stats$pct_positive,
  
  # Statistical significance
  call_t_stat = call_ttest$statistic[[1]],
  call_p_value = call_ttest$p.value,
  put_t_stat = put_ttest$statistic[[1]],
  put_p_value = put_ttest$p.value
)

cat("\n=== STANDARDIZED METRICS FOR COMPARISON ===\n")
cat("Method: Cao-Han (2009)\n")
cat("Calls - Mean Return:", round(comparison_metrics$call_mean_return, 2), "%\n")
cat("Puts - Mean Return:", round(comparison_metrics$put_mean_return, 2), "%\n")

write.csv(delta_hedged_final, "cao_han_sp500_delta_hedged_returns.csv", row.names = FALSE)
saveRDS(comparison_metrics, "cao_han_sp500_comparison_metrics.rds")
