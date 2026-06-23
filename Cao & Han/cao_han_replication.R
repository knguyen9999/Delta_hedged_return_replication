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
dbListTables(con)
dbDisconnect(con)
#Create variable names for available data in duckdb
option_price <- tbl(con, "option_price")
security_price <- tbl(con, "security_price")
ff_data <- tbl(con, "ff_rates")
distribution_file <- tbl(con, "distribution_file")

#Filter data for the sample period (1996-2009 as in Cao-Han)
sample_start <- as.Date("1996-01-01")
sample_end <- as.Date("2009-12-31")

print(paste("Processing data from", sample_start, "to", sample_end))

  #SECURITY DATA PREPARATION ================================
  #Create base security data directly from IvyDB security_price
  security_full <- security_price |>
    filter(
      date >= sample_start,
      date <= sample_end
    ) |>
    select(secid, date, close, security_vol = volume)

  print("Security data prepared")

  #OPTION_FILTERING======================================
  atm_options_selected <- option_price |>
    filter(
      date >= sample_start,
      date <= sample_end,
      # Basic liquidity filters (standard in option literature)
      volume > 0,
      best_bid > 0,
      best_offer > best_bid,
      (best_bid + best_offer) / 2 >= 0.125,  # Minimum $0.125 mid-price (common)
      am_settlement == 0,  # European-style only (standard)
      !is.na(delta)
    ) |>
    mutate(
      mid_quote = (best_bid + best_offer) / 2,
      strike_price = strike_price / 1000, # Convert to dollars
      days_to_maturity = as.numeric(exdate - date),
      spreadad = best_offer - best_bid,
      spread_ratio = bid_ask_spread / mid_quote
    ) |>
    # CONSERVATIVE FILTERS (avoiding over-filtering)
    filter(
      days_to_maturity > 30
    ) |>
    # Join with stock data and risk-free rate
    left_join(ff_data |> select(date, rf_daily), by = "date") |>
    left_join(security_full, by = c("secid", "date")) |>
    # Basic stock price filter
    filter(
      !is.na(close)
    ) |>
    mutate(close = abs(close))

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

  #Calculate the moneyness
  atm_options_with_moneyness <- atm_options_selected |>
    mutate(
      moneyness = close / strike_price,
      atm_distance = abs(strike_price - close)
    )

  #For each stock-date-option type, select the most at-the-money option
  selected_options_pre_arb <- atm_options_with_moneyness |>
    group_by(secid, date, cp_flag) |>
    #shortest maturity > 30 days first
    filter(days_to_maturity == min(days_to_maturity)) |>
    #then find closest to atm among those
    slice_min(atm_distance, n = 1, with_ties = FALSE) |>
    ungroup()

  #arbitrage limit
  selected_options <- selected_options_pre_arb |>
    mutate(
      #Call bounds: max(0, S - K*exp(-r*T)) <= C <= S
      call_lower_bound = pmax(0, close - strike_price * exp(-rf_daily * days_to_maturity)),
      call_upper_bound = close,
      #Put bounds: max(0, K*exp(-r*T) - S) <= P <= K*exp(-r*T)
      put_lower_bound = pmax(0, strike_price * exp(-rf_daily * days_to_maturity) - close),
      put_upper_bound = strike_price * exp(-rf_daily * days_to_maturity),
      # Check arbitrage conditions
      no_arb_cond = case_when(
        cp_flag == "C" ~ (mid_quote >= call_lower_bound) & (mid_quote <= call_upper_bound),
        cp_flag == "P" ~ (mid_quote >= put_lower_bound) & (mid_quote <= put_upper_bound),
        TRUE ~ FALSE
      )
    ) |>
    #Keep only options that satisfy no-arbitrage conditions
    filter(no_arb_cond == TRUE) |>
    #Remove temporary columns
    select(-call_lower_bound, -call_upper_bound, -put_lower_bound, -put_upper_bound, 
           -no_arb_cond)

  print("Option filtering completed")

  #DELTA-HEDGED RETURNS TABLE===========================

  print("Starting dividend filtering...")

  #Filter out the dividends using IvyDB distribution file
  #get relevant dividend dates from distribution_file
  #Filter for regular cash dividends only (exclude stock splits, special dividends, etc.)


  dividend_dates <- distribution_file |>
    filter(
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

  print("Dividend filtering completed")

  # Get all daily option data for selected options (keep in DuckDB)
  all_option_daily <- option_price |>
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
    # Use the same calculations as in initial filtering for consistency
    mutate(
      mid_quote = (best_bid + best_offer) / 2,
      strike_price = strike_price / 1000,
      bid_ask_spread = best_offer - best_bid,
      spread_ratio = bid_ask_spread / mid_quote
    ) |>
    select(optionid, date, mid_quote, delta, vega, strike_price, spread_ratio)

  #get all stock prices for selected securities within date ranges (keep in DuckDB)
  all_stock_daily <- security_full |>
    # Join with selected options to get all relevant date ranges
    inner_join(
      selected_options_final |> 
        select(secid, start_date = date, end_date = exdate),
      by = "secid"
    ) |>
    filter(
      date >= start_date,
      date <= end_date,
      # Stock data quality filters for research
      !is.na(close),        # Must have closing price
      close > 0,            # Positive stock price
      !is.na(security_vol)  # Must have volume data
    ) |>
    mutate(
      close = abs(close)    # Ensure positive prices
    ) |>
    select(secid, date, close, security_vol)

  #create option metadata (keep in DuckDB)
  option_metadata <- selected_options_final |>
    select(optionid, secid, cp_flag, 
           start_date = date, exdate, moneyness, days_to_maturity)

  #join everything together and apply all filters (all in DuckDB)
  delta_hedged_data <- option_metadata |>
    # Add daily option data
    left_join(all_option_daily, by = "optionid") |>
    # Add stock data
    left_join(all_stock_daily, by = c("secid", "date")) |>
    # Add risk-free rate
    left_join(ff_data |> select(date, rf_daily), by = "date") |>
    # Filter to the specific option's trading period
    filter(
      date >= start_date,
      date <= exdate,
      # Keep only observations with complete data
      !is.na(mid_quote),
      !is.na(close),
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
    arrange(secid, start_date, cp_flag, optionid, date)

delta_hedged_data |> head()
  print("Delta-hedged data preparation completed")

  #CALCULATION===============================
  delta_hedged_calculation <- delta_hedged_data |>
    group_by(secid, start_date, cp_flag, optionid) |>
    mutate(
      #Lagged values for calculation
      lag_close = lag(close),
      lag_mid_quote = lag(mid_quote),
      lag_delta = lag(delta),
      lag_rf_daily = lag(rf_daily),
      lag_date = lag(date),
      
      #Stock price changes
      stock_change = close - lag_close,
      
      #Option price changes
      option_change = mid_quote - lag_mid_quote,
      
      #Days between observations
      day_diff = as.numeric(date - lag_date),
      
      #Interest (C - Delta * S)
      interest_component = case_when(
        is.na(lag_rf_daily) ~ 0,
        TRUE ~ lag_rf_daily * day_diff * (lag_mid_quote - lag_delta * lag_close)
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
      initial_value = first(abs(delta * close - mid_quote)),
      
      # Scaled return (as percentage)
      scaled_return = (cumulative_gain / initial_value) * 100,
      
      # Daily scaled return
      daily_scaled_return = (daily_gain / initial_value) * 100
    ) |>
    ungroup() |>
    select(-starts_with("lag_"))

  #SUMMARIZATION=================== (Only collect at the very end)
  delta_hedged_summary <- delta_hedged_calculation |>
    group_by(secid, start_date, cp_flag, optionid) |>
    summarise(
      # Count of days
      n_days = n(),
      
      # Initial and final values
      C_initial = first(mid_quote),
      C_final = last(mid_quote),
      S_initial = first(close),
      S_final = last(close),
      delta_initial = first(delta),
      vega_initial = first(vega),
      strike_price = first(strike_price),
      
      # Total gains (final cumulative values)
      total_gain = last(cumulative_gain),
      
      # Scaling factor: |Delta * S - C| for both calls and puts
      scaling_factor = first(initial_value),
      
      # Additional info
      initial_date = first(date),
      final_date = last(date),
      days_to_maturity_initial = first(days_to_maturity),
      moneyness_initial = first(moneyness),
      
      .groups = "drop"
    ) |> 
    mutate(
      delta_hedged_return = total_gain / scaling_factor
    )

# Calculate stock volatility in DuckDB first
stock_volatility <- security_price |>
  filter(date >= sample_start, date <= sample_end) |>
  select(secid, date, close) |>
  mutate(close = abs(close)) |>
  arrange(secid, date) |>
  group_by(secid) |>
  mutate(
    stock_return = (close / lag(close)) - 1
  ) |>
  filter(!is.na(stock_return)) |>
  summarise(
    stock_volatility = sqrt(252) * sqrt(sum(stock_return * stock_return) / (n() - 1)), # Annualized volatility
    .groups = "drop"
  )

# Use the summary data directly for analysis (collect only final results)
full_data <- delta_hedged_summary |>
  left_join(stock_volatility, by = "secid") |>
  mutate(
    # Convert to percentages for display
    moneyness_pct = (S_initial / strike_price) * 100,
    delta_hedged_return_pct = delta_hedged_return * 100,
    delta_hedged_gain_scaled = (total_gain / scaling_factor) * 100,
    vol_quintile = ntile(stock_volatility, 5)
  ) |>
  filter(!is.na(delta_hedged_return) & is.finite(delta_hedged_return)) |>
  collect()  # ONLY COLLECT THE FINAL SUMMARY RESULTS

print(paste("Final data:", nrow(full_data), "option contracts"))

#REPLICATION===================
create_summary_stats <- function(data, variable, var_name) {
  data |>
    summarise(
      Variable = var_name,
      Mean = round(mean({{variable}}, na.rm = TRUE), 2),
      Median = round(median({{variable}}, na.rm = TRUE), 2),
      StdDev = round(sd({{variable}}, na.rm = TRUE), 2),
      `10th_Pct` = round(quantile({{variable}}, 0.10, na.rm = TRUE), 2),
      `25th_Pct` = round(quantile({{variable}}, 0.25, na.rm = TRUE), 2),
      `75th_Pct` = round(quantile({{variable}}, 0.75, na.rm = TRUE), 2),
      `90th_Pct` = round(quantile({{variable}}, 0.90, na.rm = TRUE), 2),
      .groups = "drop"
    )
}

# Panel A: Call options
panel_a_stats <- full_data |>
  filter(cp_flag == "C") |>
  summarise(
    n_obs = n(),
    .groups = "drop"
  )

panel_a_table <- bind_rows(
  # Delta-hedged gains until maturity
  full_data |> filter(cp_flag == "C") |> 
    create_summary_stats(delta_hedged_gain_scaled, "Delta-hedged gain until maturity/(ΔS-C) (%)"),
  # Days to maturity
  full_data |> filter(cp_flag == "C") |> 
    create_summary_stats(days_to_maturity_initial, "Days to maturity"),
  # Moneyness
  full_data |> filter(cp_flag == "C") |> 
    create_summary_stats(moneyness_pct, "Moneyness = S/K (%)"),
  # Vegar) 
  full_data |> filter(cp_flag == "C") |> 
    create_summary_stats(vega_initial, "Vega")
)

# Panel B: Put options
panel_b_stats <- full_data |>
  filter(cp_flag == "P") |>
  summarise(
    n_obs = n(),
    .groups = "drop"
  )

panel_b_table <- bind_rows(
  # Delta-hedged gains until maturity
  full_data |> filter(cp_flag == "P") |> 
    create_summary_stats(delta_hedged_gain_scaled, "Delta-hedged gain until maturity/(P-ΔS) (%)"),
  # Days to maturity
  full_data |> filter(cp_flag == "P") |> 
    create_summary_stats(days_to_maturity_initial, "Days to maturity"),
  # Moneyness
  full_data |> filter(cp_flag == "P") |> 
    create_summary_stats(moneyness_pct, "Moneyness = S/K (%)"),
  # Initial delta (vega from paper)
  full_data |> filter(cp_flag == "P") |> 
    create_summary_stats(abs(vega_initial), "Vega")
)

# Panel D: Summary by volatility quintiles (for calls only, similar to Cao-Han)
# All data is already collected, so this works with the collected data
panel_d_table <- full_data |>
  filter(cp_flag == "C", !is.na(vol_quintile)) |>
  group_by(vol_quintile) |>
  summarise(
    n_obs = n(),
    Mean_Return = round(mean(delta_hedged_gain_scaled, na.rm = TRUE), 2),
    Median_Return = round(median(delta_hedged_gain_scaled, na.rm = TRUE), 2),
    StdDev_Return = round(sd(delta_hedged_gain_scaled, na.rm = TRUE), 2),
    Mean_Volatility = round(mean(stock_volatility, na.rm = TRUE), 4),
    Mean_Moneyness = round(mean(moneyness_pct, na.rm = TRUE), 2),
    Mean_Days_to_Maturity = round(mean(days_to_maturity_initial, na.rm = TRUE), 0),
    .groups = "drop"
  ) |>
  mutate(
    Quintile = paste0("Q", vol_quintile)
  ) |>
  select(Quintile, n_obs, Mean_Return, Median_Return, StdDev_Return, 
         Mean_Volatility, Mean_Moneyness, Mean_Days_to_Maturity)

# Display results
cat("=== TABLE 1 REPLICATION ===\n\n")

cat("Panel A: Call Options Summary Statistics (N =", panel_a_stats$n_obs, ")\n")
print(panel_a_table)

cat("\n\nPanel B: Put Options Summary Statistics (N =", panel_b_stats$n_obs, ")\n")
print(panel_b_table)

cat("\n\nPanel D: Call Options by Stock Volatility Quintiles\n")
print(panel_d_table)

# Additional analysis: Test for statistical significance
# T-test for mean returns being different from zero
call_ttest <- t.test(full_data$delta_hedged_gain_scaled[full_data$cp_flag == "C"])
put_ttest <- t.test(full_data$delta_hedged_gain_scaled[full_data$cp_flag == "P"])

cat("\n\n=== STATISTICAL TESTS ===\n")
cat("Call Options - Mean Delta-Hedged Return:\n")
cat("Mean:", round(call_ttest$estimate, 2), "%, t-stat:", round(call_ttest$statistic, 2), 
    ", p-value:", format(call_ttest$p.value, scientific = TRUE), "\n")

cat("\nPut Options - Mean Delta-Hedged Return:\n")
cat("Mean:", round(put_ttest$estimate, 2), "%, t-stat:", round(put_ttest$statistic, 2), 
    ", p-value:", format(put_ttest$p.value, scientific = TRUE), "\n")

# Test for volatility effect (linear regression of returns on volatility quintiles)
vol_model <- lm(delta_hedged_gain_scaled ~ vol_quintile, 
                data = full_data[full_data$cp_flag == "C" & !is.na(full_data$vol_quintile), ])
cat("\nVolatility Effect (Calls only):\n")
cat("Coefficient on Volatility Quintile:", round(coef(vol_model)[2], 2), 
    ", t-stat:", round(summary(vol_model)$coefficients[2, 3], 2),
    ", p-value:", format(summary(vol_model)$coefficients[2, 4], scientific = TRUE), "\n")

# Save results for further analysis
write.csv(panel_a_table, "panel_a_call_summary.csv", row.names = FALSE)
write.csv(panel_b_table, "panel_b_put_summary.csv", row.names = FALSE)
write.csv(panel_d_table, "panel_d_volatility_quintiles.csv", row.names = FALSE)
write.csv(full_data, "full_delta_hedged_data.csv", row.names = FALSE)

cat("\n\nResults saved to CSV files:\n")
cat("- panel_a_call_summary.csv\n")
cat("- panel_b_put_summary.csv\n") 
cat("- panel_d_volatility_quintiles.csv\n")
cat("- full_delta_hedged_data.csv\n")

# Close database connection
dbDisconnect(con)
