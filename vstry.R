library(arrow)
library(tidyverse)
library(fs)
library(duckdb)

#Setting up the data path
setwd("/Users/kainguyen/Desktop/Paper_replication")

con <- dbConnect(duckdb(), dbdir = "delta_hedged_returns.duckdb", read_only = FALSE)
dbDisconnect(con)
dbListTables(con)
option_price   <- tbl(con, "option_price")
security_price <- tbl(con, "security_price")
distribution_file <- tbl(con, "distribution_file")
ff_data <- tbl(con, "ff_rates")
#DISTRIBUTION_DATA=================================
distribution_file_path <- "/Users/kainguyen/Desktop/Paper_replication/Distribution_File_2023-03-23.parquet"
distribution_file <- read_parquet(distribution_file_path)
distribution_data <- distribution_file |>
  filter(
    ex_date >= as.Date("1996-01-01"),
    ex_date <= as.Date("2009-11-01"),
    cancel_flag == 0,  # Not cancelled
    distr_type %in% c(1, 4, 5),  #Regular, capital gain, or special dividends
    amount > 0  #Positive dividend amount
  ) |>
  select(secid, ex_date, amount, distr_type)

#SECURITY_DATA======================================

security_price <- security_price |>
  arrange(secid, date) |>
  select(secid, date, close, return, cfadj, cfret) |>
  mutate(
    close = abs(close),
    last_cfadj = last(cfadj),
    last_cfret = last(cfret),
    
    adj_close = (close * cfadj) / last_cfadj,
    adj_close2 = (close * cfret) / last_cfret,
    
    .by = secid
  )
head(option_price)

#RISK-FREE_DATA======================================

# FF risk-free data

# Create a temporary file for the ZIP
temp_zip <- tempfile(fileext = ".zip")

# Download the ZIP file from the URL
download.file(
  url = "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors_daily_CSV.zip",
  destfile = temp_zip,
  mode = "wb" # Use binary mode for ZIP files
)

# Unzip the file to a temporary directory
temp_dir <- tempdir()
csv_file <- unzip(temp_zip, exdir = temp_dir)

# Read the CSV file using vroom
ff_data <- read_csv(csv_file, col_types = cols(), skip = 3) |>
  janitor::clean_names()
ff_data <- ff_data |>
  mutate(date = ymd(x1)) |>
  select(!x1) |>
  relocate(date) |> 
  mutate(
    rf_daily = rf/100,
    rf_annual = rf_daily * 365
  ) |> 
  select(-rf)

# Clean up temporary files (optional)
unlink(temp_zip)
unlink(csv_file)

#MONTH-END DATES======================================
month_end_dates <- security_price |>
  filter(
    date >= as.Date("1996-01-01"),
    date <= as.Date("2009-11-01")
  ) |>
  select(date) |>
  distinct() |>
  mutate(
    year_month = format(date, "%Y-%m")
  ) |>
  group_by(year_month) |>
  summarise(
    month_end_date = max(date),
    .groups = "drop"
  ) |>
  pull(month_end_date)

colnames(option_price)
#OPTION_FILTERING======================================
atm_options_selected <- option_price |>
  filter(
    #Only month-end dates
    date %in% month_end_dates,
    
    #Sample period
    date >= as.Date("1996-01-01"),
    date <= as.Date("2009-11-01"),
    
    #Basic filters
    volume > 0,
    best_bid > 0,
    best_offer > best_bid,
    (best_bid + best_offer) / 2 >= 0.125,
    am_settlement == 0,
    !is.na(delta)
  ) |> 
  mutate(
    mid_quote = (best_bid + best_offer) / 2,
    strike_price = strike_price / 1000,  # Convert to dollars
    days_to_maturity = as.numeric(exdate - date)
  ) |>
  filter(days_to_maturity > 30) |>
  left_join(
    security_price,
    by = c("secid", "date")
  ) |>
  mutate(
    moneyness = close / strike_price
  ) |>
  #For each stock, date, and option type
  group_by(secid, date, cp_flag) |>
  filter(days_to_maturity == min(days_to_maturity)) |>
  #find closest to ATM (strike closest to stock price)
  mutate(atm_distance = abs(strike_price - close)) |>
  slice_min(atm_distance, n = 1, with_ties = FALSE) |>
  ungroup()

#CHECK FOR DIVIDENDS======================================
dividend_check <- atm_options_selected |>
  select(secid, date, cp_flag, strike_price, exdate) |>
  left_join(
    distribution_data,
    by = "secid",
    relationship = "many-to-many"
  ) |>
  mutate(
    has_dividend = case_when(
      is.na(ex_date) ~ FALSE,
      ex_date > date & ex_date <= exdate ~ TRUE,
      TRUE ~ FALSE
    )
  ) |>
  group_by(secid, date, cp_flag, strike_price, exdate) |>
  summarize(
    has_dividend_during_life = any(has_dividend),
    .groups = "drop"
  )

atm_options_filtered <- atm_options_selected |>
  left_join(
    dividend_check,
    by = c("secid", "date", "cp_flag", "strike_price", "exdate")
  ) |>
  mutate(
    has_dividend_during_life = coalesce(has_dividend_during_life, FALSE)
  ) |>
  # Exclude call options with dividends during life
  filter(!(cp_flag == "C" & has_dividend_during_life == TRUE))

#NO-ARBITRAGE CONDITIONS======================================
atm_options_filtered <- atm_options_filtered |>
  left_join(
    ff_data |> select(date, rf_daily, rf_annual),
    by = "date"
  ) |>
  mutate(
    # Time to maturity in years
    time_to_maturity_years = days_to_maturity / 365,
    
    # No-arbitrage bounds
    # Call: max(0, S - K*exp(-r*T)) <= C <= S
    call_lower_bound = pmax(0, close - strike_price * exp(-rf_annual * time_to_maturity_years)),
    call_upper_bound = close,
    
    # Put: max(0, K*exp(-r*T) - S) <= P <= K*exp(-r*T)
    put_lower_bound = pmax(0, strike_price * exp(-rf_annual * time_to_maturity_years) - close),
    put_upper_bound = strike_price * exp(-rf_annual * time_to_maturity_years),
    
    # Check violations
    violates_no_arbitrage = case_when(
      cp_flag == "C" & (mid_quote < call_lower_bound | mid_quote > call_upper_bound) ~ TRUE,
      cp_flag == "P" & (mid_quote < put_lower_bound | mid_quote > put_upper_bound) ~ TRUE,
      TRUE ~ FALSE
    )
  ) |>
  filter(!violates_no_arbitrage) |>
  select(-contains("bound"), -violates_no_arbitrage)

final_selected_options <- atm_options_filtered |> 
  mutate(
    selection_month = format(date, '%Y-%m'),
    selection_date = date  # Rename to avoid confusion in joins
  ) |>
  select(-c(contract_size, forward_price, expiry_indicator, adj_close, adj_close2, has_dividend_during_life))

total_unique_stocks <- final_selected_options |>
  pull(secid) |>
  n_distinct()

daily_option_data <- option_price |>
  filter(
    date >= as.Date("1996-01-01"),
    date <= as.Date("2009-10-31"),
    !is.na(delta),
    best_bid > 0,
    best_offer > best_bid,
    (best_bid + best_offer) / 2 >= 0.125
  ) |>
  mutate(
    strike_price = strike_price / 1000,
    mid_quote = (best_bid + best_offer) / 2
  ) |>
  select(
    secid, date, strike_price, cp_flag, exdate,
    mid_quote, delta, gamma, best_bid, best_offer, volume
  )

dbWriteTable(con, "final_selected_options_temp", final_selected_options, overwrite = TRUE)
final_selected_options_db <- tbl(con, "final_selected_options_temp")

tracking_data <- final_selected_options_db |>
  select(secid, cp_flag, strike_price, exdate, 
         selection_date, selection_month) |>
  inner_join(
    daily_option_data,
    by = c("secid", "cp_flag", "strike_price", "exdate"),
    relationship = "many-to-many"
  ) |>
  filter(
    date >= selection_date,
    date <= selection_date + months(1)  # Track for one month
  )

dbWriteTable(con, "ff_data_temp", ff_data, overwrite = TRUE)
ff_data_db <- tbl(con, "ff_data_temp") |>
  select(date, rf_daily, rf_annual) |>
  rename(trade_date = date)

tracking_data <- tracking_data |>
  left_join(
    security_price,
    by = c("secid", "date")
  ) |>
  left_join(
    ff_data_db,
    by = "date"
  ) |>
  mutate(
    days_to_maturity = as.numeric(exdate - date)
  ) |>
  arrange(secid, cp_flag, selection_month, date) |>
  # Only collect after all joins and filters
  collect()

#Delta_hedging
daily_calculations <- tracking_data |>
  group_by(secid, cp_flag, selection_month) |>
  arrange(date) |>
  mutate(
    # Lag values for previous day
    prev_date = lag(date),
    prev_close = lag(close),
    prev_mid_quote = lag(mid_quote),
    prev_delta = lag(delta),
    prev_rf_annual = lag(rf_annual),
    
    # Days elapsed (for interest calculation)
    days_elapsed = as.numeric(date - prev_date),
    
    # Components of delta-hedged gain:
    # 1. Change in option value
    pnl_option = mid_quote - prev_mid_quote,
    
    # 2. Gain from stock hedge: -Δ[t-1] * (S[t] - S[t-1])
    pnl_stock_hedge = -prev_delta * (close - prev_close),
    
    # 3. Interest on net investment: -r * (days/365) * (C[t-1] - Δ[t-1] * S[t-1])
    pnl_interest = -(days_elapsed * prev_rf_annual / 365) * 
      (prev_mid_quote - prev_delta * prev_close)
  ) |>
  ungroup()

# AGGREGATE TO MONTHLY RETURNS======================================
# Group by stock (secid) rather than individual option series
monthly_returns <- daily_calculations |>
  group_by(secid, cp_flag, selection_month) |>
  summarise(
    # Initial values
    selection_date = first(date),
    strike_price = first(strike_price),
    expiration_date = first(exdate),
    S_initial = first(close),
    C_initial = first(mid_quote),
    delta_initial = first(delta),
    days_to_maturity_initial = first(days_to_maturity),
    
    # Final values
    final_date = last(date),
    S_final = last(close),
    C_final = last(mid_quote),
    
    # Number of trading days
    n_trading_days = n() - 1,  # Subtract 1 for initial observation
    
    # Total PnL components (excluding first NA row)
    total_pnl_option = sum(pnl_option, na.rm = TRUE),
    total_pnl_stock_hedge = sum(pnl_stock_hedge, na.rm = TRUE),
    total_pnl_interest = sum(pnl_interest, na.rm = TRUE),
    
    .groups = "drop"
  ) |>
  filter(
    n_trading_days >= 10,  # Meaningful holding period
    !is.na(C_initial),
    !is.na(C_final)
  )

# CALCULATE DELTA-HEDGED RETURNS======================================
delta_hedged_returns <- monthly_returns |>
  mutate(
    # Total gain
    total_gain = total_pnl_option + total_pnl_stock_hedge + total_pnl_interest,
    
    # Scaling factor: |Δ*S - C| for calls, |P - Δ*S| for puts
    scaling_factor = if_else(
      cp_flag == "C",
      abs(delta_initial * S_initial - C_initial),
      abs(C_initial - delta_initial * S_initial)
    ),
    
    # Delta-hedged return
    delta_hedged_return = total_gain / scaling_factor,
    
    # Additional metrics
    holding_period_days = as.numeric(final_date - selection_date),
    moneyness = S_initial / strike_price
  )

# SUMMARY STATISTICS - TABLE 1 REPLICATION======================================
# Panel A: Call options
panel_a_calls <- delta_hedged_returns |>
  filter(cp_flag == "C") |>
  summarise(
    n_obs = n(),
    
    # Delta-hedged gain statistics (as percentage)
    dh_return_mean = mean(delta_hedged_return) * 100,
    dh_return_median = median(delta_hedged_return) * 100,
    dh_return_sd = sd(delta_hedged_return) * 100,
    dh_return_p10 = quantile(delta_hedged_return, 0.10) * 100,
    dh_return_p25 = quantile(delta_hedged_return, 0.25) * 100,
    dh_return_p75 = quantile(delta_hedged_return, 0.75) * 100,
    dh_return_p90 = quantile(delta_hedged_return, 0.90) * 100,
    
    # Days to maturity
    maturity_mean = mean(days_to_maturity_initial),
    maturity_median = median(days_to_maturity_initial),
    maturity_sd = sd(days_to_maturity_initial),
    
    # Moneyness (as percentage)
    moneyness_mean = mean(moneyness) * 100,
    moneyness_median = median(moneyness) * 100,
    moneyness_sd = sd(moneyness) * 100
  )

# Panel B: Put options
panel_b_puts <- delta_hedged_returns |>
  filter(cp_flag == "P") |>
  summarise(
    n_obs = n(),
    
    # Delta-hedged gain statistics (as percentage)
    dh_return_mean = mean(delta_hedged_return) * 100,
    dh_return_median = median(delta_hedged_return) * 100,
    dh_return_sd = sd(delta_hedged_return) * 100,
    dh_return_p10 = quantile(delta_hedged_return, 0.10) * 100,
    dh_return_p25 = quantile(delta_hedged_return, 0.25) * 100,
    dh_return_p75 = quantile(delta_hedged_return, 0.75) * 100,
    dh_return_p90 = quantile(delta_hedged_return, 0.90) * 100,
    
    # Days to maturity
    maturity_mean = mean(days_to_maturity_initial),
    maturity_median = median(days_to_maturity_initial),
    maturity_sd = sd(days_to_maturity_initial),
    
    # Moneyness (as percentage)
    moneyness_mean = mean(moneyness) * 100,
    moneyness_median = median(moneyness) * 100,
    moneyness_sd = sd(moneyness) * 100
  )

# Print results
cat("\n=== TABLE 1 REPLICATION ===\n")
cat("\nPanel A: Call Options (", panel_a_calls$n_obs, " observations)\n", sep = "")
cat("Delta-hedged gain until month-end/(Δ×S−C) (%):\n")
cat("  Mean: ", sprintf("%.2f", panel_a_calls$dh_return_mean), "\n")
cat("  Median: ", sprintf("%.2f", panel_a_calls$dh_return_median), "\n")
cat("  Std Dev: ", sprintf("%.2f", panel_a_calls$dh_return_sd), "\n")
cat("  10th %ile: ", sprintf("%.2f", panel_a_calls$dh_return_p10), "\n")
cat("  90th %ile: ", sprintf("%.2f", panel_a_calls$dh_return_p90), "\n")
cat("\nDays to maturity: Mean =", round(panel_a_calls$maturity_mean), 
    ", Median =", round(panel_a_calls$maturity_median), "\n")
cat("Moneyness = S/K (%): Mean =", sprintf("%.2f", panel_a_calls$moneyness_mean),
    ", Median =", sprintf("%.2f", panel_a_calls$moneyness_median), "\n")

cat("\nPanel B: Put Options (", panel_b_puts$n_obs, " observations)\n", sep = "")
cat("Delta-hedged gain until month-end/(P−Δ×S) (%):\n")
cat("  Mean: ", sprintf("%.2f", panel_b_puts$dh_return_mean), "\n")
cat("  Median: ", sprintf("%.2f", panel_b_puts$dh_return_median), "\n")
cat("  Std Dev: ", sprintf("%.2f", panel_b_puts$dh_return_sd), "\n")
cat("  10th %ile: ", sprintf("%.2f", panel_b_puts$dh_return_p10), "\n")
cat("  90th %ile: ", sprintf("%.2f", panel_b_puts$dh_return_p90), "\n")
cat("\nDays to maturity: Mean =", round(panel_b_puts$maturity_mean),
    ", Median =", round(panel_b_puts$maturity_median), "\n")
cat("Moneyness = S/K (%): Mean =", sprintf("%.2f", panel_b_puts$moneyness_mean),
    ", Median =", sprintf("%.2f", panel_b_puts$moneyness_median), "\n")

# Panel D: Unique stocks
panel_d <- delta_hedged_returns |>
  group_by(cp_flag) |>
  summarise(
    total_unique_stocks = n_distinct(secid),
    .groups = "drop"
  )

cat("\nPanel D: Average delta-hedged gain by unique stocks\n")
cat("Call options - Total stocks: ", 
    panel_d |> filter(cp_flag == "C") |> pull(total_unique_stocks), "\n")
cat("Put options - Total stocks: ", 
    panel_d |> filter(cp_flag == "P") |> pull(total_unique_stocks), "\n")
