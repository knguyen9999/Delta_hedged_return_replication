library(arrow)
library(tidyverse)
library(fs)
library(duckdb)
source("delta_hedged_db_database_building.R")
library(janitor)


#Setting up the data path
setwd("/Users/kainguyen/Desktop/Paper_replication")

con <- dbConnect(
  duckdb(),
  dbdir = "delta_hedged_returns.duckdb",
  read_only = FALSE
)
dbListTables(con)

#Create variable names for available data in duckdb
option_price <- tbl(con, "option_price")
security_price <- tbl(con, "security_price")
distribution_file <- tbl(con, "distribution_file")
ff_data <- tbl(con, "ff_rates")

dbListTables(con)

# TABLE 1 REPLICATION - CAO & HAN (2013)
# "Cross-Section of Option Returns and Idiosyncratic Stock Volatility"

print("Starting Table 1 Replication...")

# STEP 1: PREPARE DISTRIBUTION DATA FOR DIVIDEND FILTERING
distribution_data <- distribution_file |>
  filter(
    ex_date >= as.Date("1996-01-01"),
    ex_date <= as.Date("2009-12-31"),
    cancel_flag == 0,  # Not cancelled
    liquidation_flag == 0,  # Non-liquidating distributions only
    distr_type %in% c(1, 4, 5, "%"),  # Regular dividend, capital gain, special dividend, and projected dividends
    amount > 0  # Positive dividend amount
  ) |>
  select(secid, ex_date, amount, distr_type)

# STEP 2: BASIC OPTION FILTERING
atm_options_selected <- option_price |>
  filter(
    date >= as.Date("1996-01-01"),
    date <= as.Date("2009-12-31"),
    volume > 0,
    best_bid > 0,
    best_offer > best_bid,
    (best_bid + best_offer) / 2 >= 0.125,
    am_settlement == 0,  # European style only
    !is.na(delta),
    !is.na(gamma),
    !is.na(vega),
    !is.na(theta)
  ) |>
  mutate(
    mid_quote = (best_bid + best_offer) / 2,
    strike_price = strike_price / 1000,  # Convert to dollars
    days_to_maturity = as.numeric(exdate - date)
  ) |>
  filter(days_to_maturity > 30)

# STEP 3: JOIN WITH SECURITY PRICES
options_with_prices <- atm_options_selected |>
  left_join(
    security_price |> select(secid, date, close),
    by = c("secid", "date")
  ) |>
  filter(!is.na(close)) |>
  mutate(close = abs(close))

# STEP 4: JOIN WITH RISK-FREE RATES
options_with_rf <- options_with_prices |>
  left_join(
    ff_data |> select(date, rf_daily),
    by = "date"
  ) |>
  filter(!is.na(rf_daily))

# STEP 5: NO-ARBITRAGE CONDITIONS
options_no_arb <- options_with_rf |>
  mutate(
    # Call bounds: max(0, S - K*exp(-r*T)) <= C <= S
    call_lower_bound = pmax(0, close - strike_price * exp(-rf_daily * days_to_maturity / 365)),
    call_upper_bound = close,
    # Put bounds: max(0, K*exp(-r*T) - S) <= P <= K*exp(-r*T)
    put_lower_bound = pmax(0, strike_price * exp(-rf_daily * days_to_maturity / 365) - close),
    put_upper_bound = strike_price * exp(-rf_daily * days_to_maturity / 365),
    # Check arbitrage conditions
    no_arb_cond = case_when(
      cp_flag == "C" ~ (mid_quote >= call_lower_bound) & (mid_quote <= call_upper_bound),
      cp_flag == "P" ~ (mid_quote >= put_lower_bound) & (mid_quote <= put_upper_bound),
      TRUE ~ FALSE
    )
  ) |>
  filter(no_arb_cond == TRUE) |>
  select(-call_lower_bound, -call_upper_bound, -put_lower_bound, -put_upper_bound, -no_arb_cond)

# STEP 6: MONEYNESS AND ATM SELECTION
options_with_moneyness <- options_no_arb |>
  mutate(
    moneyness = close / strike_price,
    atm_distance = abs(moneyness - 1)
  )

# Select most ATM option for each stock-date-option type
selected_atm_options <- options_with_moneyness |>
  group_by(secid, date, cp_flag) |>
  slice_min(atm_distance, n = 1, with_ties = FALSE) |>
  slice_min(days_to_maturity, n = 1, with_ties = FALSE) |>
  ungroup()

# STEP 7: DIVIDEND FILTERING
# Identify options with dividends during their life
options_with_dividends <- selected_atm_options |>
  inner_join(
    distribution_data |> select(secid, ex_date),
    by = "secid",
    relationship = "many-to-many"
  ) |>
  filter(
    ex_date > date,
    ex_date <= exdate
  ) |>
  select(optionid) |>
  distinct()

# Remove options with dividends
selected_options_final <- selected_atm_options |>
  anti_join(options_with_dividends, by = "optionid")

print(paste("Final sample size:", selected_options_final |> count() |> pull(n)))

# STEP 8: CALCULATE DELTA-HEDGED RETURNS
# Get all daily prices for selected options
daily_option_data <- option_price |>
  semi_join(selected_options_final, by = "optionid") |>
  select(optionid, date, best_bid, best_offer, delta, gamma, vega, theta) |>
  mutate(mid_quote = (best_bid + best_offer) / 2)

daily_security_data <- security_price |>
  semi_join(selected_options_final, by = "secid") |>
  select(secid, date, close) |>
  mutate(close = abs(close))

# Create option metadata
option_metadata <- selected_options_final |>
  select(optionid, secid, cp_flag, date, exdate, strike_price, moneyness, days_to_maturity) |>
  rename(start_date = date)

# Comprehensive join for delta-hedged calculations
delta_hedged_data <- option_metadata |>
  left_join(daily_option_data, by = "optionid", relationship = "many-to-many") |>
  left_join(daily_security_data, by = c("secid", "date")) |>
  left_join(ff_data |> select(date, rf_daily), by = "date") |>
  filter(
    date >= start_date,
    date <= exdate,
    !is.na(mid_quote),
    !is.na(close),
    !is.na(delta),
    !is.na(rf_daily)
  ) |>
  arrange(optionid, date)

# Calculate daily delta-hedged returns
delta_hedged_returns <- delta_hedged_data |>
  group_by(optionid) |>
  mutate(
    # Lagged values
    lag_close = lag(close),
    lag_mid_quote = lag(mid_quote),
    lag_delta = lag(delta),
    lag_rf = lag(rf_daily),
    
    # Changes
    stock_change = close - lag_close,
    option_change = mid_quote - lag_mid_quote,
    
    # Interest component
    interest = lag_rf * (lag_mid_quote - lag_delta * lag_close),
    
    # Daily delta-hedged P&L
    daily_pnl = case_when(
      is.na(stock_change) ~ 0,  # First observation
      TRUE ~ option_change - lag_delta * stock_change - interest
    ),
    
    # Cumulative P&L
    cumulative_pnl = cumsum(daily_pnl),
    
    # Initial investment (scaling factor)
    initial_investment = first(abs(delta * close - mid_quote)),
    
    # Delta-hedged return
    delta_hedged_return = cumulative_pnl / initial_investment
  ) |>
  ungroup()

# STEP 9: SUMMARY STATISTICS FOR TABLE 1
option_summary <- delta_hedged_returns |>
  group_by(optionid) |>
  summarise(
    secid = first(secid),
    cp_flag = first(cp_flag),
    start_date = first(start_date),
    exdate = first(exdate),
    days_to_maturity = first(days_to_maturity),
    moneyness = first(moneyness),
    initial_vega = first(vega),
    final_return = last(delta_hedged_return),
    final_return_pct = final_return * 100,
    .groups = "drop"
  ) |>
  filter(!is.na(final_return) & is.finite(final_return))

# Function to create summary statistics
create_summary_stats <- function(data, variable, var_name) {
  data |>
    summarise(
      Variable = var_name,
      Mean = round(mean({{variable}}, na.rm = TRUE), 3),
      Median = round(median({{variable}}, na.rm = TRUE), 3),
      StdDev = round(sd({{variable}}, na.rm = TRUE), 3),
      `10th_Pct` = round(quantile({{variable}}, 0.10, na.rm = TRUE), 3),
      `25th_Pct` = round(quantile({{variable}}, 0.25, na.rm = TRUE), 3),
      `75th_Pct` = round(quantile({{variable}}, 0.75, na.rm = TRUE), 3),
      `90th_Pct` = round(quantile({{variable}}, 0.90, na.rm = TRUE), 3),
      N = n(),
      .groups = "drop"
    )
}

# PANEL A: CALL OPTIONS
panel_a_data <- option_summary |> filter(cp_flag == "C")
panel_a_table <- bind_rows(
  panel_a_data |> create_summary_stats(final_return_pct, "Delta-hedged gain until maturity (%)"),
  panel_a_data |> create_summary_stats(days_to_maturity, "Days to maturity"),
  panel_a_data |> create_summary_stats(moneyness * 100, "Moneyness = S/K (%)"),
  panel_a_data |> create_summary_stats(initial_vega, "Vega")
)

# PANEL B: PUT OPTIONS  
panel_b_data <- option_summary |> filter(cp_flag == "P")
panel_b_table <- bind_rows(
  panel_b_data |> create_summary_stats(final_return_pct, "Delta-hedged gain until maturity (%)"),
  panel_b_data |> create_summary_stats(days_to_maturity, "Days to maturity"),
  panel_b_data |> create_summary_stats(moneyness * 100, "Moneyness = S/K (%)"),
  panel_b_data |> create_summary_stats(abs(initial_vega), "Vega")
)

print("=== PANEL A: CALL OPTIONS ===")
print(panel_a_table)
cat("\n")

print("=== PANEL B: PUT OPTIONS ===")
print(panel_b_table)
cat("\n")

# PANEL D: VOLATILITY QUINTILES (CALLS ONLY)
# Calculate historical volatility for each stock
stock_volatility <- security_price |>
  filter(date >= as.Date("1995-01-01"), date <= as.Date("2009-12-31")) |>
  select(secid, date, close) |>
  mutate(close = abs(close)) |>
  arrange(secid, date) |>
  group_by(secid) |>
  mutate(
    return = log(close / lag(close)),
    return_sq = return^2
  ) |>
  filter(!is.na(return)) |>
  summarise(
    volatility = sqrt(252 * mean(return_sq, na.rm = TRUE)),
    .groups = "drop"
  )

# Merge with call options and create quintiles
panel_d_data <- panel_a_data |>
  left_join(stock_volatility, by = "secid") |>
  filter(!is.na(volatility)) |>
  mutate(
    vol_quintile = ntile(volatility, 5)
  )

# Create Panel D summary
panel_d_table <- panel_d_data |>
  group_by(vol_quintile) |>
  summarise(
    N = n(),
    Mean_Return = round(mean(final_return_pct, na.rm = TRUE), 3),
    Median_Return = round(median(final_return_pct, na.rm = TRUE), 3),
    StdDev_Return = round(sd(final_return_pct, na.rm = TRUE), 3),
    Mean_Volatility = round(mean(volatility, na.rm = TRUE), 3),
    Mean_Moneyness = round(mean(moneyness * 100, na.rm = TRUE), 1),
    Mean_Days_to_Mat = round(mean(days_to_maturity, na.rm = TRUE), 1),
    .groups = "drop"
  )

print("=== PANEL D: VOLATILITY QUINTILES (CALLS) ===")
print(panel_d_table)

# Save results
write_csv(panel_a_table, "panel_a_call_summary.csv")
write_csv(panel_b_table, "panel_b_put_summary.csv") 
write_csv(panel_d_table, "panel_d_volatility_quintiles.csv")
write_csv(option_summary, "option_summary_full.csv")

cat("\nTable 1 replication completed. Files saved:\n")
cat("- panel_a_call_summary.csv\n")
cat("- panel_b_put_summary.csv\n") 
cat("- panel_d_volatility_quintiles.csv\n")
cat("- option_summary_full.csv\n")



