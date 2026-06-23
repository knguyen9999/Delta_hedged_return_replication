library(dplyr)
library(dbplyr)
library(duckdb)
con <- dbConnect(duckdb(), "delta_hedged_returns.duckdb", read_only = FALSE)
option_price_tbl <- tbl(con, "option_price")
security_price_tbl <- tbl(con, "security_price") 
ff_data_tbl <- tbl(con, "ff_rates")

#SPX secid
target_secids <- 108105  

#Build dataset
dhret_data <- option_price_tbl |> 
  filter(secid == target_secids) |> 
  #underlying prices
  left_join(
    security_price_tbl |>
      select(secid, date, close, return),
    by = c("secid", "date")
  ) |>
  #risk-free rates  
  left_join(
    ff_data_tbl |> select(date, rf_daily),
    by = "date"
  ) |>
  # Basic variables
  mutate(
    iscall = if_else(cp_flag == "C", 1L, 0L),
    ndays = as.integer(exdate - date),
    strike = strike_price / 1000,
    mid = (best_bid + best_offer) / 2,
    rel_spread = (best_offer - best_bid) / pmax(mid, 1e-12),
    # Create unique option identifier
    optionid = paste(secid, iscall, strike, exdate, sep = "_")
  ) |>
  #paper's basic filter
  filter(
    mid >= 0.10,                           # Minimum midpoint
    rel_spread <= 0.50,                    # Max 50% relative spread
    open_interest > 0,                     # Positive open interest
    !is.na(impl_volatility),               # IV present
    abs(delta) >= 0.01, abs(delta) <= 0.99, # Delta bounds
    best_offer > best_bid,                 # Valid quotes
    ndays >= 14, ndays <= 60,              # Maturity range
    !is.na(close), close > 0,              # Valid underlying price
    !is.na(rf_daily)                       # Valid risk-free rate
  ) |>
  collect()


lag_dhret_data <- dhret_data |> 
  # Add lags using optionid grouping
  group_by(optionid) |>
  arrange(date) |>
  mutate(
    # t-1 prices (NO fallback for prices)
    lag1_mid = lag(mid, 1),
    lag1_close = lag(close, 1),
    
    # Delta with fallback to t-2 (ONLY for delta)
    lag1_delta = lag(delta, 1),
    lag2_delta = lag(delta, 2),
    delta_final = coalesce(lag1_delta, lag2_delta),
    
    # Track which delta lag was used
    delta_lag_used = case_when(
      !is.na(lag1_delta) ~ 1L,
      !is.na(lag2_delta) ~ 2L,
      TRUE ~ NA_integer_
    )
  ) |>
  ungroup()

dh_calc <- lag_dhret_data |> 
  # Compute delta-hedged returns
  mutate(
    #Risk-free rate
    gross_rf = 1 + rf_daily,
    
    #Simple excess returns (NOT log returns)
    xret_opt = (mid / lag1_mid) - gross_rf,           # Option excess return
    xret_stock = (close / lag1_close) - gross_rf,     # Stock excess return
    
    #Correct hedge ratio: shares of stock per option
    beta_hedge = (lag1_close / lag1_mid) * delta_final,
    
    # `ELTA-HEDGED RETURN
    dhxret_avg = xret_opt - beta_hedge * xret_stock
  ) |>
  #Final filters for valid calculations
  filter(
    !is.na(dhxret_avg),
    !is.na(lag1_mid), lag1_mid > 0,        # t-1 option price available
    !is.na(lag1_close), lag1_close > 0,    # t-1 stock price available  
    !is.na(delta_final),                   # Delta (t-1 or t-2) available
    !is.na(delta_lag_used)                 # Valid delta lag identifier
  ) |>
  select(
    secid, date, optionid, iscall, ndays, strike, exdate,
    mid, lag1_mid, close, lag1_close,
    delta, lag1_delta, lag2_delta, delta_final, delta_lag_used,
    impl_volatility, open_interest, rel_spread,
    rf_daily, gross_rf,
    xret_opt, xret_stock, beta_hedge, dhxret_avg
  )

dh_calc |> head() |> collect() |> View()

#Results
message("Total observations: ", nrow(dhret_data))

#Show delta lag usage
delta_usage <- dh_calc |>
  group_by(iscall, delta_lag_used) |>
  summarise(n_obs = n(), .groups = "drop") |>
  mutate(
    option_type = if_else(iscall == 1, "Calls", "Puts"),
    delta_description = case_when(
      delta_lag_used == 1 ~ "Used lag1_delta",
      delta_lag_used == 2 ~ "Used lag2_delta (fallback)",
      TRUE ~ "Unknown"
    )
  )

print("Delta lag usage summary:")
print(delta_usage)

#Sample results
print("Sample delta-hedged returns:")
dh_calc |>
  select(date, iscall, ndays, strike, dhxret_avg, beta_hedge, delta_lag_used) |>
  slice_head(n = 10) |>
  print()

#Summary statistics by option type
dhret_summary <- dh_calc |>
  group_by(iscall) |>
  summarise(
    n_obs = n(),
    n_delta1_used = sum(delta_lag_used == 1, na.rm = TRUE),
    n_delta2_used = sum(delta_lag_used == 2, na.rm = TRUE),
    pct_delta2_fallback = round(100 * n_delta2_used / n_obs, 1),
    mean_dhret = mean(dhxret_avg, na.rm = TRUE),
    sd_dhret = sd(dhxret_avg, na.rm = TRUE),
    median_dhret = median(dhxret_avg, na.rm = TRUE),
    min_dhret = min(dhxret_avg, na.rm = TRUE),
    max_dhret = max(dhxret_avg, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(option_type = if_else(iscall == 1, "Calls", "Puts"))

print("Delta-hedged return summary:")
print(dhret_summary)

dhret_summary |> collect() |> View()

dbDisconnect(con, shutdown = TRUE)



