#Diarte, Jones, and Wang

library(dplyr)
library(dbplyr)
library(duckdb)
con <- dbConnect(duckdb(), "delta_hedged_returns.duckdb", read_only = FALSE)

# Load the master S&P 500 data
master_data_sp500 <- tbl(con, "master_data_sp500")
ff_data <- tbl(con, "ff_rates")

# S&P 500 secid
sp500_secid <- 108105

print("Processing S&P 500 Index Options data - Noisy Prices Methodology...")

# OPTION FILTERING AND PREPARATION ======================================
dhret_data <- master_data_sp500 |> 
  # Basic variables
  mutate(
    iscall = if_else(cp_flag == "C", 1L, 0L),
    ndays = days_to_expiry,
    rel_spread = spread / pmax(mid_price, 1e-12),
    # Create unique option identifier
    optionid_unique = paste(secid, iscall, strike, exdate, sep = "_")
  ) |>
  # Apply baseline filters BEFORE collecting (matching paper's criteria)
  filter(
    mid_price >= 0.10,                           # Minimum midpoint
    rel_spread <= 0.50,                          # Max 50% relative spread
    open_interest > 0,                           # Positive open interest
    !is.na(impl_volatility),                     # IV present
    abs(delta) >= 0.01, abs(delta) <= 0.99,      # Delta bounds
    best_offer > best_bid,                       # Valid quotes
    ndays >= 14, ndays <= 60,                    # Maturity range (paper's focus)
    !is.na(stock_price), stock_price > 0,        # Valid underlying price
    !is.na(rf_daily)                             # Valid risk-free rate
  ) |>
  # Collect for lag operations
  collect() |>
  # Add lags using optionid grouping
  group_by(optionid_unique) |>
  arrange(date) |>
  mutate(
    # t-1 prices (NO fallback for prices - key difference from other methods)
    lag1_mid = lag(mid_price, 1),
    lag1_stock = lag(stock_price, 1),
    
    # Delta with fallback to t-2 (ONLY for delta - paper's specific approach)
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
  ungroup() |>
  # Compute delta-hedged returns (paper's methodology)
  mutate(
    # Risk-free rate (assuming rf_daily is already daily rate)
    gross_rf = 1 + rf_daily,
    
    # Simple excess returns (NOT log returns - paper uses simple returns)
    xret_opt = (mid_price / lag1_mid) - gross_rf,      # Option excess return
    xret_stock = (stock_price / lag1_stock) - gross_rf, # Stock excess return
    
    # Correct hedge ratio: shares of stock per option (paper's formulation)
    beta_hedge = (lag1_stock / lag1_mid) * delta_final,
    
    # DELTA-HEDGED RETURN (paper's calculation)
    dhxret_avg = xret_opt - beta_hedge * xret_stock
  ) |>
  # Final filters for valid calculations
  filter(
    !is.na(dhxret_avg),
    !is.na(lag1_mid), lag1_mid > 0,        # t-1 option price available
    !is.na(lag1_stock), lag1_stock > 0,    # t-1 stock price available  
    !is.na(delta_final),                   # Delta (t-1 or t-2) available
    !is.na(delta_lag_used)                 # Valid delta lag identifier
  )

print(paste("Total observations after filtering:", nrow(dhret_data)))

# ==============================================================================
# ANALYSIS AND RESULTS
# ==============================================================================

# Show delta lag usage
delta_usage <- dhret_data |>
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

cat("\n=== NOISY PRICES DELTA-HEDGED RETURNS - S&P 500 INDEX OPTIONS ===\n\n")
cat("Delta lag usage summary:\n")
print(delta_usage)

# Summary statistics by option type
dhret_summary <- dhret_data |>
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
    pct_positive = mean(dhxret_avg > 0, na.rm = TRUE) * 100,
    .groups = "drop"
  ) |>
  mutate(option_type = if_else(iscall == 1, "Calls", "Puts"))

cat("\nDelta-hedged return summary:\n")
cat("-----------------------------\n")
print(dhret_summary)

# Get statistics for calls and puts separately
call_stats <- dhret_summary |> filter(iscall == 1)
put_stats <- dhret_summary |> filter(iscall == 0)

# Check if we have data for both
if(nrow(call_stats) == 0) {
  call_stats <- tibble(
    n_obs = 0, mean_dhret = NA_real_, median_dhret = NA_real_,
    sd_dhret = NA_real_, pct_positive = NA_real_, pct_delta2_fallback = NA_real_
  )
}
if(nrow(put_stats) == 0) {
  put_stats <- tibble(
    n_obs = 0, mean_dhret = NA_real_, median_dhret = NA_real_,
    sd_dhret = NA_real_, pct_positive = NA_real_, pct_delta2_fallback = NA_real_
  )
}

# Statistical tests
call_returns <- dhret_data |> filter(iscall == 1) |> pull(dhxret_avg)
put_returns <- dhret_data |> filter(iscall == 0) |> pull(dhxret_avg)

call_ttest <- t.test(call_returns)
put_ttest <- t.test(put_returns)

cat("\n=== STATISTICAL TESTS ===\n")
cat("Call Options - Mean Delta-Hedged Return:\n")
cat("Mean:", round(call_stats$mean_dhret * 100, 4), "%\n")
cat("t-stat:", round(call_ttest$statistic, 2), ", p-value:", format(call_ttest$p.value, scientific = TRUE), "\n")

cat("\nPut Options - Mean Delta-Hedged Return:\n")
cat("Mean:", round(put_stats$mean_dhret * 100, 4), "%\n")
cat("t-stat:", round(put_ttest$statistic, 2), ", p-value:", format(put_ttest$p.value, scientific = TRUE), "\n")

# Additional analysis: By maturity buckets (paper's analysis)
maturity_analysis <- dhret_data |>
  mutate(
    maturity_bucket = case_when(
      ndays <= 30 ~ "14-30 days",
      ndays <= 45 ~ "31-45 days",
      TRUE ~ "46-60 days"
    )
  ) |>
  group_by(iscall, maturity_bucket) |>
  summarise(
    n_obs = n(),
    mean_return = mean(dhxret_avg, na.rm = TRUE) * 100,
    std_return = sd(dhxret_avg, na.rm = TRUE) * 100,
    .groups = "drop"
  ) |>
  mutate(option_type = if_else(iscall == 1, "Calls", "Puts"))

cat("\n=== RETURNS BY MATURITY ===\n")
print(maturity_analysis)

# Analysis by moneyness (paper's approach)
moneyness_analysis <- dhret_data |>
  mutate(
    moneyness_bucket = case_when(
      moneyness < 0.95 ~ "OTM (< 0.95)",
      moneyness < 1.00 ~ "OTM (0.95-1.00)",
      moneyness < 1.05 ~ "ITM (1.00-1.05)",
      TRUE ~ "ITM (> 1.05)"
    )
  ) |>
  group_by(iscall, moneyness_bucket) |>
  summarise(
    n_obs = n(),
    mean_return = mean(dhxret_avg, na.rm = TRUE) * 100,
    std_return = sd(dhxret_avg, na.rm = TRUE) * 100,
    .groups = "drop"
  ) |>
  mutate(option_type = if_else(iscall == 1, "Calls", "Puts"))

cat("\n=== RETURNS BY MONEYNESS ===\n")
print(moneyness_analysis)

#==============================================================================#
# CREATE STANDARDIZED METRICS FOR COMPARISON
#==============================================================================#

# Create standardized comparison metrics
comparison_metrics <- list(
  method = "Duarte-Jones-Wang, Delta Fallback Method",
  data = "S&P 500 Index Options",
  
  # Sample size
  n_call_options = call_stats$n_obs,
  n_put_options = put_stats$n_obs,
  
  # Returns - Calls (convert to percentage)
  call_mean_return = ifelse(nrow(call_stats) > 0, call_stats$mean_dhret * 100, NA_real_),
  call_median_return = ifelse(nrow(call_stats) > 0, call_stats$median_dhret * 100, NA_real_),
  call_std_return = ifelse(nrow(call_stats) > 0, call_stats$sd_dhret * 100, NA_real_),
  call_pct_positive = ifelse(nrow(call_stats) > 0, call_stats$pct_positive, NA_real_),
  
  # Returns - Puts (convert to percentage)
  put_mean_return = ifelse(nrow(put_stats) > 0, put_stats$mean_dhret * 100, NA_real_),
  put_median_return = ifelse(nrow(put_stats) > 0, put_stats$median_dhret * 100, NA_real_),
  put_std_return = ifelse(nrow(put_stats) > 0, put_stats$sd_dhret * 100, NA_real_),
  put_pct_positive = ifelse(nrow(put_stats) > 0, put_stats$pct_positive, NA_real_),
  
  # Statistical significance
  call_t_stat = call_ttest$statistic[[1]],
  call_p_value = call_ttest$p.value,
  put_t_stat = put_ttest$statistic[[1]],
  put_p_value = put_ttest$p.value,
  
  # Additional metrics specific to this method
  call_pct_delta2_used = ifelse(nrow(call_stats) > 0, call_stats$pct_delta2_fallback, NA_real_),
  put_pct_delta2_used = ifelse(nrow(put_stats) > 0, put_stats$pct_delta2_fallback, NA_real_)
)

cat("\n=== STANDARDIZED METRICS FOR COMPARISON ===\n")
cat("Method: Noisy Prices (Delta Fallback)\n")
cat("Calls - Mean Return:", round(comparison_metrics$call_mean_return, 4), "%\n")
cat("Calls - % Using Delta t-2:", round(comparison_metrics$call_pct_delta2_used, 1), "%\n")
cat("Puts - Mean Return:", round(comparison_metrics$put_mean_return, 4), "%\n")
cat("Puts - % Using Delta t-2:", round(comparison_metrics$put_pct_delta2_used, 1), "%\n")

# Save results
write.csv(dhret_data, "noisy_prices_sp500_delta_hedged_returns.csv", row.names = FALSE)
saveRDS(comparison_metrics, "noisy_prices_sp500_comparison_metrics.rds")



