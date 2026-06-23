library(tidyverse)
library(dbplyr)
library(duckdb)
library(moments)
library(sandwich)
library(lmtest)


con <- dbConnect(duckdb(), "delta_hedged_returns.duckdb", read_only = FALSE)

# Load the master S&P 500 data
master_data_sp500 <- tbl(con, "master_data_sp500")

# S&P 500 secid
sp500_secid <- 108105


# ============================================================================
# OPTION FILTERING AND PREPARATION
# ============================================================================

dhret_data <- master_data_sp500 |> 
  # Basic variables
  mutate(
    iscall = if_else(cp_flag == "C", 1L, 0L),
    ndays = days_to_expiry,
    rel_spread = spread / pmax(mid_price, 1e-12),
  ) |>
  # Apply baseline filters BEFORE collecting (matching paper's criteria)
  filter(
    mid_price >= 0.10,                           # Minimum midpoint
    rel_spread <= 0.50,                          # Max 50% relative spread
    open_interest > 0,                           # Positive open interest
    !is.na(impl_volatility),                     # IV present
    abs(delta) >= 0.01, abs(delta) <= 0.99,      # Delta bounds
    best_offer > best_bid,                       # Valid quotes
    !is.na(stock_price), stock_price > 0,        # Valid underlying price
    !is.na(rf_daily)                             # Valid risk-free rate
  ) |>
  # Collect for lag operations
  collect()

# ============================================================================
# CALCULATE BASE DAILY RETURNS WITH DELTA FALLBACK
# ============================================================================

base_returns <- dhret_data |>
  arrange(secid, optionid, date) |>
  group_by(secid, optionid) |>
  # Add lags using optionid grouping
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
    
    #Simple excess returns (NOT log returns - paper uses simple returns)
    xret_opt = (mid_price / lag1_mid) - gross_rf,      # Option excess return
    xret_stock = (stock_price / lag1_stock) - gross_rf, # Stock excess return
    
    #Correct hedge ratio: shares of stock per option (paper's formulation)
    beta_hedge = (lag1_stock / lag1_mid) * delta_final,
    
    #DELTA-HEDGED RETURN (paper's calculation)
    dhxret_avg = xret_opt - beta_hedge * xret_stock
  ) |>
  #Final filters for valid calculations
  filter(
    !is.na(dhxret_avg),
    !is.na(lag1_mid), lag1_mid > 0,        # t-1 option price available
    !is.na(lag1_stock), lag1_stock > 0,    # t-1 stock price available  
    !is.na(delta_final),                   # Delta (t-1 or t-2) available
    !is.na(delta_lag_used)                 # Valid delta lag identifier
  )|>
  mutate(
    # Add buckets (including <7 days)
    maturity_bucket = case_when(
      ndays < 7 ~ "<7 days",
      ndays >= 7 & ndays <= 14 ~ "7-14 days",
      ndays >= 15 & ndays <= 30 ~ "15-30 days",
      ndays >= 31 & ndays <= 60 ~ "31-60 days",
      ndays >= 61 & ndays <= 90 ~ "61-90 days",
      ndays >= 91 & ndays <= 180 ~ "91-180 days",
      ndays >= 181 & ndays <= 360 ~ "181-360 days",
      ndays > 360 ~ "360+ days",
      TRUE ~ NA_character_
    ),
    
    moneyness_bucket = case_when(
      cp_flag == "C" ~ case_when(
        moneyness < 0.90 ~ "Deep OTM",
        moneyness < 0.97 ~ "OTM",
        moneyness <= 1.03 ~ "ATM",
        moneyness <= 1.10 ~ "ITM",
        TRUE ~ "Deep ITM"
      ),
      cp_flag == "P" ~ case_when(
        moneyness > 1.10 ~ "Deep OTM",
        moneyness > 1.03 ~ "OTM",
        moneyness >= 0.97 ~ "ATM",
        moneyness >= 0.90 ~ "ITM",
        TRUE ~ "Deep ITM"
      )
    ),
    
    delta_hedged_return_pct = dhxret_avg * 100
  ) |>
  filter(!is.na(maturity_bucket))

print(paste("Base returns calculated:", nrow(base_returns), "observations"))

# ============================================================================
# METHOD 1: ALL OBSERVATIONS
# ============================================================================

returns_all <- base_returns

# Function for clustered standard errors.
# Delta-hedged returns are NOT independent: the same option appears on many
# dates (time-series correlation) and many options share the same date
# (cross-sectional correlation). A naive t = mean / (sd / sqrt(n)) assumes
# independence and badly understates the standard error, inflating |t| as n
# grows. We therefore report t-stats from standard errors DOUBLE-CLUSTERED by
# both optionid and date (Cameron-Gelbach-Miller / Petersen), which is the
# defensible choice for an option-day panel.
calc_clustered_stats <- function(data, return_col = "delta_hedged_return_pct") {
  if(nrow(data) < 10) {
    return(tibble(
      t_stat_clustered = NA_real_,
      p_value_clustered = NA_real_,
      se_clustered = NA_real_,
      pct_delta2_used = NA_real_
    ))
  }

  pct_delta2 <- mean(data$delta_lag_used == 2, na.rm = TRUE) * 100

  tryCatch({
    formula_str <- paste(return_col, "~ 1")
    model <- lm(as.formula(formula_str), data = data)
    # Two-way cluster-robust covariance: by optionid AND by date.
    # Falls back to one-way (optionid) if date has too few distinct values.
    vcov_cluster <- if (dplyr::n_distinct(data$date) > 1) {
      sandwich::vcovCL(model, cluster = ~ optionid + date)
    } else {
      sandwich::vcovCL(model, cluster = ~ optionid)
    }
    coef_test <- coeftest(model, vcov = vcov_cluster)

    tibble(
      t_stat_clustered = coef_test[1, "t value"],
      p_value_clustered = coef_test[1, "Pr(>|t|)"],
      se_clustered = coef_test[1, "Std. Error"],
      pct_delta2_used = pct_delta2
    )
  }, error = function(e) {
    tibble(
      t_stat_clustered = NA_real_,
      p_value_clustered = NA_real_,
      se_clustered = NA_real_,
      pct_delta2_used = pct_delta2
    )
  })
}

stats_all_maturity <- returns_all |>
  group_by(maturity_bucket, cp_flag) |>
  summarise(
    n_obs = n(),
    n_unique_options = n_distinct(optionid),
    mean_return = mean(delta_hedged_return_pct, na.rm = TRUE),
    median_return = median(delta_hedged_return_pct, na.rm = TRUE),
    std_return = sd(delta_hedged_return_pct, na.rm = TRUE),
    pct_positive = mean(dhxret_avg > 0, na.rm = TRUE) * 100,
    data = list(cur_data()),
    .groups = "drop"
  ) |>
  mutate(
    clustered = map(data, calc_clustered_stats),
    method = "All_Observations"
  ) |>
  unnest(clustered) |>
  select(-data)

stats_all_moneyness <- returns_all |>
  group_by(moneyness_bucket, cp_flag) |>
  summarise(
    n_obs = n(),
    n_unique_options = n_distinct(optionid),
    mean_return = mean(delta_hedged_return_pct, na.rm = TRUE),
    median_return = median(delta_hedged_return_pct, na.rm = TRUE),
    std_return = sd(delta_hedged_return_pct, na.rm = TRUE),
    pct_positive = mean(dhxret_avg > 0, na.rm = TRUE) * 100,
    data = list(cur_data()),
    .groups = "drop"
  ) |>
  mutate(
    clustered = map(data, calc_clustered_stats),
    method = "All_Observations"
  ) |>
  unnest(clustered) |>
  select(-data)

print(paste("All observations:", nrow(returns_all)))

# ============================================================================
# METHOD 2: MIDPOINT
# ============================================================================

returns_midpoint <- base_returns |>
  mutate(
    target_maturity = case_when(
      ndays >= 1 & ndays < 7 ~ 3.5,
      ndays >= 7 & ndays <= 14 ~ 10.5,
      ndays >= 15 & ndays <= 30 ~ 22.5,
      ndays >= 31 & ndays <= 60 ~ 45.5,
      ndays >= 61 & ndays <= 90 ~ 75.5,
      ndays >= 91 & ndays <= 180 ~ 135.5,
      ndays >= 181 & ndays <= 360 ~ 270,
      ndays > 360 ~ 400,
      TRUE ~ NA_real_
    ),
    distance_from_midpoint = abs(ndays - target_maturity)
  ) |>
  filter(!is.na(target_maturity)) |>
  group_by(optionid, target_maturity) |>
  arrange(distance_from_midpoint) |>
  slice(1) |>
  ungroup()

stats_midpoint_maturity <- returns_midpoint |>
  group_by(maturity_bucket, cp_flag) |>
  summarise(
    n_obs = n(),
    n_unique_options = n_distinct(optionid),
    mean_return = mean(delta_hedged_return_pct, na.rm = TRUE),
    median_return = median(delta_hedged_return_pct, na.rm = TRUE),
    std_return = sd(delta_hedged_return_pct, na.rm = TRUE),
    pct_positive = mean(dhxret_avg > 0, na.rm = TRUE) * 100,
    data = list(cur_data()),
    .groups = "drop"
  ) |>
  mutate(
    clustered = map(data, calc_clustered_stats),
    method = "Midpoint"
  ) |>
  unnest(clustered) |>
  select(-data)

stats_midpoint_moneyness <- returns_midpoint |>
  group_by(moneyness_bucket, cp_flag) |>
  summarise(
    n_obs = n(),
    n_unique_options = n_distinct(optionid),
    mean_return = mean(delta_hedged_return_pct, na.rm = TRUE),
    median_return = median(delta_hedged_return_pct, na.rm = TRUE),
    std_return = sd(delta_hedged_return_pct, na.rm = TRUE),
    pct_positive = mean(dhxret_avg > 0, na.rm = TRUE) * 100,
    data = list(cur_data()),
    .groups = "drop"
  ) |>
  mutate(
    clustered = map(data, calc_clustered_stats),
    method = "Midpoint"
  ) |>
  unnest(clustered) |>
  select(-data)

print(paste("Midpoint observations:", nrow(returns_midpoint)))

# ============================================================================
# METHOD 3: RANDOM SAMPLING
# ============================================================================

set.seed(29103353)

returns_random <- base_returns |>
  group_by(optionid, maturity_bucket) |>
  sample_n(1) |>
  ungroup()

stats_random_maturity <- returns_random |>
  group_by(maturity_bucket, cp_flag) |>
  summarise(
    n_obs = n(),
    n_unique_options = n_distinct(optionid),
    mean_return = mean(delta_hedged_return_pct, na.rm = TRUE),
    median_return = median(delta_hedged_return_pct, na.rm = TRUE),
    std_return = sd(delta_hedged_return_pct, na.rm = TRUE),
    pct_positive = mean(dhxret_avg > 0, na.rm = TRUE) * 100,
    data = list(cur_data()),
    .groups = "drop"
  ) |>
  mutate(
    clustered = map(data, calc_clustered_stats),
    method = "Random"
  ) |>
  unnest(clustered) |>
  select(-data)

stats_random_moneyness <- returns_random |>
  group_by(moneyness_bucket, cp_flag) |>
  summarise(
    n_obs = n(),
    n_unique_options = n_distinct(optionid),
    mean_return = mean(delta_hedged_return_pct, na.rm = TRUE),
    median_return = median(delta_hedged_return_pct, na.rm = TRUE),
    std_return = sd(delta_hedged_return_pct, na.rm = TRUE),
    pct_positive = mean(dhxret_avg > 0, na.rm = TRUE) * 100,
    data = list(cur_data()),
    .groups = "drop"
  ) |>
  mutate(
    clustered = map(data, calc_clustered_stats),
    method = "Random"
  ) |>
  unnest(clustered) |>
  select(-data)

print(paste("Random observations:", nrow(returns_random)))

# ============================================================================
# COMBINE AND SAVE RESULTS
# ============================================================================

#Combine all statistics
all_maturity_stats <- bind_rows(
  stats_all_maturity,
  stats_midpoint_maturity,
  stats_random_maturity
) |>
  mutate(paper = "Duarte-Jones-Wang")

all_moneyness_stats <- bind_rows(
  stats_all_moneyness,
  stats_midpoint_moneyness,
  stats_random_moneyness
) |>
  mutate(paper = "Duarte-Jones-Wang")

#Save results
djw_results <- list(
  maturity_stats = all_maturity_stats,
  moneyness_stats = all_moneyness_stats,
  returns_all = returns_all,
  returns_midpoint = returns_midpoint,
  returns_random = returns_random
)

saveRDS(djw_results, "djw_three_methods.rds")

#Print summary
cat("\n=== SUMMARY COMPARISON ===\n")
summary_table <- all_maturity_stats |>
  filter(maturity_bucket %in% c("<7 days", "7-14 days", "31-60 days")) |>
  select(method, maturity_bucket, cp_flag, n_obs, mean_return, t_stat_clustered, pct_delta2_used) |>
  arrange(maturity_bucket, cp_flag, method)

print(summary_table)

dbDisconnect(con)
