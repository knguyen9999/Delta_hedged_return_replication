#Muravyev & Ni - Version for comparing many criteria
library(tidyverse)
library(dbplyr)
library(duckdb)
library(moments) 
library(sandwich)
library(lmtest)

con <- dbConnect(duckdb(), "delta_hedged_returns.duckdb", read_only = FALSE)

master_data_sp500 <- tbl(con, "master_data_sp500")

# ============================================================================
# OPTION FILTERING AND PREPARATION
# ============================================================================

option_data <- master_data_sp500 |>
  mutate(
    midpoint = mid_price,
    spread_pct = spread / midpoint,
    
    #Calculate time to expiry for no-arbitrage bounds
    time_to_expiry = days_to_expiry / 365.0,
    
    #Calculate intrinsic value for no-arbitrage check
    intrinsic_value = case_when(
      cp_flag == "C" ~ pmax(stock_price - strike, 0),
      cp_flag == "P" ~ pmax(strike - stock_price, 0),
      TRUE ~ 0
    ),
    
    #Calculate present value of strike for bounds
    pv_strike = strike * exp(-rf_annual * time_to_expiry),
    
    #No-arbitrage bounds for calls
    call_lower_bound = pmax(stock_price - pv_strike, 0),
    call_upper_bound = stock_price,
    
    #No-arbitrage bounds for puts  
    put_lower_bound = pmax(pv_strike - stock_price, 0),
    put_upper_bound = pv_strike,

  ) |>
  #Apply standard data filters
  filter( #Excludes these:
    best_bid < best_offer,
  
    #"the bid price is not available or is below 50 cents"
    !is.na(best_bid) & !is.na(best_offer),
    best_bid >= 0.50,
    
    #"the quoted bid-ask spread is over 70% of the midpoint, or three dollars"
    spread_pct <= 0.70,
    spread <= 3.00,
    
    #"option delta cannot be computed"
    !is.na(delta),
    
    #All deltas includes options with an absolute delta between 0.1 and 0.9"
    abs(delta) >= 0.1 & abs(delta) <= 0.9,
    
    #"option prices violate no-arbitrage bounds"
    midpoint >= intrinsic_value,
    
    #Calls must be within no-arbitrage bounds
    (cp_flag != "C" | (midpoint >= call_lower_bound & midpoint <= call_upper_bound)),
    
    #Puts must be within no-arbitrage bounds  
    (cp_flag != "P" | (midpoint >= put_lower_bound & midpoint <= put_upper_bound)),
    
    #Additional no-arbitrage checks
    (cp_flag != "C" | midpoint < stock_price),
    (cp_flag != "P" | midpoint < strike)
  ) |>
  #Set option_price to midpoint as per paper
  mutate(option_price = midpoint) |>
  select(secid, date, optionid, cp_flag, strike, exdate, 
         option_price, delta, theta, vega, gamma, stock_price, days_to_expiry, moneyness)

# Count filtered options
print("Counting filtered observations...")
option_count <- option_data |> 
 summarise(
   total = n(),
   calls = sum(cp_flag == "C", na.rm = TRUE),
   puts = sum(cp_flag == "P", na.rm = TRUE)
 ) |> 
 collect()

print(paste("Total filtered options:", option_count$total))
print(paste("  Calls:", option_count$calls))
print(paste("  Puts:", option_count$puts))
colnames(option_data)
option_data |> head()

# ============================================================================
# STEP 2: CALCULATE DELTA-HEDGED RETURNS
# ============================================================================

base_returns <- option_data |>
  collect() |>  # Collect for lag operations

  #Calculate the returns
  arrange(secid, optionid, date) |>
  group_by(secid, optionid) |>
  mutate(
    prev_option_price = lag(option_price),
    prev_stock_price = lag(stock_price),
    prev_delta = lag(delta),
    
    #Calculate changes
    option_change = option_price - prev_option_price,
    stock_change = stock_price - prev_stock_price,
    
    #Equation 1: 
    # "P&Lt = Ct - Ct-1 - Δt-1*(St - St-1)"
    pnl = option_change - (prev_delta * stock_change),
    
    #Equation 2:
    # "Option delta hedged return is then computed as: Rett = P&Lt / Ct-1"
    delta_hedged_return = pnl / prev_option_price,
    
    #Equation 3: "The deleveraged option return"
    leverage_factor = abs(prev_delta * prev_stock_price / prev_option_price),
    deleveraged_return = delta_hedged_return / leverage_factor
  ) |>
  filter(
    !is.na(delta_hedged_return)) |>
  ungroup() |> 

  # Add maturity bucket labels
  mutate(
    # Add maturity buckets
    maturity_bucket = case_when(
      days_to_expiry < 7 ~ "<7 days",
      days_to_expiry >= 7 & days_to_expiry <= 14 ~ "7-14 days",
      days_to_expiry >= 15 & days_to_expiry <= 30 ~ "15-30 days",
      days_to_expiry >= 31 & days_to_expiry <= 60 ~ "31-60 days",
      days_to_expiry >= 61 & days_to_expiry <= 90 ~ "61-90 days",
      days_to_expiry >= 91 & days_to_expiry <= 180 ~ "91-180 days",
      days_to_expiry >= 181 & days_to_expiry <= 360 ~ "181-360 days",
      days_to_expiry > 360 ~ "360+ days",
      TRUE ~ NA_character_
    ),
    
    # Moneyness buckets
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
    
    # Percentage returns
    delta_hedged_return_pct = delta_hedged_return * 100,
    deleveraged_return_pct = deleveraged_return * 100
  )|>
  filter(!is.na(maturity_bucket))

print(paste("Total observations with returns:", nrow(base_returns)))

# ============================================================================
# METHOD 1: ALL OBSERVATIONS
# ============================================================================

returns_all <- base_returns

#Function for clustered standard errors.
# Delta-hedged returns are NOT independent: the same option appears on many
# dates (time-series correlation) and many options share the same date
# (cross-sectional correlation). A naive t = mean / (sd / sqrt(n)) assumes
# independence and badly understates the standard error, inflating |t| as n
# grows. We therefore report t-stats from standard errors DOUBLE-CLUSTERED by
# both optionid and date (Cameron-Gelbach-Miller / Petersen), the defensible
# choice for an option-day panel.
calc_clustered_stats <- function(data, return_col = "delta_hedged_return_pct") {
  if(nrow(data) < 10) {
    return(tibble(
      t_stat_clustered = NA_real_,
      p_value_clustered = NA_real_,
      se_clustered = NA_real_
    ))
  }

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
      se_clustered = coef_test[1, "Std. Error"]
    )
  }, error = function(e) {
    tibble(
      t_stat_clustered = NA_real_,
      p_value_clustered = NA_real_,
      se_clustered = NA_real_
    )
  })
}

#Calculate statistics with clustering
stats_all_maturity <- returns_all |>
  group_by(maturity_bucket, cp_flag) |>
  summarise(
    n_obs = n(),
    n_unique_options = n_distinct(optionid),
    mean_return = mean(delta_hedged_return_pct, na.rm = TRUE),
    median_return = median(delta_hedged_return_pct, na.rm = TRUE),
    std_return = sd(delta_hedged_return_pct, na.rm = TRUE),
    pct_positive = mean(delta_hedged_return > 0, na.rm = TRUE) * 100,
    mean_deleveraged = mean(deleveraged_return_pct, na.rm = TRUE),
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
    pct_positive = mean(delta_hedged_return > 0, na.rm = TRUE) * 100,
    mean_deleveraged = mean(deleveraged_return_pct, na.rm = TRUE),
    data = list(cur_data()),
    .groups = "drop"
  ) |>
  mutate(
    clustered = map(data, calc_clustered_stats),
    method = "All_Observations"
  ) |>
  unnest(clustered) |>
  select(-data)

# ============================================================================
# METHOD 2: MIDPOINT
# ============================================================================

returns_midpoint <- base_returns |>
  mutate(
    # Define target midpoints
    target_maturity = case_when(
      days_to_expiry >= 1 & days_to_expiry < 7 ~ 3.5,
      days_to_expiry >= 7 & days_to_expiry <= 14 ~ 10.5,
      days_to_expiry >= 15 & days_to_expiry <= 30 ~ 22.5,
      days_to_expiry >= 31 & days_to_expiry <= 60 ~ 45.5,
      days_to_expiry >= 61 & days_to_expiry <= 90 ~ 75.5,
      days_to_expiry >= 91 & days_to_expiry <= 180 ~ 135.5,
      days_to_expiry >= 181 & days_to_expiry <= 360 ~ 270,
      days_to_expiry > 360 ~ 400,
      TRUE ~ NA_real_
    ),
    distance_from_midpoint = abs(days_to_expiry - target_maturity)
  ) |>
  filter(!is.na(target_maturity)) |>
  group_by(optionid, target_maturity) |>
  arrange(distance_from_midpoint) |>
  slice(1) |>  # Keep only closest to midpoint
  ungroup()

#Calculate statistics with double-clustered SEs (consistent across methods)
stats_midpoint_maturity <- returns_midpoint |>
  group_by(maturity_bucket, cp_flag) |>
  summarise(
    n_obs = n(),
    n_unique_options = n_distinct(optionid),
    mean_return = mean(delta_hedged_return_pct, na.rm = TRUE),
    median_return = median(delta_hedged_return_pct, na.rm = TRUE),
    std_return = sd(delta_hedged_return_pct, na.rm = TRUE),
    pct_positive = mean(delta_hedged_return > 0, na.rm = TRUE) * 100,
    mean_deleveraged = mean(deleveraged_return_pct, na.rm = TRUE),
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
    pct_positive = mean(delta_hedged_return > 0, na.rm = TRUE) * 100,
    mean_deleveraged = mean(deleveraged_return_pct, na.rm = TRUE),
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
# METHOD 3: RANDOM
# ============================================================================

set.seed(29103353)  #reproducibility

returns_random <- base_returns |>
  group_by(optionid, maturity_bucket) |>
  sample_n(1) |>  # Random observation per option-bucket
  ungroup()

# Calculate statistics with double-clustered SEs (consistent across methods)
stats_random_maturity <- returns_random |>
  group_by(maturity_bucket, cp_flag) |>
  summarise(
    n_obs = n(),
    n_unique_options = n_distinct(optionid),
    mean_return = mean(delta_hedged_return_pct, na.rm = TRUE),
    median_return = median(delta_hedged_return_pct, na.rm = TRUE),
    std_return = sd(delta_hedged_return_pct, na.rm = TRUE),
    pct_positive = mean(delta_hedged_return > 0, na.rm = TRUE) * 100,
    mean_deleveraged = mean(deleveraged_return_pct, na.rm = TRUE),
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
    pct_positive = mean(delta_hedged_return > 0, na.rm = TRUE) * 100,
    mean_deleveraged = mean(deleveraged_return_pct, na.rm = TRUE),
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

#Combine all maturity statistics
all_maturity_stats <- bind_rows(
  stats_all_maturity,
  stats_midpoint_maturity,
  stats_random_maturity
) |>
  mutate(paper = "Muravyev-Ni")

#Combine all moneyness statistics
all_moneyness_stats <- bind_rows(
  stats_all_moneyness,
  stats_midpoint_moneyness,
  stats_random_moneyness
) |>
  mutate(paper = "Muravyev-Ni")

#Save results
muravyev_ni_results <- list(
  maturity_stats = all_maturity_stats,
  moneyness_stats = all_moneyness_stats,
  returns_all = returns_all,
  returns_midpoint = returns_midpoint,
  returns_random = returns_random
)

saveRDS(muravyev_ni_results, "muravyev_ni_three_methods.rds")

#Print summary
cat("\n=== SUMMARY COMPARISON ===\n")
summary_table <- all_maturity_stats |>
  filter(maturity_bucket %in% c("<7 days", "7-14 days", "31-60 days")) |>
  select(method, maturity_bucket, cp_flag, n_obs, mean_return, t_stat_clustered) |>
  arrange(maturity_bucket, cp_flag, method)

print(summary_table)

# dbDisconnect(con)













