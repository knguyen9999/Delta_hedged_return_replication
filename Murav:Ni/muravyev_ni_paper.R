#Muravyev & Ni 

library(dplyr)
library(dbplyr)
library(duckdb)

con <- dbConnect(duckdb(), "delta_hedged_returns.duckdb", read_only = FALSE)

master_data_sp500 <- tbl(con, "master_data_sp500")
sp500_secid <- 108105

# OPTION FILTERING AND PREPARATION ======================================
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
    pv_strike = strike * exp(-rf_daily * time_to_expiry),
    
    #No-arbitrage bounds for calls
    call_lower_bound = pmax(stock_price - pv_strike, 0),
    call_upper_bound = stock_price,
    
    #No-arbitrage bounds for puts  
    put_lower_bound = pmax(pv_strike - stock_price, 0),
    put_upper_bound = pv_strike
  ) |>
  #Apply standard data filters
  filter(
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
    
    # Additional no-arbitrage checks
    (cp_flag != "C" | midpoint < stock_price),
    (cp_flag != "P" | midpoint < strike)
  ) |>
  # Set option_price to midpoint as per paper
  mutate(option_price = midpoint) |>
  select(secid, date, optionid, cp_flag, strike, exdate, 
         option_price, delta, stock_price, days_to_expiry)

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

returns_data <- option_data |>
  collect() |>  # Collect for lag operations
  arrange(secid, optionid, date) |>
  group_by(secid, optionid) |>
  mutate(
    prev_option_price = lag(option_price),
    prev_stock_price = lag(stock_price),
    prev_delta = lag(delta),
    
    # Calculate changes
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
  filter(!is.na(delta_hedged_return)) |>
  # Remove extreme returns likely due to data errors
  filter(abs(delta_hedged_return) < 10) |>  
  ungroup()

# Get final returns (percentage)
returns_data <- returns_data |>
  mutate(
    delta_hedged_return_pct = delta_hedged_return * 100,
    deleveraged_return_pct = deleveraged_return * 100
  )

print(paste("Total observations with returns:", nrow(returns_data)))

# ============================================================================
# CALCULATE SUMMARY STATISTICS 
# ============================================================================

# Statistics by option type
cp_stats <- returns_data |>
  group_by(cp_flag) |>
  summarise(
    n_obs = n(),
    mean_return = mean(delta_hedged_return_pct, na.rm = TRUE),
    median_return = median(delta_hedged_return_pct, na.rm = TRUE),
    std_return = sd(delta_hedged_return_pct, na.rm = TRUE),
    pct_positive = mean(delta_hedged_return > 0, na.rm = TRUE) * 100,
    mean_deleveraged = mean(deleveraged_return_pct, na.rm = TRUE)
  )

# Display results for calls
call_stats <- cp_stats |> filter(cp_flag == "C") |> slice(1)
if(length(call_stats$n_obs) > 0 && call_stats$n_obs > 0) {
  cat("Call Options Summary:\n")
  cat("---------------------\n")
  cat("N =", call_stats$n_obs, "\n")
  cat("Mean Return:", round(call_stats$mean_return, 2), "%\n")
  cat("Mean Deleveraged:", round(call_stats$mean_deleveraged, 2), "%\n")
  cat("Median Return:", round(call_stats$median_return, 2), "%\n")
  cat("Std Dev:", round(call_stats$std_return, 2), "%\n")
  cat("% Positive:", round(call_stats$pct_positive, 2), "%\n")
}

# Display results for puts
put_stats <- cp_stats |> filter(cp_flag == "P") |> slice(1)
if(length(put_stats$n_obs) > 0 && put_stats$n_obs > 0) {
  cat("\nPut Options Summary:\n")
  cat("--------------------\n")
  cat("N =", put_stats$n_obs, "\n")
  cat("Mean Return:", round(put_stats$mean_return, 2), "%\n")
  cat("Mean Deleveraged:", round(put_stats$mean_deleveraged, 2), "%\n")
  cat("Median Return:", round(put_stats$median_return, 2), "%\n")
  cat("Std Dev:", round(put_stats$std_return, 2), "%\n")
  cat("% Positive:", round(put_stats$pct_positive, 2), "%\n")
}

# Statistical tests
call_returns <- returns_data |> filter(cp_flag == "C") |> pull(delta_hedged_return_pct)
put_returns <- returns_data |> filter(cp_flag == "P") |> pull(delta_hedged_return_pct)

if(length(call_returns) > 0) {
  call_ttest <- t.test(call_returns)
} else {
  call_ttest <- list(statistic = NA, p.value = NA)
}

if(length(put_returns) > 0) {
  put_ttest <- t.test(put_returns)
} else {
  put_ttest <- list(statistic = NA, p.value = NA)
}

cat("\n=== STATISTICAL TESTS ===\n")
if(length(call_returns) > 0) {
  cat("Call Options - Mean Delta-Hedged Return:\n")
  cat("t-stat:", round(call_ttest$statistic, 2), ", p-value:", format(call_ttest$p.value, scientific = TRUE), "\n")
}

if(length(put_returns) > 0) {
  cat("\nPut Options - Mean Delta-Hedged Return:\n")
  cat("t-stat:", round(put_ttest$statistic, 2), ", p-value:", format(put_ttest$p.value, scientific = TRUE), "\n")
}

#==============================================================================#
# CREATE STANDARDIZED METRICS FOR COMPARISON
#==============================================================================#

# Create standardized comparison metrics
comparison_metrics <- list(
  method = "Muravyev-Ni (2018), Day/Night (Deleveraged)",
  data = "S&P 500 Index Options",
  
  # Sample size
  n_call_options = ifelse(length(call_stats$n_obs) > 0, call_stats$n_obs, 0),
  n_put_options = ifelse(length(put_stats$n_obs) > 0, put_stats$n_obs, 0),
  
  # Returns - Calls
  call_mean_return = ifelse(length(call_stats$mean_return) > 0, call_stats$mean_return, NA_real_),
  call_median_return = ifelse(length(call_stats$median_return) > 0, call_stats$median_return, NA_real_),
  call_std_return = ifelse(length(call_stats$std_return) > 0, call_stats$std_return, NA_real_),
  call_pct_positive = ifelse(length(call_stats$pct_positive) > 0, call_stats$pct_positive, NA_real_),
  
  # Returns - Puts
  put_mean_return = ifelse(length(put_stats$mean_return) > 0, put_stats$mean_return, NA_real_),
  put_median_return = ifelse(length(put_stats$median_return) > 0, put_stats$median_return, NA_real_),
  put_std_return = ifelse(length(put_stats$std_return) > 0, put_stats$std_return, NA_real_),
  put_pct_positive = ifelse(length(put_stats$pct_positive) > 0, put_stats$pct_positive, NA_real_),
  
  # Statistical significance
  call_t_stat = ifelse(!is.na(call_ttest$statistic), call_ttest$statistic[[1]], NA_real_),
  call_p_value = call_ttest$p.value,
  put_t_stat = ifelse(!is.na(put_ttest$statistic), put_ttest$statistic[[1]], NA_real_),
  put_p_value = put_ttest$p.value,
  
  # Additional metrics specific to this method
  call_mean_deleveraged = ifelse(length(call_stats$mean_deleveraged) > 0, call_stats$mean_deleveraged, NA_real_),
  put_mean_deleveraged = ifelse(length(put_stats$mean_deleveraged) > 0, put_stats$mean_deleveraged, NA_real_)
)

cat("\n=== STANDARDIZED METRICS FOR COMPARISON ===\n")
cat("Method: Day/Night (Deleveraged)\n")
cat("Calls - Mean Return:", round(comparison_metrics$call_mean_return, 2), "%\n")
cat("Calls - Mean Deleveraged:", round(comparison_metrics$call_mean_deleveraged, 2), "%\n")
cat("Puts - Mean Return:", round(comparison_metrics$put_mean_return, 2), "%\n")
cat("Puts - Mean Deleveraged:", round(comparison_metrics$put_mean_deleveraged, 2), "%\n")

saveRDS(comparison_metrics, "day_night_sp500_comparison_metrics.rds")
