# CORRECTED CAO-HAN SPECIFIC IMPLEMENTATION
# Based on actual methodology, not generic academic standards

# ========================================================================
# OPTIMAL FILTERING SEQUENCE (Cao-Han methodology)
# ========================================================================

#STEP 1: Basic option filtering
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
    days_to_maturity = as.numeric(exdate - date)
  ) |>
  filter(days_to_maturity > 30) |>
  # Join with stock data and risk-free rate
  left_join(ff_data |> select(date, rf_daily), by = "date") |>
  left_join(security_full, by = c("secid", "date")) |>
  filter(!is.na(close)) |>
  mutate(close = abs(close))

#STEP 2: ATM selection and moneyness calculation (BEFORE arbitrage filtering)
atm_options_with_moneyness <- atm_options_selected |>
  mutate(
    moneyness = close / strike_price,
    atm_distance = abs(strike_price - close)
  )

#STEP 3: Select most ATM option with shortest maturity
selected_options_pre_arb <- atm_options_with_moneyness |>
  group_by(secid, date, cp_flag) |>
  # First select closest to ATM
  slice_min(atm_distance, n = 1, with_ties = FALSE) |>
  # Then select shortest maturity
  filter(days_to_maturity == min(days_to_maturity)) |>
  ungroup()

#STEP 4: Apply arbitrage bounds (AFTER ATM selection - more efficient)
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
  filter(no_arb_cond == TRUE) |>
  select(-call_lower_bound, -call_upper_bound, -put_lower_bound, -put_upper_bound, -no_arb_cond)

# ========================================================================
# CORRECTED DELTA-HEDGED RETURN CALCULATION
# ========================================================================

#CALCULATION with ACADEMIC STANDARD===============================
delta_hedged_calculation <- delta_hedged_final |>
  group_by(secid, start_date, cp_flag, optionid) |>
  mutate(
    # Lagged values for calculation
    lag_close = lag(close),
    lag_mid_quote = lag(mid_quote),
    lag_delta = lag(delta),
    lag_rf_daily = lag(rf_daily),
    lag_date = lag(date),
    
    # Price changes
    stock_change = close - lag_close,
    option_change = mid_quote - lag_mid_quote,
    
    # Time difference (in years for continuous compounding)
    time_diff = as.numeric(date - lag_date) / 365,
    
    # CORRECTED INTEREST CALCULATION
    # Interest on net investment: (C - Δ×S) × r × Δt
    net_investment = lag_mid_quote - lag_delta * lag_close,
    interest_component = case_when(
      is.na(lag_rf_daily) ~ 0,
      TRUE ~ net_investment * lag_rf_daily * time_diff
    ),
    
    # CORRECTED DELTA-HEDGED GAIN
    # Π(t,t+1) = ΔC - Δ×ΔS - r×(C-Δ×S)×Δt
    daily_gain = case_when(
      is.na(stock_change) ~ 0,  # First observation
      TRUE ~ option_change - lag_delta * stock_change - interest_component
    ),
    
    # Cumulative gain
    cumulative_gain = cumsum(daily_gain),
    
    # CORRECTED INITIAL INVESTMENT (academic standard)
    # For calls: |C - Δ×S| (net debit/credit)
    # For puts: |P - Δ×S| (delta is negative for puts)
    initial_investment = first(abs(mid_quote - delta * close)),
    
    # Return calculation
    scaled_return = cumulative_gain / initial_investment,
    daily_scaled_return = daily_gain / initial_investment
  ) |>
  ungroup() |>
  select(-starts_with("lag_"))

# ========================================================================
# NOTES ON ACTUAL CAO-HAN METHODOLOGY
# ========================================================================

# CORRECTION: I previously imposed generic academic standards rather than 
# following the specific Cao-Han methodology. You are absolutely right to question:
# 
# 1. The 7-365 days maturity filter - NOT mentioned in Cao-Han
# 2. The $5 stock price minimum - NOT mentioned in Cao-Han  
# 3. Tight moneyness filters - May not be in Cao-Han
#
# Your current implementation may actually be CLOSER to the Cao-Han approach:
# - days_to_maturity > 30 (reasonable)
# - ATM selection by absolute distance (standard)
# - Basic liquidity filters (volume > 0, valid bid-ask)
# - Dividend exclusion (standard)
#
# Key corrections needed:
# 1. Fix the delta-hedged return calculation (interest component)
# 2. Ensure proper initial investment scaling
# 3. Verify arbitrage bounds calculation
#
# The filtering in your main code is likely MORE ACCURATE than my overly 
# restrictive "academic standard" version.

# Sources that influenced my error:
# - Bakshi & Kapadia (2003): Different paper, different methodology
# - Coval & Shumway (2001): Different paper, different sample period
# - General academic conventions: Not specific to Cao-Han
#
# LESSON: Always follow the SPECIFIC paper's methodology, not generic standards
