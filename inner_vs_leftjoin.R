library(dplyr)
library(dbplyr)
library(duckdb)

# Connect to database
con <- dbConnect(duckdb(), "delta_hedged_returns.duckdb", read_only = TRUE)

# Get table references
option_price_tbl <- tbl(con, "option_price")
security_price_tbl <- tbl(con, "security_price")
ff_data_tbl <- tbl(con, "ff_rates")

sp500_secid <- 108105

print("=============================================================================")
print("COMPARING INNER JOIN VS LEFT JOIN FOR S&P 500 DATA")
print("=============================================================================\n")

#==============================================================================#
# 1. COUNT ROWS IN EACH BASE TABLE
#==============================================================================#

print("STEP 1: Base table counts for S&P 500")
print("----------------------------------------")

# Option data count
option_count <- option_price_tbl |>
  filter(secid == sp500_secid) |>
  summarise(
    total_rows = n(),
    unique_dates = n_distinct(date),
    date_min = min(date, na.rm = TRUE),
    date_max = max(date, na.rm = TRUE)
  ) |>
  collect()

print("Option price data:")
print(option_count)

# Security price count
security_count <- security_price_tbl |>
  filter(secid == sp500_secid) |>
  summarise(
    total_rows = n(),
    unique_dates = n_distinct(date),
    date_min = min(date, na.rm = TRUE),
    date_max = max(date, na.rm = TRUE)
  ) |>
  collect()

print("\nSecurity price data:")
print(security_count)

# Risk-free rate count
rf_count <- ff_data_tbl |>
  summarise(
    total_rows = n(),
    unique_dates = n_distinct(date),
    date_min = min(date, na.rm = TRUE),
    date_max = max(date, na.rm = TRUE)
  ) |>
  collect()

print("\nRisk-free rate data:")
print(rf_count)

#==============================================================================#
# 2. LEFT JOIN - Keep all options, even without matching stock/rf data
#==============================================================================#

print("\n\nSTEP 2: LEFT JOIN Results")
print("----------------------------------------")

left_join_data <- option_price_tbl |>
  filter(secid == sp500_secid) |>
  # LEFT JOIN with stock prices
  left_join(
    security_price_tbl |>
      filter(secid == sp500_secid) |>
      select(secid, date, close),
    by = c("secid", "date")
  ) |>
  # LEFT JOIN with risk-free rates
  left_join(
    ff_data_tbl |> 
      select(date, rf_daily),
    by = "date"
  ) |>
  summarise(
    total_rows = n(),
    rows_with_stock_price = sum(!is.na(close)),
    rows_missing_stock_price = sum(is.na(close)),
    rows_with_rf = sum(!is.na(rf_daily)),
    rows_missing_rf = sum(is.na(rf_daily)),
    rows_with_both = sum(!is.na(close) & !is.na(rf_daily)),
    rows_missing_either = sum(is.na(close) | is.na(rf_daily)),
    pct_complete = round(100 * sum(!is.na(close) & !is.na(rf_daily)) / n(), 2)
  ) |>
  collect()

print("LEFT JOIN summary:")
print(left_join_data)

#==============================================================================#
# 3. INNER JOIN - Only keep rows with complete data
#==============================================================================#

print("\n\nSTEP 3: INNER JOIN Results")
print("----------------------------------------")

inner_join_data <- option_price_tbl |>
  filter(secid == sp500_secid) |>
  # INNER JOIN with stock prices
  inner_join(
    security_price_tbl |>
      filter(secid == sp500_secid) |>
      select(secid, date, close),
    by = c("secid", "date")
  ) |>
  # INNER JOIN with risk-free rates
  inner_join(
    ff_data_tbl |> 
      select(date, rf_daily),
    by = "date"
  ) |>
  summarise(
    total_rows = n(),
    # All should have data (by definition of inner join)
    rows_with_stock_price = sum(!is.na(close)),
    rows_with_rf = sum(!is.na(rf_daily))
  ) |>
  collect()

print("INNER JOIN summary:")
print(inner_join_data)

#==============================================================================#
# 4. DETAILED COMPARISON
#==============================================================================#

print("\n\nSTEP 4: DETAILED COMPARISON")
print("----------------------------------------")

comparison <- data.frame(
  Join_Type = c("LEFT JOIN", "INNER JOIN"),
  Total_Rows = c(left_join_data$total_rows, inner_join_data$total_rows),
  Complete_Rows = c(left_join_data$rows_with_both, inner_join_data$total_rows),
  Missing_Data_Rows = c(left_join_data$rows_missing_either, 0),
  Pct_Complete = c(left_join_data$pct_complete, 100)
)

print(comparison)

rows_lost <- left_join_data$total_rows - inner_join_data$total_rows
pct_lost <- round(100 * rows_lost / left_join_data$total_rows, 2)

print(paste("\nRows excluded by INNER JOIN:", format(rows_lost, big.mark = ",")))
print(paste("Percentage of data excluded:", pct_lost, "%"))

#==============================================================================#
# 5. INVESTIGATE MISSING DATA PATTERNS
#==============================================================================#

print("\n\nSTEP 5: INVESTIGATING MISSING DATA PATTERNS")
print("----------------------------------------")

# Check which dates have missing stock prices
missing_stock_dates <- option_price_tbl |>
  filter(secid == sp500_secid) |>
  select(date) |>
  distinct() |>
  left_join(
    security_price_tbl |>
      filter(secid == sp500_secid) |>
      select(date, close),
    by = "date"
  ) |>
  filter(is.na(close)) |>
  arrange(date) |>
  collect()

if (nrow(missing_stock_dates) > 0) {
  print(paste("Dates with options but no stock prices:", nrow(missing_stock_dates)))
  print("First 10 dates:")
  print(head(missing_stock_dates, 10))
} else {
  print("No dates with missing stock prices")
}

# Check which dates have missing risk-free rates
missing_rf_dates <- option_price_tbl |>
  filter(secid == sp500_secid) |>
  select(date) |>
  distinct() |>
  left_join(
    ff_data_tbl |> select(date, rf_daily),
    by = "date"
  ) |>
  filter(is.na(rf_daily)) |>
  arrange(date) |>
  collect()

if (nrow(missing_rf_dates) > 0) {
  print(paste("\nDates with options but no risk-free rates:", nrow(missing_rf_dates)))
  print("First 10 dates:")
  print(head(missing_rf_dates, 10))
} else {
  print("\nNo dates with missing risk-free rates")
}

#==============================================================================#
# 6. CHECK DATA QUALITY FOR KEY FIELDS
#==============================================================================#

print("\n\nSTEP 6: DATA QUALITY CHECK (Using INNER JOIN)")
print("----------------------------------------")

quality_check <- option_price_tbl |>
  filter(secid == sp500_secid) |>
  inner_join(
    security_price_tbl |>
      filter(secid == sp500_secid) |>
      select(secid, date, close),
    by = c("secid", "date")
  ) |>
  inner_join(
    ff_data_tbl |> select(date, rf_daily),
    by = "date"
  ) |>
  summarise(
    total_rows = n(),
    # Check for missing Greeks
    missing_delta = sum(is.na(delta)),
    missing_gamma = sum(is.na(gamma)),
    missing_vega = sum(is.na(vega)),
    missing_theta = sum(is.na(theta)),
    missing_impl_vol = sum(is.na(impl_volatility)),
    # Check for data quality issues
    zero_bid = sum(best_bid == 0, na.rm = TRUE),
    bid_gte_ask = sum(best_bid >= best_offer, na.rm = TRUE),
    negative_volume = sum(volume < 0, na.rm = TRUE),
    # Percentage with complete Greeks
    pct_complete_greeks = round(100 * sum(!is.na(delta) & !is.na(gamma) & 
                                          !is.na(vega) & !is.na(theta)) / n(), 2)
  ) |>
  collect()

print("Data quality summary (after INNER JOIN):")
print(quality_check)

#==============================================================================#
# 7. RECOMMENDATION
#==============================================================================#

print("\n\n=============================================================================")
print("RECOMMENDATION")
print("=============================================================================")

if (pct_lost < 5) {
  print(paste("✓ INNER JOIN is recommended - only", pct_lost, "% of data is excluded"))
  print("  This ensures complete data for delta-hedged return calculations")
} else if (pct_lost < 15) {
  print(paste("⚠ INNER JOIN excludes", pct_lost, "% of data - consider investigating"))
  print("  Check if missing data is systematic or random")
} else {
  print(paste("✗ INNER JOIN excludes", pct_lost, "% of data - this may be too much"))
  print("  Consider using LEFT JOIN and handling NAs explicitly")
}

print("\nKey insights:")
print(paste("- Total option observations:", format(option_count$total_rows, big.mark = ",")))
print(paste("- Observations with complete data:", format(inner_join_data$total_rows, big.mark = ",")))
print(paste("- Data completeness:", 100 - pct_lost, "%"))