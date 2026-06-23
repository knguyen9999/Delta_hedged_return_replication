library(tidyverse)
library(duckdb)

# Connect to database
con <- dbConnect(
  duckdb(),
  dbdir = "delta_hedged_returns.duckdb",
  read_only = TRUE
)

#Create variable names for available data in duckdb
option_price <- tbl(con, "option_price")
security_price <- tbl(con, "security_price")
ff_data <- tbl(con, "ff_rates")
distribution_file <- tbl(con, "distribution_file")

#Filter data for the sample period (1996-2009 as in Cao-Han)
sample_start <- as.Date("1996-01-01")
sample_end <- as.Date("2009-11-01")

print("=== DATA SIZE ANALYSIS ===")

# Check total option data size
print("1. Total option data in sample period:")
option_count <- option_price |>
  filter(
    date >= sample_start,
    date <= sample_end
  ) |>
  summarise(total_rows = n()) |>
  collect()
print(paste("Total option observations:", format(option_count$total_rows, big.mark = ",")))

# Check after basic filtering
print("2. After basic quality filters:")
basic_filtered <- option_price |>
  filter(
    date >= sample_start,
    date <= sample_end,
    volume > 0,
    best_bid > 0,
    best_offer > best_bid,
    (best_bid + best_offer) / 2 >= 0.125,
    am_settlement == 0,
    !is.na(delta),
    as.numeric(exdate - date) > 30
  ) |>
  summarise(filtered_rows = n()) |>
  collect()
print(paste("After basic filters:", format(basic_filtered$filtered_rows, big.mark = ",")))

# Check unique securities
print("3. Unique securities:")
unique_secids <- option_price |>
  filter(
    date >= sample_start,
    date <= sample_end
  ) |>
  distinct(secid) |>
  summarise(unique_secids = n()) |>
  collect()
print(paste("Unique securities:", unique_secids$unique_secids))

# Check ATM selection (this is where it gets much smaller)
print("4. After ATM selection (most restrictive):")
print("   This step selects only 1 call + 1 put per stock-date...")

# Check a sample month to estimate
sample_month <- option_price |>
  filter(
    date >= as.Date("1996-01-01"),
    date <= as.Date("1996-01-31"),
    volume > 0,
    best_bid > 0,
    best_offer > best_bid,
    (best_bid + best_offer) / 2 >= 0.125,
    am_settlement == 0,
    !is.na(delta),
    as.numeric(exdate - date) > 30
  ) |>
  mutate(
    strike_price = strike_price / 1000,
    days_to_maturity = as.numeric(exdate - date),
    atm_distance = abs(strike_price - abs(close))
  ) |>
  group_by(secid, date, cp_flag) |>
  filter(days_to_maturity == min(days_to_maturity)) |>
  slice_min(atm_distance, n = 1, with_ties = FALSE) |>
  ungroup() |>
  summarise(sample_month_count = n()) |>
  collect()

estimated_annual <- sample_month_count$sample_month_count * 12
estimated_total <- estimated_annual * 14  # 14 years

print(paste("Sample month (Jan 1996) ATM options:", sample_month_count$sample_month_count))
print(paste("Estimated total ATM options (14 years):", format(estimated_total, big.mark = ",")))

# Memory estimation
print("5. Memory estimation:")
fields_per_row <- 20  # Approximate number of fields we'll keep
bytes_per_field <- 8  # Average bytes per field (mix of numbers, dates, strings)
estimated_bytes <- estimated_total * fields_per_row * bytes_per_field
estimated_mb <- estimated_bytes / (1024^2)
estimated_gb <- estimated_mb / 1024

print(paste("Estimated memory for final dataset:", round(estimated_mb, 1), "MB"))
print(paste("That's:", round(estimated_gb, 3), "GB"))

if (estimated_mb < 500) {
  print("✅ This should be fine for most laptops!")
} else if (estimated_mb < 2000) {
  print("⚠️  This might be challenging - monitor memory usage")
} else {
  print("❌ This is likely too large - consider chunking by year")
}

# Close connection
dbDisconnect(con)
