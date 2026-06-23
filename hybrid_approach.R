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

print("=== HYBRID APPROACH: Minimal Materialization ===")

# Step 1: Only materialize the final selected options (this is small!)
sample_start <- as.Date("1996-01-01")
sample_end <- as.Date("2009-11-01")

# This is the complex part that causes SQL issues, so we materialize it
print("Materializing selected options...")
selected_options_final <- option_price |>
  filter(
    date >= sample_start,
    date <= sample_end,
    volume > 0,
    best_bid > 0,
    best_offer > best_bid,
    (best_bid + best_offer) / 2 >= 0.125,
    am_settlement == 0,
    !is.na(delta)
  ) |>
  # Create a simpler version first
  select(secid, date, exdate, cp_flag, optionid, best_bid, best_offer, 
         strike_price, delta, vega, volume) |>
  collect()  # This should be manageable

print(paste("Collected", nrow(selected_options_final), "options for further processing"))

# Now do the complex ATM selection in R (which is fast)
print("Processing ATM selection in R...")
selected_options_df <- selected_options_final |>
  mutate(
    mid_quote = (best_bid + best_offer) / 2,
    strike_price = strike_price / 1000,
    days_to_maturity = as.numeric(exdate - date),
    spread_ratio = (best_offer - best_bid) / mid_quote
  ) |>
  filter(days_to_maturity > 30) |>
  # ATM selection logic here...
  group_by(secid, date, cp_flag) |>
  slice_min(days_to_maturity, n = 1) |>  # Simplified for now
  ungroup()

print(paste("Final selected options:", nrow(selected_options_df)))

# Step 2: Keep other operations lazy but simple
print("Building lazy queries for daily data...")

# This stays lazy - just simple joins and filters
all_option_daily <- option_price |>
  inner_join(
    selected_options_df |> select(optionid), 
    by = "optionid"
  ) |>
  select(optionid, date, best_bid, best_offer, delta, vega, strike_price)

print("Testing if this approach works...")
test_result <- all_option_daily |> head(100) |> collect()
print(paste("✅ Success! Got", nrow(test_result), "test observations"))

# Close connection
dbDisconnect(con)
