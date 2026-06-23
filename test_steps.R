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
sample_end <- as.Date("1996-01-31")  # Start with just January 1996 for testing

print("Testing step 1: Basic security data...")
security_full <- security_price |>
  filter(
    date >= sample_start,
    date <= sample_end
  ) |>
  select(secid, date, close, security_vol = volume)

test1 <- security_full |> head(5) |> collect()
print("Security data OK")
print(dim(test1))

print("Testing step 2: Basic option filtering...")
atm_options_selected <- option_price |>
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
  mutate(
    mid_quote = (best_bid + best_offer) / 2,
    strike_price = strike_price / 1000,
    days_to_maturity = as.numeric(exdate - date),
    bid_ask_spread = best_offer - best_bid,
    spread_ratio = bid_ask_spread / mid_quote
  ) |>
  filter(
    days_to_maturity > 30
  )

test2 <- atm_options_selected |> head(5) |> collect()
print("Basic option filtering OK")
print(dim(test2))

# Close connection
dbDisconnect(con)
