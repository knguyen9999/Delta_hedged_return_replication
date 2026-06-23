library(arrow)
library(tidyverse)
library(fs)
library(duckdb)
library(lubridate)

# Setting up the data path
setwd("/Users/kainguyen/Desktop/Paper_replication")
con <- dbConnect(duckdb(), dbdir = "delta_hedged_returns.duckdb", read_only = FALSE)

# Load all data first
option_price   <- tbl(con, "option_price")
security_price <- tbl(con, "security_price")
distribution_file_path <- "/Users/kainguyen/Desktop/Paper_replication/Distribution_File_2023-03-23.parquet"
distribution_file <- read_parquet(distribution_file_path)

#security data
security_price_clean <- security_price |>
  filter(
    date >= as.Date("1996-01-01"),
    date <= as.Date("2009-10-31")
  ) |>
  arrange(secid, date) |>
  select(secid, date, close, volume) |>
  mutate(close = abs(close)) |>
  collect()

#rf data
temp_zip <- tempfile(fileext = ".zip")
download.file(
  url = "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors_daily_CSV.zip",
  destfile = temp_zip,
  mode = "wb"
)
ff_data <- read_csv(unzip(temp_zip, exdir = tempdir()), col_types = cols(), skip = 3) |>
  janitor::clean_names() |>
  mutate(
    date = ymd(x1),
    rf_daily = rf / 100,
    rf_annual = rf_daily * 365
  ) |>
  select(date, rf_daily, rf_annual)
unlink(temp_zip)

#month-end dates
month_end_dates <- security_price_clean |>
  mutate(year_month = format(date, "%Y-%m")) |>
  group_by(year_month) |>
  summarise(month_end_date = max(date), .groups = "drop") |>
  pull(month_end_date)

#Load options with minimal filters
all_options <- option_price |>
  filter(
    date %in% month_end_dates,
    date >= as.Date("1996-01-01"),
    date <= as.Date("2009-10-31"),
    !is.na(delta),
    am_settlement == 0
  ) |>
  mutate(
    strike_price = strike_price / 1000,
    days_to_maturity = as.numeric(exdate - date),
    mid_quote = (best_bid + best_offer) / 2
  ) |>
  collect()

options_with_prices <- all_options |>
  left_join(
    security_price_clean,
    join_by(secid, closest(date >= date)),
    suffix = c("", "_stock")
  ) |>
  rename(
    stock_price_date = date_stock,
  )

options_with_rf <- options_with_prices |>
  left_join(
    ff_data |> rename(rf_date = date),
    join_by(closest(date >= rf_date))
  )

options_filtered <- options_with_rf |>
  filter(
    # Basic validity
    best_bid > 0,
    best_offer > best_bid,
    mid_quote >= 0.125,
    volume > 0,
    
    # Has required data
    !is.na(close),
    !is.na(rf_daily),
    
    # Maturity > 30 days
    days_to_maturity > 30
  ) |>
  mutate(
    moneyness = close / strike_price,
    # Time to maturity in years
    time_to_maturity_years = days_to_maturity / 365,
    
    # No-arbitrage bounds
    # Call: max(0, S - K*exp(-r*T)) <= C <= S
    call_lower_bound = pmax(0, close - strike_price * exp(-rf_annual * time_to_maturity_years)),
    call_upper_bound = close,
    
    # Put: max(0, K*exp(-r*T) - S) <= P <= K*exp(-r*T)
    put_lower_bound = pmax(0, strike_price * exp(-rf_annual * time_to_maturity_years) - close),
    put_upper_bound = strike_price * exp(-rf_annual * time_to_maturity_years),
    
    # Check violations
    violates_no_arbitrage = case_when(
      cp_flag == "C" & (mid_quote < call_lower_bound | mid_quote > call_upper_bound) ~ TRUE,
      cp_flag == "P" & (mid_quote < put_lower_bound | mid_quote > put_upper_bound) ~ TRUE,
      TRUE ~ FALSE
    )
  ) |>
  # Filter out violations
  filter(!violates_no_arbitrage) |>
  # Clean up temporary columns
  select(-contains("bound"), -violates_no_arbitrage, -time_to_maturity_years)

