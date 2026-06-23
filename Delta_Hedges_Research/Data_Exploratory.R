library(tidyverse)
library(dplyr)
library(arrow)
library(lobstr)
library(duckdb)


#Accessing the options file
file_path = "/Users/kainguyen/Desktop/Delta_Hedges_Research/Option_Price2021_2023-03-29.parquet"
option_price_23 = open_dataset(file_path) #open_dataset() function introduced by Neal
#schema(option_price_23)

#THIS CHUNK IS FOR TESTING AND LOOKING AT SAMPLE DATA INITIAL
#================================================================#
# #Look at data dimension
# option_price_23 |> obj_size()
# option_price_23 |> nrow()
# option_price_23 |> 
#   summarise(
#     total_rows = n(),
#     min_date = min(date, na.rm = TRUE),
#     max_date = max(date, na.rm = TRUE),
#     unique_securities = n_distinct(secid),
#     unique_symbols = n_distinct(symbol)
#   ) |> 
#   collect()
# 
# #Examine a small amount of data to see what I'm dealing with
# sample_data = option_price_23 |> 
#   slice_sample(n = 500) |> #I'm choosing random sample to see how the data looks like
#   collect()
# glimpse(sample_data)
# head(sample_data)
# 
# sample_data |> #I'm checking to see the quality of the sample data
#   summarise(
#     across(c(best_bid, best_offer, strike_price, impl_volatility, delta),
#            list(missing = ~sum(is.na(.)),
#                 zeros = ~sum(. == 0, na.rm = TRUE))),
#     .groups = "drop"
#   )
#================================================================#

setwd("/Users/kainguyen/Desktop/Delta_Hedges_Research")

#Set up DuckDB. I'm creating a dbdir aka a persistent database file on disk.
#I think this's not a permanent approach but it's a great method
#during this instant since it allows me to make collected data survive
#the R session restarts.
con = dbConnect(duckdb(), dbdir = "option_prices.duckdb", read_only = FALSE)

#I learned this method of connecting duckdb from LatinR YT channel. Link: 'https://www.youtube.com/watch?v=yp5Q85geHF0'
dbExecute(
  con,
  "CREATE TABLE option_prices_file AS SELECT * FROM read_parquet('/Users/kainguyen/Desktop/Delta_Hedges_Research/Option_Price2021_2023-03-29.parquet')"
)
dbListTables(con)

#Checking the columns name...
columns = dbListFields(con, "option_prices_file")
columns

#THIS CHUNK IS FOR TESTING THE DUCKDB DATA COLLECTION
#================================================================#
#Messing and testing around with the data collection method...
bid_ask_spread = tbl(con, "option_prices_file") |>
  summarise(avg_spread = mean(best_offer - best_bid, na.rm = TRUE),
            median_spread = quantile(best_offer - best_bid, 0.5, na.rm = TRUE),
            n_observations = n(),
            .by = symbol) |>
  collect()

dbWriteTable(con, "bid_ask_spread", bid_ask_spread) #Create another table in my duck dtbase
dbListTables(con) #Checked and verified the new table is created
print(tbl(con, "bid_ask_spread"), n = 100) #Checking up to 100 listed

con = dbConnect(duckdb(), 'option_prices.duckdb') #I'm disconnecting and reconnecting to check if things stayed
dbListTables(con)
rm(bid_ask_spread) #removing unused data
gc()
#================================================================#

#Create another dataset with security prices
dbExecute(
  con,
  "CREATE TABLE security_prices_file AS SELECT * FROM read_parquet('/Users/kainguyen/Desktop/Delta_Hedges_Research/Security_Price2021_2023-03-23.parquet')"
)
dbListFields(con, 'security_prices_file')

#Create the dataset for zero-curve contains zero-coupon interest rate curve used
dbExecute(
  con,
  "CREATE TABLE zero_curve_file AS SELECT * FROM read_parquet('/Users/kainguyen/Desktop/Delta_Hedges_Research/Zero_Curve_File_2023-03-23.parquet')"
)
dbListFields(con, 'zero_curve_file')

#================================================================#
#In order to join the datasets, 1st of all I want to clean each data individually.
#I'm calling this step SAC (Step A of Cleaning). In SAC, we have 3 sub-steps.

#SAC: STEP 1: CLEAN SECURITY PRICE DATA
#Check the data 1st
security_info = tbl(con, "security_prices_file") |> 
  summarise(
    total_rows = n(),
    unique_securities = n_distinct(secid),
    min_date = min(date, na.rm = TRUE),
    max_date = max(date, na.rm = TRUE),
    date_range_days = max(date, na.rm = TRUE) - min(date, na.rm = TRUE)
  ) |> 
  collect()
security_info 
#Check for and handle any missing stock_price values. 
#Given that a missing stock price makes option analysis impossible for that day, 
#the safest strategy is to drop these rows.
security_missing_check <- tbl(con, "security_prices_file") |>
  summarise(
    missing_secid = sum(case_when(is.na(secid) ~ 1, TRUE ~ 0)),
    missing_date = sum(case_when(is.na(date) ~ 1, TRUE ~ 0)),
    missing_close = sum(case_when(is.na(close) ~ 1, TRUE ~ 0)),
    zero_close = sum(case_when(close <= 0 ~ 1, TRUE ~ 0)),
    missing_shrout = sum(case_when(is.na(shrout) ~ 1, TRUE ~ 0))
  ) |>
  collect()
security_missing_check
#Ensure that the combination of secid and date is unique. Duplicate can exist because of 
#multiple listings for the same security on the same day
security_duplicate_check <- tbl(con, "security_prices_file") |>
  group_by(secid, date) |>
  summarise(count = n(), .groups = "drop") |>
  filter(count > 1) |>
  summarise(
    duplicate_combinations = n(),
    max_duplicates = max(count, na.rm = TRUE)
  ) |>
  collect()
security_duplicate_check
#Look at a sample
tbl(con, "security_prices_file") |>
  select(secid, date, close, shrout) |>
  head(10)

#Create the clean security dataset and send it to duckdb
security_prices_clean <- tbl(con, "security_prices_file") |>
  filter(
    !is.na(secid),           
    !is.na(date),
    !is.na(close),
    close > 0
  ) |>
  select(
    secid,
    date,
    stock_price = close,
    shrout #I'm keeping this in case it needs to be used for market cap
  ) |>
  collect()
dbWriteTable(con, "security_prices_clean", security_prices_clean, overwrite = TRUE)
# rm(security_prices_clean)
# gc()

#SAC: STEP 2: CLEAN ZERO-CURVE DATA
#Check the info
zero_curve_info <- tbl(con, "zero_curve_file") |>
  summarise(
    total_rows = n(),
    unique_dates = n_distinct(date),
    unique_maturities = n_distinct(days),
    min_date = min(date, na.rm = TRUE),
    max_date = max(date, na.rm = TRUE),
    min_days = min(days, na.rm = TRUE),
    max_days = max(days, na.rm = TRUE),
    min_rate = min(rate, na.rm = TRUE),
    max_rate = max(rate, na.rm = TRUE),
    avg_rate = mean(rate, na.rm = TRUE)
  )
zero_curve_info
zero_curve_missing_check <- tbl(con, "zero_curve_file") |>
  summarise(
    missing_date = sum(case_when(is.na(date) ~ 1, TRUE ~ 0)),
    missing_days = sum(case_when(is.na(days) ~ 1, TRUE ~ 0)),
    missing_rate = sum(case_when(is.na(rate) ~ 1, TRUE ~ 0))
  )
zero_curve_missing_check
curve_structure <- tbl(con, "zero_curve_file") |>
  group_by(date) |>
  summarise(
    maturities_per_date = n(),
    min_maturity = min(days, na.rm = TRUE),
    max_maturity = max(days, na.rm = TRUE),
    .groups = "drop"
  ) |>
  summarise(
    avg_maturities_per_date = mean(maturities_per_date),
    min_maturities_per_date = min(maturities_per_date),
    max_maturities_per_date = max(maturities_per_date)
  )
curve_structure
#Look at sample
sample_zero_curve <- tbl(con, "zero_curve_file") |>
  head(10)
sample_zero_curve

#Create a clean zero-curve dataset and send it do duckdb
zero_curve_clean <- tbl(con, "zero_curve_file") |>
  filter(
    !is.na(date),
    !is.na(days),
    !is.na(rate),
    days > 0
  ) |> 
  mutate(
    #From the sample, seems like rate is in % format => I'm converting it to ratio
    rate_decimal = rate/100
  ) |> 
  select(
    date, days, rate_decimal
  ) |> 
  arrange(date, days) |> 
  collect()
dbWriteTable(con, "zero_curve_clean", zero_curve_clean, overwrite = TRUE)
# rm(zero_curve_clean)
# gc()

#SAC: STEP 3: CLEAN OPTION PRICE DATA
option_info <- tbl(con, "option_prices_file") |>
  summarise(
    total_rows = n(),
    unique_securities = n_distinct(secid),
    unique_options = n_distinct(optionid),
    min_date = min(date, na.rm = TRUE),
    max_date = max(date, na.rm = TRUE),
    call_count = sum(case_when(cp_flag == "C" ~ 1, TRUE ~ 0)),
    put_count = sum(case_when(cp_flag == "P" ~ 1, TRUE ~ 0))
  ) 
option_info

option_missing_check <- tbl(con, "option_prices_file") |>
  summarise(
    missing_secid = sum(case_when(is.na(secid) ~ 1, TRUE ~ 0)),
    missing_date = sum(case_when(is.na(date) ~ 1, TRUE ~ 0)),
    missing_exdate = sum(case_when(is.na(exdate) ~ 1, TRUE ~ 0)),
    missing_best_bid = sum(case_when(is.na(best_bid) ~ 1, TRUE ~ 0)),
    missing_best_offer = sum(case_when(is.na(best_offer) ~ 1, TRUE ~ 0)),
    missing_delta = sum(case_when(is.na(delta) ~ 1, TRUE ~ 0)),
    missing_impl_vol = sum(case_when(is.na(impl_volatility) ~ 1, TRUE ~ 0)),
    zero_volume = sum(case_when(volume == 0 ~ 1, TRUE ~ 0)),
    invalid_bids = sum(case_when(best_bid <= 0 ~ 1, TRUE ~ 0)),
    invalid_spreads = sum(case_when(best_bid >= best_offer ~ 1, TRUE ~ 0))
  )
option_missing_check

sample_options <- tbl(con, "option_prices_file") |>
  select(secid, date, exdate, cp_flag, strike_price, best_bid, best_offer, 
         volume, delta, impl_volatility, optionid) |>
  head(10)
sample_options

#Clean the option price dataset and send it to duckdb. I'm following the direction of Cao & Han paper
options_clean <- tbl(con, "option_prices_file") |>
  filter(
    #Remove rows with missing data
    !is.na(secid),
    !is.na(date),
    !is.na(exdate),
    !is.na(best_bid),
    !is.na(best_offer),
    !is.na(delta),
    !is.na(impl_volatility),
    
    #Initial Quality Filters (based on Cao and Han, 2013)
    volume > 0, #keep only rows where volume > 0
    best_bid > 0, #Ensure best_bid > 0
    best_offer > best_bid #Remove data errors where best_bid >= best_offer
  ) |> 
  mutate(
    #calculate the midpoint like in the paper
    option_price = (best_bid + best_offer) / 2,
    #calculate time to maturity
    TTM_days = as.integer(exdate - date),
    #additional calculation
    bid_ask_spread = best_offer - best_bid,
    relative_spread = bid_ask_spread / option_price,
    strike_dollars = strike_price / 1000 #convert cents to dollar
  ) |> 
  filter(
    option_price >= 0.125, #I keep only options where option_price is at least $1/8
    TTM_days > 0, #TTM has to > than 0
    impl_volatility > 0 #invalid sigma
  ) |> 
  select(
    secid,
    date,
    exdate, 
    optionid,
    cp_flag,
    strike_dollars, #above strike dollar calculated
    TTM_days, #from above
    option_price,#from above
    best_bid,
    best_offer, 
    bid_ask_spread,#fr abv
    relative_spread, #fr abv
    delta,
    gamma,
    vega,
    theta,
    impl_volatility,
    volume,
    open_interest
  ) |> 
  arrange(secid, date, optionid) |> 
  collect()
dbWriteTable(con, "options_clean", options_clean, overwrite = TRUE)

#================================================================#
# quick_look = tbl(con, "options_clean") |> 
#   summarise(
#     clean_rows = n(),
#     avg_option_price = mean(option_price, na.rm = TRUE),
#     min_TTM = min(TTM_days, na.rm = TRUE),
#     max_TTM = max(TTM_days, na.rm = TRUE),
#     avg_TTM = mean(TTM_days, na.rm = TRUE)
#   )
# quick_look
#================================================================#


#Clean up
dbDisconnect(con)


#EXAMPLE OF SLICE_MIN
# ── 1.  Toy option panel ─────────────────────────────────────────────
opt_panel <- tibble(
  secid          = c(101, 101, 101, 101, 202, 202, 202, 202),
  date           = as.Date(c("2025-06-10", "2025-06-10",
                             "2025-06-11", "2025-06-11",
                             "2025-06-10", "2025-06-10",
                             "2025-06-11", "2025-06-11")),
  cp_flag        = c("C",  "C",  "C",  "P",  "C",  "P",  "C",  "P"),
  strike_price   = c(98,   105,  100,  90,   50,   55,   50,   60),
  security_price = c(100,  100,  101, 101,  52,   52,   51,   51),
  optionid       = 1:8
)
atm_contracts <- opt_panel |> 
  slice_min( abs(strike_price - security_price),
             by = c(secid, date, cp_flag),
             n = 1,           # default; keep 1 row
             with_ties = FALSE)  # drop ties deterministically
atm_contracts

# ── FAKE OPTION QUOTES ──
demo_options <- tibble(
  opt_secid = c(1, 1, 1, 2, 3),
  opt_date  = as.Date(c("2025-06-01", "2025-06-01", "2025-06-01", "2025-06-01", "2025-06-01")),
  strike_price = c(95, 100, 105, 50, 60),
  cp_flag = c("C", "C", "C", "P", "C"),
  optionid = 101:105
)

# ── FAKE SECURITY PRICES ──
demo_stock_prices <- tibble(
  sec_secid = c(1, 2),
  sec_date  = as.Date(c("2025-06-01", "2025-06-01")),
  close_price = c(102, 51)
)

# ── JOIN: match stock price into each option row ──
demo_joined <- demo_options |>
  left_join(
    demo_stock_prices,
    by = c("opt_secid" = "sec_secid", "opt_date" = "sec_date")
  )

# View the joined result
print(demo_joined)

# ── SLICE: pick 1 option closest to ATM per (opt_secid, opt_date, cp_flag) ──
demo_atm <- demo_joined |>
  slice_min(
    abs(strike_price - close_price),
    by = c(opt_secid, opt_date, cp_flag),
    n = 1,
    with_ties = FALSE
  )

# View the final result: closest-to-ATM per group
print(demo_atm)



# Options to price
demo_opt <- tibble(
  date = as.Date("2025-06-01"),
  optionid = 1:4,
  days_to_maturity = c(15, 45, 90, 300)
)

# Zero curve: days and corresponding rates
demo_curve <- tibble(
  date = as.Date("2025-06-01"),
  days = c(30, 60, 180, 365),
  rf_rate = c(0.02, 0.025, 0.03, 0.035)
)

# Rolling join: find rate where days_to_maturity <= days
result <- demo_opt |>
  left_join(
    demo_curve,
    by = join_by(
      date,
      closest(days_to_maturity <= days)
    )
  )

print(result)
