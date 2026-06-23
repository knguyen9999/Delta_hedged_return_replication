library(arrow)
library(tidyverse)
library(fs)
library(duckdb)

file_path_option <- dir_ls(regexp = "^Option_Price.*.parquet$")
file_path_security <- dir_ls(regexp = "^Security_Price.*.parquet$")


option_price <- open_dataset(file_path_option) |> to_duckdb()
security_price <- open_dataset(file_path_security) |> to_duckdb()

op_header <- option_price |> head() |> collect()
sec_header <- security_price |> head() |> collect()

option_price |> slice_min(strike_price, by = c(secid))

security_price <- security_price |>
  select(secid, date, close) |> 
  rename(security_price = close)

opt_data <- option_price |> 
  mutate(
    mid_quote = (best_bid + best_offer) / 2, 
    strike_price = strike_price / 1000,
    days_to_maturity = as.integer(exdate - date)) |>  
  filter(
    !is.na(delta),
    am_settlement == 0,
    ss_flag == 0,
    best_bid > 0,                      
    best_bid < best_offer,
    volume > 0,
    mid_quote >= 1/8, 
    days_to_maturity >= 0, # sanity check
  ) |> 
  left_join(security_price, 
    by = c("secid", "date")) |> 
  select(optionid, secid, date, delta, gamma, strike_price, cp_flag, exdate, mid_quote, days_to_maturity, security_price) |> 
  slice_min(
    abs(strike_price - security_price),
    by = c("secid", "date", "cp_flag")
  ) |> 
  collect() # collect the data, now small


file_path_zero_curve <- "Zero_Curve_File_2023-03-23.parquet"
zero_curve_data <- read_parquet(file_path_zero_curve) # read locally directlry, for it is small

zero_curve_data <- zero_curve_data |>
  mutate(
    rf_rate = rate / 100,
    rate = NULL
  )

# rolling join (closest to days_to_maturity) Think about why I used days_to_maturity <= days
opt_data <- opt_data |> 
  left_join(zero_curve_data,
    by = join_by(
      date,
      closest(days_to_maturity <= days))
    )

# no aribtrage bounds (zero and intrinscic value bound)
opt_date <- opt_data |> 
  mutate(
    call_lower_bound = pmax(0, security_price - strike_price * exp(-rf_rate * (days_to_maturity / 365))),
    call_upper_bound = security_price,
    put_upper_bound = strike_price * exp(-rf_rate * (days_to_maturity / 365)),
    put_lower_bound = pmax(0,strike_price * exp(-rf_rate * (days_to_maturity / 365)) - security_price),
    no_arb_cond = 
      ifelse(cp_flag == "C",
        ifelse( (mid_quote >= call_lower_bound) & (mid_quote <= call_upper_bound), TRUE, FALSE), # check call no-arb condition
        ifelse( (mid_quote >= put_lower_bound) & (mid_quote <= put_upper_bound) , TRUE, FALSE) # check put no-arb condition
      )
    ) |> 
  filter(no_arb_cond == TRUE) |> 
  select(!call_lower_bound:no_arb_cond)








# Step 1: Backward-Looking Option Price Difference
filtered_arbitrage_data <- filtered_arbitrage_data |> 
  group_by(secid, exdate) |>  # Group by security and expiration date
  mutate(
    option_price_diff = mid_price - lag(mid_price, n = 7)  # Backward-looking for 7-day holding period
  ) |> 
  ungroup()

# Display the first few rows to confirm
head(filtered_arbitrage_data)







library(tidyr)

# Step 2: Handle NA values and calculate Stock Return Component
filtered_arbitrage_data <- filtered_arbitrage_data |> 
  group_by(secid, exdate) |>  # Group by security and expiration date
  mutate(
    log_stock_return = log(close) - lag(log(close)),  # Daily log return
    log_stock_return = replace_na(log_stock_return, 0),  # Replace NA with 0
    delta = replace_na(delta, 0),                      # Replace NA in delta with 0
    stock_return_component = -delta * log_stock_return,  # Delta-adjusted return
    cumulative_stock_return = cumsum(stock_return_component)  # Cumulative sum
  ) |> 
  ungroup()

# Display the first few rows to confirm
head(filtered_arbitrage_data)


# Step 3: Financing Cost Component without filling missing dates
filtered_arbitrage_data <- filtered_arbitrage_data |> 
  arrange(secid, date) |>  # Ensure proper ordering
  group_by(secid, exdate) |>  # Group by security and expiration date
  mutate(
    an = as.numeric(lead(date) - date, units = "days"),  # Rebalancing interval (from actual data)
    portfolio_value = mid_price - delta * close,  # Calculate portfolio value
    daily_financing_cost = (an * rate / 365) * portfolio_value,  # Financing cost
    cumulative_financing_cost = cumsum(daily_financing_cost)  # Cumulative financing cost
  ) |> 
  ungroup()

# Drop rows where critical values are NA
filtered_arbitrage_data <- filtered_arbitrage_data |> 
  filter(!is.na(daily_financing_cost), !is.na(cumulative_financing_cost))

# Display the first few rows
head(filtered_arbitrage_data)




# Final Delta-Hedged Option Gain
delta_hedged_results <- filtered_arbitrage_data |> 
  group_by(secid, exdate) |> 
  mutate(
    delta_hedged_gain = (lead(mid_price) - mid_price) -  # Option price difference
      cumulative_stock_return -                         # Stock return component
      cumulative_financing_cost                         # Financing cost component
  ) |> 
  ungroup()

# Display the first few rows for verification
head(delta_hedged_results)

# Calculate the scaled delta-hedged option return
scaled_data <- delta_hedged_results |> 
  mutate(
    scaling_factor = (delta * close - mid_price),  # Compute the scaling factor
    scaled_delta_hedged_return = delta_hedged_gain / scaling_factor  # Compute the scaled return
  )

# Display the first few rows to confirm
head(scaled_data)
# Save the scaled results for the 7-day interval
write.csv(scaled_data, "delta_hedged_return_7_days.csv", row.names = FALSE)
