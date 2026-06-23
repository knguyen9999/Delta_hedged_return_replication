library(arrow)
library(tidyverse)
library(fs)
library(duckdb)

#Setting up the data path
setwd("/Users/kainguyen/Desktop/Delta_Hedges_Research")

# Use DuckDB wild-cards instead of dir_ls() vectors
file_path_option <- dir_ls(regexp = "^Option_Price.*.parquet$")
file_path_security <- dir_ls(regexp = "^Security_Price.*.parquet$")

option_price_ds   <- open_dataset(file_path_option,   format = "parquet")
security_price_ds <- open_dataset(file_path_security, format = "parquet")

# Create new DuckDB database for delta-hedged returns analysis
con <- dbConnect(duckdb(), dbdir = "delta_hedged_returns.duckdb", read_only = FALSE)
dbDisconnect(con)

# Arrow -> temporary *views*
option_price_view   <- option_price_ds   |> to_duckdb(con, "option_price_view")
security_price_view <- security_price_ds |> to_duckdb(con, "security_price_view")

#Materialize parquet files as tables
#Physical table for option price
dbExecute(con, "
  CREATE OR REPLACE TABLE option_price AS
  SELECT * FROM option_price_view;
")
#Physical table for security price
dbExecute(con, "
  CREATE OR REPLACE TABLE security_price AS
  SELECT * FROM security_price_view;
")
dbListTables(con)

#Point dplyr at the tables in duckdb
option_price   <- tbl(con, "option_price")
security_price <- tbl(con, "security_price")

#Take a look at the sample
option_price |> head() |> collect()
security_price |> head() |> collect()

#Look at the smallest price of each stock
# option_price |> slice_min(strike_price, by = c(secid))
# View(option_price)

#SECURITY_DATA======================================
#Trim the security price dataset
security_price <- security_price |>
  select(secid, date, close) |> 
  rename(security_price = close)
  #collect() #security still a lazy data, hence collecting in this part would
  #cause problem to merge

#OPTION_DATA======================================
#Construction of option panel
opt_data <- option_price |> 
  mutate(
    mid_quote = (best_bid + best_offer) / 2, #Cao & Han methodology
    strike_price = strike_price / 1000, #Convert to dollars
    days_to_maturity = as.integer(exdate - date)) |> #TTM calculation
  filter(
    !is.na(delta),
    am_settlement == 0, #PM settlement only
    ss_flag == 0, #Standard settlement only
    best_bid > 0,                      
    best_bid < best_offer, #Valid spreads
    volume > 0, #Traded options only
    mid_quote >= 1/8, #minimum price filter (0.125)
    days_to_maturity >= 30,
    days_to_maturity <= 90#TTM requirement
  ) |> 
  left_join(security_price, 
    by = c("secid", "date")) |> 
  select(optionid, secid, date, delta, gamma, strike_price, cp_flag, exdate, mid_quote, days_to_maturity, security_price) |> 
  #So apparently what's happening with this code is that the slice min will select
  #the 1 row of C and P for that date where the strike is CLOSEST to the underlying
  #stock, which is a core feature of Cao & Han's paper. It's meant to measure how
  #ATM an option is. A smaller value means the option is closer to being at-the-money.
  slice_min(
    abs(strike_price - security_price),
    by = c("secid", "date", "cp_flag")
  ) |> 
  collect() # collect the data, now small

#RISK-FREE_DATA======================================
#Working on the risk-free dataset
file_path_zero_curve <- "Zero_Curve_File_2023-03-23.parquet"
zero_curve_data <- read_parquet(file_path_zero_curve) # read locally directlry, for it is small

#Create new rf-rate by converting rate from % to decimal. Then remove the og rate column by setting it to NULL
zero_curve_data <- zero_curve_data |>
  mutate(
    rf_rate = rate / 100,
    rate = NULL
  )

#FINAL-NEAT_DATA==============================================
# rolling join (closest to days_to_maturity) Think about why I used days_to_maturity <= days
#So basically what's happening is that for each option (Dr.Matthew calculated the TTM), 
#find the closest available interest rate (rf_rate) in the zero curve on the same date,
#but only where days_to_maturity <= days in the curve.
opt_data <- opt_data |> 
  left_join(zero_curve_data,
    by = join_by(
      date,
      closest(days_to_maturity <= days))
    )

# #I forgot to account for the S/K between 0.8 and 1.2 mentioned in the paper
# opt_data = opt_data |>
#   mutate(
#     moneyness = security_price / strike_price
#   ) |>
#   filter(
#     between(moneyness, 0.8, 1.2)
#   )

# no aribtrage bounds (zero and intrinscic value bound)
#This was mentioned in section 2.1 in the paper. 
opt_date <- opt_data |> 
  mutate(
    #C Upper bound: A call option gives the right to buy the underlying stock. Therefore, its price
    #can never exceed the price of the stock itself. If it did, one could sell the expensive
    #call and buy the cheaper stock for an immediate risk-free profit. => S >= C
    #C Lower bound: The price of a call option must be at least its "intrinsic value."
    #If the price fell below the difference in the current stock minus the present value of the strike price,
    #an arbitrageur could buy the cheap call, sell the stock short, and invest the proceeds to earn a risk-free profits.
    call_lower_bound = pmax(0, security_price - strike_price * exp(-rf_rate * (days_to_maturity / 365))), #The exp(...) represent the present value
    call_upper_bound = security_price,
    #P Upper bound: A put option gives the right to sell the underlying at strike price => the most the option can be worth
    #is the strike price, which would happen if the stock price fell to 0.
    #P lower bound: Similar to C lower bound, the price of the put option must be at least intrinsic value.
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


#THE CALCULATION==============================================

#SECTION 2 OF THE PAPER
#In the delta-hedging formula, we need to compare the today's value with prev.
#Hence, create lag() columns to not have to call it in all the calculation agian.
#Group by optionid so the lag() won't go across different options.

#2.1: Create the lag columns for easy calc
opt_date_lagged = opt_date |> 
  arrange(optionid, date) |> 
  group_by(optionid) |> 
  mutate(
    #Only lag for key values
    prev_day_date = lag(date),
    prev_day_price_S = lag(security_price),
    prev_day_price_C = lag(mid_quote),
    prev_day_delta = lag(delta),
    prev_day_rf = lag(rf_rate),
    #Since we're using 365 days calendar, account for weekends and calendar gaps
    #in trading days
    days_elapsed = as.numeric(date - prev_day_date)
  ) |> 
  ungroup()

#2.2: Calc for daily component
opt_daily_pnl = opt_date_lagged |> 
  mutate(
    #1: The middle component of the equation
    #Formula: - delta_C,t_n * [S(t_n+1) − S(t_n)]
    pnl_stock_leg = -prev_day_delta * (security_price - prev_day_price_S),
    
    #2: The interest component
    #Formula: - (a_n*rf)/365 * [C_t_n - delta_C,t_n * S_t_n]
    pnl_interest_leg = - (days_elapsed * prev_day_rf/365) * (prev_day_price_C - (prev_day_delta * prev_day_price_S)),
  )

#2.3: Define the monthly holding periods and aggregate returns
#Create start_month to identify the observations
opt_monthly = opt_daily_pnl |> 
  #I want to get all options on the same calendar month.
  #Let's ssay that if the option was traded on 01-15,
  #then it'll fall under 2021-01-01 aka Jan group
  mutate(start_month = floor_date(date, "month")) |> 
  group_by(optionid, secid, cp_flag, start_month) |> 
  summarise(
    #count observations in the month
    n_obs = n(),
    #Suum of daily components in the equation
    total_stock_pnl = sum(pnl_stock_leg, na.rm= TRUE),
    total_interest_pnl = sum(pnl_interest_leg, na.rm = TRUE),
    #The change in option value
    C_initial = first(mid_quote),
    C_final = last(mid_quote),
    pnl_option_leg = C_final - C_initial,
    #Initial value of price and delta
    S_initial = first(security_price),
    delta_initial = first(delta),
    #Additional info for analysis
    #The initial and final dates to track the time span of each monthly holding periods
    date_initial = first(date),
    date_final = last(date),
    #This is to know how far from the expiration the option was at the start
    days_to_maturity_initial = first(days_to_maturity),
    strike_price = first(strike_price),
    
    .groups = "drop"
  ) |>
  #With the options sorted out, I want to remove any n_obs with fewer the 10.
  #The reason is because we want to look at meaningful holding periods.
  #At least 10 trading days in the month meaning that we're able to
  #rebalance the option effectively and avoid any options that are
  #1) listed near-end month, 2) expired soon, 3) rarely-traded options,...
  filter(
    # n_obs >= 10, #To be checked
    !is.na(C_initial),
    !is.na(C_final)
  )

#2.4: Final Delta-Hedged Return
delta_hedged_returns = opt_monthly |> 
  mutate(
    #Calc the total gain - all components
    total_gain = pnl_option_leg + total_stock_pnl + total_interest_pnl,
    #Calc scaling factor for calls or puts
    scaling_factor = abs(delta_initial * S_initial - C_initial),
    #Delta-Hedged Return
    delta_hedged_return = total_gain / scaling_factor,
    #Additional metrics
    holding_days = as.numeric(date_final - date_initial),
    moneyness = S_initial / strike_price,
    #Option return
    option_return = total_gain /C_initial
  ) |> 
  select(
    optionid,
    secid,
    cp_flag,
    start_month,
    delta_hedged_return,
    option_return,
    holding_days,
    n_obs,
    moneyness,
    delta_initial,
    days_to_maturity_initial,
    total_gain,
    scaling_factor
  )

#DELTA-HEDGED RETURN STAT SUMMARY====================================
# summary_stats <- delta_hedged_returns |>
#   group_by(cp_flag) |>
#   summarise(
#     n_options = n(),
#     mean_delta_hedged = mean(delta_hedged_return, na.rm = TRUE),
#     median_delta_hedged = median(delta_hedged_return, na.rm = TRUE),
#     sd_delta_hedged = sd(delta_hedged_return, na.rm = TRUE),
#     mean_opt_return = mean(option_return, na.rm = TRUE),
#     median_opt_return = median(option_return, na.rm = TRUE),
#     sd_opt_return = sd(option_return, na.rm = TRUE),
#     pct_negative_dh = mean(delta_hedged_return < 0, na.rm = TRUE) * 100,
#     .groups = "drop"
#   )
# 
# #To look at the average for each option type in each month
# monthly_avg_returns <- delta_hedged_returns |>
#   group_by(start_month, cp_flag) |>
#   summarise(
#     n = n(),
#     avg_delta_hedged = mean(delta_hedged_return, na.rm = TRUE),
#     avg_return = mean(option_return, na.rm = TRUE),
#     .groups = "drop"
#   )
# 
# # Moneyness analysis
# moneyness_analysis <- delta_hedged_returns |>
#   mutate(
#     moneyness_bucket = cut(
#       moneyness, 
#       breaks = c(0, 0.95, 0.98, 1.02, 1.05, Inf),
#       labels = c("Deep OTM", "OTM", "ATM", "ITM", "Deep ITM")
#     )
#   ) |>
#   group_by(cp_flag, moneyness_bucket) |>
#   summarise(
#     n = n(),
#     mean_dlt_hedged_return = mean(delta_hedged_return, na.rm = TRUE),
#     median_dlt_hedged_return = median(delta_hedged_return, na.rm = TRUE),
#     sd_dlt_hedged_return = sd(delta_hedged_return, na.rm = TRUE),
#     pct_negative = mean(delta_hedged_return < 0, na.rm = TRUE) * 100,
#     .groups = "drop"
#   ) |>
#   filter(!is.na(moneyness_bucket))

#Try to replicate Table 1 in the research paper
#First, get the raw option data with all needed variables
full_data <- delta_hedged_returns |>
  mutate(
    #Convert moneyness to percentage
    moneyness_pct = moneyness * 100,
    #Scale returns to percentage
    delta_hedged_return_pct = delta_hedged_return * 100,
    #Create scaled gain for display
    delta_hedged_gain_scaled = total_gain / scaling_factor * 100,
  )

#Create summary statistics function
create_summary_stats <- function(data, variable, var_name) {
  data |>
    summarise(
      Variable = var_name,
      Mean = round(mean({{variable}}, na.rm = TRUE), 2),
      Median = round(median({{variable}}, na.rm = TRUE), 2),
      StdDev = round(sd({{variable}}, na.rm = TRUE), 2),
      `10th_Pct` = round(quantile({{variable}}, 0.10, na.rm = TRUE), 2),
      `25th_Pct` = round(quantile({{variable}}, 0.25, na.rm = TRUE), 2),
      `75th_Pct` = round(quantile({{variable}}, 0.75, na.rm = TRUE), 2),
      `90th_Pct` = round(quantile({{variable}}, 0.90, na.rm = TRUE), 2),
      .groups = "drop"
    )
}

#Panel A: Call options
panel_a_stats <- full_data |>
  filter(cp_flag == "C") |>
  summarise(
    n_obs = n(),
    .groups = "drop"
  )
panel_a_table <- bind_rows(
  full_data |> filter(cp_flag == "C") |> 
    create_summary_stats(delta_hedged_gain_scaled, "Delta-hedged gain until month-end/(ΔS-C) (%)"),
  full_data |> filter(cp_flag == "C") |> 
    create_summary_stats(days_to_maturity_initial, "Days to maturity"),
  full_data |> filter(cp_flag == "C") |> 
    create_summary_stats(moneyness_pct, "Moneyness = S/K (%)"),
)

#Panel B: Put options
panel_b_stats <- full_data |>
  filter(cp_flag == "P") |>
  summarise(
    n_obs = n(),
    .groups = "drop"
  )
panel_b_table <- bind_rows(
  full_data |> filter(cp_flag == "P") |> 
    create_summary_stats(delta_hedged_gain_scaled, "Delta-hedged gain until month-end/(P-ΔS) (%)"),
  full_data |> filter(cp_flag == "P") |> 
    create_summary_stats(days_to_maturity_initial, "Days to maturity"),
  full_data |> filter(cp_flag == "P") |> 
    create_summary_stats(moneyness_pct, "Moneyness = S/K (%)"),
)
