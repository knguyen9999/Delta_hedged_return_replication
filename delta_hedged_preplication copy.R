library(arrow)
library(tidyverse)
library(fs)
library(duckdb)

#Setting up the data path
setwd("/Users/kainguyen/Desktop/Paper_replication")

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
option_price |> head()

#Look at the smallest price of each stock
# option_price |> slice_min(strike_price, by = c(secid))
# View(option_price)

#SECURITY_DATA======================================
#Trim the security price dataset
security_price <- security_price |>
  select(secid, date, close) |> 
  mutate(security_price = abs(close))

#RISK-FREE_DATA======================================
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
#Working on the risk-free dataset
file_path_zero_curve <- "Zero_Curve_File_2023-03-23.parquet"
zero_curve_data <- read_parquet(file_path_zero_curve) # read locally directlry, for it is small

#Create new rf-rate by converting rate from % to decimal. Then remove the og rate column by setting it to NULL
zero_curve_data <- zero_curve_data |>
  mutate(
    rf_rate = rate / 100,
    rate = NULL
  )

#OPTION_DATA======================================
#I'm breaking down each step to follow the methodology from Cao & Han
#Select ATM options at the end of each month
#Following this quote: "At the end of each month and for each optionable stock,
# we collect a pair of options (one call and one put) that
# are closest to being at-the-money and have the shortest
# maturity among those with more than one month to expiration."

#I'm now identifying the month-end dates in the data
#First, identify month-end dates in our data, aka the last trading day of each month
month_end_dates <- option_price |>
  select(date) |>
  distinct() |>
  collect() |>
  mutate(
    year_month = format(date, "%Y-%m")
  ) |>
  group_by(year_month) |>
  summarise(
    month_end_date = max(date),
    .groups = "drop"
  ) |>
  pull(month_end_date)

#Select one ATM call and one ATM put for each stock at each month-end
atm_options_selected <- option_price |>
  filter(date %in% month_end_dates) |> 
  mutate(
    mid_quote = (best_bid + best_offer) / 2, #Cao & Han methodology
    strike_price = strike_price / 1000, #Convert to dollars
    days_to_maturity = as.integer(exdate - date)
    ) |> #TTM calculation
  filter(
    !is.na(delta),
    #am_settlement == 0, #PM settlement only
    #ss_flag == 0, #Standard settlement only
    best_bid > 0,                      
    best_bid < best_offer, #Valid spreads
    volume > 0, #Traded options only
    mid_quote >= 1/8, #minimum price filter (0.125)
    between(moneyness, 0.8, 1.2),
    days_to_maturity >= 30 #Work with the expiration rule for now, forcing 47-52 now will mess the data up
  ) |> 
  left_join(security_price, 
    by = c("secid", "date")) |> 
    mutate(
      #For each stock, date, and option type, find the option closest to ATM
    moneyness_distance = abs(strike_price - security_price)
  ) |> 
    #First, among options with > 30 days to expiration, find shortest maturity
    group_by(secid, date, cp_flag) |> 
    filter(days_to_maturity == min(days_to_maturity)) |> 
    #Then, among those with shortest maturity, find closest to ATM
    slice_min(moneyness_distance, n = 1, with_ties = FALSE) |> 
    ungroup() |>
    transmute(
      option_series_id = concat_ws('_', secid, strftime(date, '%Y-%m'), cp_flag, strike_price, exdate),
      secid,
      cp_flag,
      strike_price,
      exdate,
      #Selection info
      selection_date = date,
      selection_month = strftime(date, '%Y-%m'),
      #Initial values to avoid overlap
      initial_mid_quote = mid_quote,
      initial_delta = delta,
      initial_gamma = gamma,
      days_to_maturity_initial = days_to_maturity,
      initial_security_price = security_price
    )        

#Now get all daily data for these selected options throughout the following month
#For each selected option, track it from selection date until next month-end or expiration
#Get all daily option data
daily_option_data <- option_price |>
  mutate(
    strike_price = strike_price / 1000,  # Convert to match atm_options_selected
    mid_quote = (best_bid + best_offer) / 2
  ) |>
  filter(
    !is.na(delta),
    best_bid > 0,
    best_bid < best_offer,
    mid_quote >= 1/8
  ) |>
  select(
    optionid, secid, date, strike_price, cp_flag, exdate,
    mid_quote, delta, gamma, best_bid, best_offer, volume
  )


#Join with selected options to check the chosen ones only
tracking_data <- atm_options_selected |>
  inner_join(
    daily_option_data,
    by = c("secid", "cp_flag", "strike_price", "exdate"),
    relationship = "many-to-many",
  ) |>
  filter(
    date >= selection_date,  # Only track after selection
    date <= selection_date + months(1)
  ) |>
  arrange(option_series_id, date)
tracking_data = tracking_data |> collect()

#Add security prices and risk-free rates
tracking_data = tracking_data |> 
  left_join(
    security_price |> collect(),
    by = c("secid", "date")
  ) |> 
  mutate(
    days_to_maturity = as.integer(exdate - date)
  ) |> 
  left_join(
    zero_curve_data,
    by = join_by(
      date,
      closest(days_to_maturity <= days)
    )
  )
tracking_data |> head()
#no aribtrage bounds (zero and intrinscic value bound)
#This was mentioned in section 2.1 in the paper. 
tracking_data <- tracking_data |> 
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
    no_arb_cond = case_when(
      cp_flag == "C" ~ (mid_quote >= call_lower_bound) & (mid_quote <= call_upper_bound),
      cp_flag == "P" ~ (mid_quote >= put_lower_bound) & (mid_quote <= put_upper_bound),
      TRUE ~ FALSE
    )
  ) |>
  filter(no_arb_cond == TRUE) |>
  select(-call_lower_bound, -call_upper_bound, -put_lower_bound, -put_upper_bound, -no_arb_cond)


#THE CALCULATION==============================================

#SECTION 2 OF THE PAPER
#In the delta-hedging formula, we need to compare the today's value with prev.
#Hence, create lag() columns to not have to call it in all the calculation agian.
#Group by optionid so the lag() won't go across different options.

#2.1: Create the lag columns for easy calc
daily_calculations = tracking_data |> 
  arrange(option_series_id, date) |> 
  group_by(option_series_id) |> 
  mutate(
    #Only lag for key values
    prev_date = lag(date),
    prev_security_price = lag(security_price),
    prev_mid_quote = lag(mid_quote),
    prev_delta = lag(delta),
    prev_rf_rate = lag(rf_rate),
    #Since we're using 365 days calendar, account for weekends and calendar gaps
    #in trading days
    days_elapsed = as.numeric(date - prev_date),
    
    #1: The middle component of the equation
    #Formula: - delta_C,t_n * [S(t_n+1) − S(t_n)]
    pnl_stock_hedge = -prev_delta * (security_price - prev_security_price),
  
    #2: The interest component
    #Formula: - (a_n*rf)/365 * [C_t_n - delta_C,t_n * S_t_n]
    pnl_interest = - (days_elapsed * prev_rf_rate/365) * (prev_mid_quote - (prev_delta * prev_security_price)),
    
    #3: Change in option value: C[t] - C[t-1]
    pnl_option = mid_quote - prev_mid_quote
  ) |> 
  ungroup()

  
#2.3: Define the monthly holding periods and aggregate returns
#Create start_month to identify the observations
monthly_returns = daily_calculations |> 
  group_by(option_series_id, secid, cp_flag, selection_month) |> 
  summarise(
    #Number of trading days
    n_trading_days = n() - 1,  # Subtract 1 for the initial observation
    
    #Initial values (at selection)
    selection_date = first(date),
    strike_price = first(strike_price),
    expiration_date = first(exdate),
    S_initial = first(security_price),
    C_initial = first(mid_quote),
    delta_initial = first(delta),
    days_to_maturity_initial = first(days_to_maturity),
    
    #Final values
    final_date = last(date),
    S_final = last(security_price),
    C_final = last(mid_quote),
    delta_final = last(delta),
    
    #PnL components (sum of daily, excluding first NA row)
    total_pnl_option = sum(pnl_option, na.rm = TRUE),
    total_pnl_stock_hedge = sum(pnl_stock_hedge, na.rm = TRUE),
    total_pnl_interest = sum(pnl_interest, na.rm = TRUE),
    
    .groups = "drop"
  ) |>
  #With the options sorted out, I want to remove any n_obs with fewer the 10.
  #The reason is because we want to look at meaningful holding periods.
  #At least 10 trading days in the month meaning that we're able to
  #rebalance the option effectively and avoid any options that are
  #1) listed near-end month, 2) expired soon, 3) rarely-traded options,...
  filter(
    !is.na(C_initial),
    !is.na(C_final)
  )
#colnames(monthly_returns)

#2.4: Final Delta-Hedged Return
delta_hedged_returns = monthly_returns |> 
  mutate(
    #Calc the total gain - all components
    total_gain = total_pnl_option + total_pnl_stock_hedge + total_pnl_interest,
    #Calc scaling factor for calls or puts
    scaling_factor = if_else(cp_flag == "C", abs(delta_initial*S_initial - C_initial), abs(C_initial - delta_initial*S_initial)),
    #Delta-Hedged Return
    delta_hedged_return = total_gain / scaling_factor,
    #Additional metrics
    holding_period_days = as.numeric(final_date - selection_date),
    moneyness = S_initial / strike_price,
    #Option return
    option_return = total_gain /C_initial
  ) |> 
  select(
    option_series_id,
    total_gain,
    delta_hedged_return,
    option_return,
    scaling_factor,
    secid,
    cp_flag,
    holding_period_days,
    n_trading_days,
    moneyness,
    delta_initial,
    days_to_maturity_initial
  )

#DELTA-HEDGED RETURN STAT SUMMARY====================================
#Try to replicate Table 1 in the research paper

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
call_stats <- delta_hedged_returns |>
  filter(cp_flag == "C") |>
  mutate(
    delta_hedged_return_pct = delta_hedged_return * 100,
    moneyness_pct = moneyness * 100
  )

panel_a_table <- bind_rows(
  call_stats |> 
    create_summary_stats(delta_hedged_return_pct, "Delta-hedged gain until month-end/(ΔS-C) (%)"),
  call_stats |> 
    create_summary_stats(days_to_maturity_initial, "Days to maturity"),
  call_stats |> 
    create_summary_stats(moneyness_pct, "Moneyness = S/K (%)")
)
panel_a_table

#Panel B: Put options
put_stats <- delta_hedged_returns |>
  filter(cp_flag == "P") |>
  mutate(
    delta_hedged_return_pct = delta_hedged_return * 100,
    moneyness_pct = moneyness * 100
  )

panel_b_table <- bind_rows(
  put_stats |> 
    create_summary_stats(delta_hedged_return_pct, "Delta-hedged gain until month-end/(P-ΔS) (%)"),
  put_stats |> 
    create_summary_stats(days_to_maturity_initial, "Days to maturity"),
  put_stats |> 
    create_summary_stats(moneyness_pct, "Moneyness = S/K (%)")
)
