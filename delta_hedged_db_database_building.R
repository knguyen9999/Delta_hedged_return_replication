library(arrow)
library(tidyverse)
library(fs)
library(duckdb)
library(readr)

#THIS FILE SAVES ALL THE DATA TO DUCKDB
#Setting up the data path
setwd("/Users/kainguyen/Desktop/Paper_replication") #Put in your own folder path
# Create new DuckDB database for delta-hedged returns analysis
con <- dbConnect(duckdb(), dbdir = "delta_hedged_returns.duckdb", read_only = FALSE)
dbListTables(con)
#==============================================================================#

# BRING PARQUET DATA TO DUCKDB AND VIEW
# IVYDB OPTIONS METRICS DATA
  #OPTION-PRICE and SECURITY-PRICE

# Use DuckDB wild-cards instead of dir_ls() vectors
file_path_option <- dir_ls(regexp = "^Option_Price.*.parquet$")
file_path_security <- dir_ls(regexp = "^Security_Price.*.parquet$")

option_price_ds   <- open_dataset(file_path_option,   format = "parquet")
security_price_ds <- open_dataset(file_path_security, format = "parquet")


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

  # SECURITY FILE
    # Build df to DB
security_file_path <- "/Users/kainguyen/Desktop/Paper_replication/Security_File_2023-03-23.parquet"
dbExecute(con, "DROP VIEW IF EXISTS security_file_ds;")
security_file_ds <- open_dataset(security_file_path, format = "parquet") |> to_duckdb(con, "security_file_ds")
dbExecute(con, "CREATE OR REPLACE TABLE security_file AS SELECT * FROM security_file_ds;")
  #View DB
security_file <- tbl(con, "security_file")
security_file |> head(10) |> collect() |> View()

  # SECURITY NAME
    # Build df to DB
security_name_path <- "/Users/kainguyen/Desktop/Paper_replication/Security_Name_2023-03-23.parquet"
security_name_ds <- open_dataset(security_name_path, format = "parquet") |> to_duckdb(con, "security_name_ds")
dbExecute(con, "CREATE OR REPLACE TABLE security_name AS SELECT * FROM security_name_ds;")
    # View DB
security_name <- tbl(con, "security_name")
security_name |> head(10) |> collect() |> View()

  # DISTRIBUTION FILE
    # Build df to DB
distribution_file_path <- "/Users/kainguyen/Desktop/Paper_replication/Distribution_File_2023-03-23.parquet"
distribution_file_ds <- open_dataset(distribution_file_path, format = "parquet") |> to_duckdb(con, "distribution_file_ds")
dbExecute(con, "CREATE OR REPLACE TABLE distribution_file AS SELECT * FROM distribution_file_ds;")
    # View DB
distribution_file <- tbl(con, "distribution_file")
distribution_file |> head(10) |> collect() |> View()

#==============================================================================#

# CRSP DATA
  #CRSP-DSF DATA
    # Build df to DB
crsp_file_path <- "/Users/kainguyen/Desktop/Paper_replication/dsf_at2023.parquet"
crsp_dataset <- open_dataset(crsp_file_path, format = "parquet") |> to_duckdb(con, "crsp_dataset")
dbExecute(con, "CREATE OR REPLACE TABLE crsp_data AS SELECT * FROM crsp_dataset;")
    # View DB
crsp_data <- tbl(con, "crsp_data")
crsp_data |> head(10) |> collect() |> View()

  # DSE
    # Build df to DB
dse_path <- "/Users/kainguyen/Desktop/Paper_replication/CRSP Data/Masterfiles/dse_at2023.parquet"
dse_ds <- open_dataset(dse_path, format = "parquet") |> to_duckdb(con, "dse_ds")
dbExecute(con, "CREATE OR REPLACE TABLE dse_file AS SELECT * FROM dse_ds;")
    #View DB
dse_file <- tbl(con, "dse_file")
dse_file |> head(10) |> collect() |> View()

  # DSE NAMES
    # Build df to DB
dse_names_path <- "/Users/kainguyen/Desktop/Paper_replication/CRSP Data/Masterfiles/dsenames_at2023.parquet"
dse_names_ds <- open_dataset(dse_names_path, format = "parquet") |> to_duckdb(con, "dse_names_ds")
dbExecute(con, "CREATE OR REPLACE TABLE dse_names_file AS SELECT * FROM dse_names_ds;")
    #View DB
dse_names_file <- tbl(con, "dse_names_file")
dse_names_file |> head(10) |> collect() |> View()

#==============================================================================#

# FAMA-FRENCH DATA
# Create a temporary file for the ZIP
temp_zip <- tempfile(fileext =    ".zip")

# Download the ZIP file from the URL
download.file(
  url = "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors_daily_CSV.zip",
  destfile = temp_zip,
  mode = "wb" # Use binary mode for ZIP files
)

# Unzip the file to a temporary directory
temp_dir <- tempdir()
csv_file <- unzip(temp_zip, exdir = temp_dir)

# Read the CSV file using vroom
ff_data <- read_csv(csv_file, col_types = cols(), skip = 3) |>
  janitor::clean_names()
ff_data <- ff_data |>
  mutate(date = ymd(x1)) |>
  select(!x1) |>
  relocate(date) |>
  mutate(
    rf_daily = rf / 100,
    rf_annual = rf_daily * 365
  ) |>
  select(-rf) |> collect()
# Clean up temporary files (optional)
unlink(temp_zip)
unlink(csv_file)
#Load risk-free data into duckDB
dbWriteTable(con, "ff_rates", ff_data, overwrite = TRUE)

#==============================================================================#
# SXP 500 DATA FOR TESTING
#==============================================================================#
option_price <- tbl(con, "option_price")
security_price <- tbl(con, "security_price")
ff_data <- tbl(con, "ff_rates")

# S&P 500 index secid (SPX)
sp500_secid <- 108105  

# Create the joined dataset for S&P 500
master_data_sp500 <- option_price |>
  filter(secid == sp500_secid) |>
  inner_join(
    security_price |>
      filter(secid == sp500_secid) |>  # Pre-filter for efficiency
      select(secid, date, close, volume, return) |>
      rename(
        stock_close = close,
        stock_volume = volume,
        stock_return = return
      ),
    by = c("secid", "date")
  ) |>
  inner_join(
    ff_data |> 
      select(date, rf_daily, rf_annual),
    by = "date"
  ) |>
  # Add basic calculated fields
  mutate(
    # Convert strike to dollars
    strike = strike_price / 1000,
    # Calculate midpoint
    mid_price = (best_bid + best_offer) / 2,
    # Calculate spread
    spread = best_offer - best_bid,
    # Days to expiration
    days_to_expiry = as.integer(exdate - date),
    # Handle negative stock prices in IvyDB
    stock_price = abs(stock_close),
    # Moneyness
    moneyness = stock_price / nullif(strike, 0)
  )
#Create the data in the database
compute(master_data_sp500, 
        name = "master_data_sp500",
        temporary = FALSE,
        overwrite = TRUE)

colnames(master_data_sp500)
dbListTables(con)
