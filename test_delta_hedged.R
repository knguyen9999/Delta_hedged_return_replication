library(tidyverse)
library(duckdb)

# Connect to database
con <- dbConnect(
  duckdb(),
  dbdir = "delta_hedged_returns.duckdb",
  read_only = TRUE
)

# Check if connection works
dbListTables(con)

# Test a simple query
option_price <- tbl(con, "option_price")
simple_test <- option_price |> 
  filter(date >= as.Date("1996-01-01"), date <= as.Date("1996-01-31")) |>
  head(10) |>
  collect()

print("Simple test successful!")
print(dim(simple_test))

# Close connection
dbDisconnect(con)
