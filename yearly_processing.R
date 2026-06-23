library(tidyverse)
library(duckdb)

# Connect to database
con <- dbConnect(
  duckdb(),
  dbdir = "delta_hedged_returns.duckdb",
  read_only = TRUE
)

# Define function to process one year
process_year <- function(year) {
  print(paste("=== PROCESSING YEAR", year, "==="))
  
  # Set date range for this year
  sample_start <- as.Date(paste0(year, "-01-01"))
  sample_end <- as.Date(paste0(year, "-12-31"))
  
  print(paste("Date range:", sample_start, "to", sample_end))
  
  # Step 1: Get selected options for this year only
  # ... (put your option selection logic here but with year filter)
  
  # Step 2: Process delta-hedged data for this year
  # ... (put your delta-hedged calculation here)
  
  # Step 3: Return summary results
  print(paste("Completed year", year))
  
  return(data.frame(year = year, processed = TRUE))
}

# Process years sequentially
years_to_process <- 1996:2009
all_results <- list()

for (year in years_to_process) {
  result <- process_year(year)
  all_results[[as.character(year)]] <- result
  
  # Clean up memory between years
  gc()
}

# Combine all results
final_results <- bind_rows(all_results)

print("=== FINAL SUMMARY ===")
print(final_results)

# Close connection
dbDisconnect(con)
