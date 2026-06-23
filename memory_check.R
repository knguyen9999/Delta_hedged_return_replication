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

#Filter data for the sample period (1996-2009 as in Cao-Han)
sample_start <- as.Date("1996-01-01")
sample_end <- as.Date("2009-11-01")

print("=== SIMPLIFIED DATA SIZE ANALYSIS ===")

# We saw: 67,026,058 observations after basic filters
# But ATM selection is VERY restrictive - it picks only 1 call + 1 put per stock-date

print("Key insights from the analysis:")
print("- Total raw option data: 562,460,044 observations")
print("- After quality filters: 67,026,058 observations") 
print("- Unique securities: 7,046")

# Rough estimation based on research knowledge:
# - Typical trading days per year: ~252
# - Years in sample: 14 (1996-2009)
# - Total trading days: ~3,500
# - Securities with options: ~1,000-2,000 actively traded
# - After ATM selection: 1 call + 1 put per security-date = 2 per security-date
# - Estimated ATM options: 2,000 securities × 3,500 days × 2 options = ~14M

# But many securities don't trade every day, and we have quality filters
# More realistic estimate: 500-1,000 securities × 3,500 days × 2 = 3.5M - 7M

estimated_atm_options <- 5000000  # Conservative middle estimate
daily_observations_per_option <- 30  # Average days from purchase to expiration
total_daily_observations <- estimated_atm_options * daily_observations_per_option

print(paste("Estimated ATM option contracts:", format(estimated_atm_options, big.mark = ",")))
print(paste("Estimated daily observations:", format(total_daily_observations, big.mark = ",")))

# Memory calculation
fields_per_observation <- 20
bytes_per_field <- 8
total_bytes <- total_daily_observations * fields_per_observation * bytes_per_field
mb_size <- total_bytes / (1024^2)
gb_size <- mb_size / 1024

print(paste("Estimated memory needed:", round(mb_size, 0), "MB"))
print(paste("That's:", round(gb_size, 2), "GB"))

print("\n=== RECOMMENDATION ===")
if (mb_size < 1000) {
  print("✅ SAFE: This should work fine on most laptops (< 1GB)")
  print("   Strategy: Collect all intermediate steps as planned")
} else if (mb_size < 4000) {
  print("⚠️  MODERATE: Monitor memory usage (1-4GB)")
  print("   Strategy: Process in chunks by year or collect with caution")
} else {
  print("❌ RISKY: Likely too large for most laptops (> 4GB)")
  print("   Strategy: Must process in smaller chunks")
}

print("\n=== ALTERNATIVE APPROACHES ===")
print("1. CHUNKING BY YEAR: Process 1996-2009 one year at a time")
print("2. PARTIAL MATERIALIZATION: Only collect the most complex steps")
print("3. STREAMING: Keep some operations as lazy queries")

# Close connection
dbDisconnect(con)
