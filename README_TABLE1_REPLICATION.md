# Table 1 Replication - Cao and Han Option Returns Paper

## Overview
This code replicates Table 1 from the Cao and Han research paper on option return predictability, focusing on delta-hedged option returns using IvyDB data.

## Key Changes Made

### 1. Removed CRSP Dependencies
- Eliminated all references to CRSP data (dsf, dse_names_file, dse_file)
- Uses IvyDB security_price table directly (security_name table removed as it was CRSP-based)
- Simplified data structure to focus on IvyDB's native format

### 2. Dividend Filtering with IvyDB Distribution File
- Uses `distribution_file` table from IvyDB for dividend ex-dates
- Filters out options with dividends during their life using `ex_date` field
- More accurate than previous CRSP paydt approach

### 3. Data Preparation Improvements
- Uses IvyDB's secid as primary identifier throughout
- Simplified joins and reduced complexity
- Direct security price data without ticker complications

### 4. Delta-Hedged Return Calculation
The code calculates delta-hedged returns following the standard methodology:

**For each option position:**
- Initial position: Long option + short Δ shares of stock
- Daily P&L: ΔC - Δ × ΔS - interest on cash position
- Scaling factor: |ΔS - C| for calls, |P - ΔS| for puts
- Final return: Total P&L / Scaling factor

### 5. Table 1 Panels

#### Panel A: Call Options Summary Statistics
- Delta-hedged gains until maturity (%)
- Days to maturity
- Moneyness (S/K %)
- Vega
- Includes mean, median, std dev, and percentiles

#### Panel B: Put Options Summary Statistics
- Same statistics as Panel A but for put options
- Uses absolute values for vega (since put vega is typically negative)

#### Panel D: Volatility Quintiles (Calls Only)
- Sorts stocks by historical volatility into quintiles
- Shows how delta-hedged returns vary by underlying stock volatility
- Includes sample sizes, return statistics, and characteristics by quintile

## Key Filters Applied

1. **Sample Period**: 1996-2009 (matching Cao-Han paper)
2. **Option Filters**:
   - Volume > 0
   - Valid bid-ask spreads
   - Mid-quote ≥ $0.125
   - European-style options only (am_settlement == 0)
   - Valid delta available
   - Days to maturity > 30

3. **Quality Filters**:
   - No-arbitrage conditions enforced
   - ATM options selected (closest to S=K)
   - Shortest maturity for each stock-date-type
   - No dividends during option life

4. **Data Quality**:
   - Complete option and stock price data required
   - Valid risk-free rates from Fama-French data
   - Finite delta-hedged returns only

## Expected Results

Based on the Cao-Han findings, you should expect:
- **Negative mean delta-hedged returns** for both calls and puts
- **Increasing magnitude of negative returns** with stock volatility
- **Statistical significance** of the volatility effect
- **Similar patterns** between calls and puts but potentially different magnitudes

## Output Files

The code generates several CSV files:
- `panel_a_call_summary.csv`: Call option summary statistics
- `panel_b_put_summary.csv`: Put option summary statistics  
- `panel_d_volatility_quintiles.csv`: Results by volatility quintiles
- `full_delta_hedged_data.csv`: Complete dataset for further analysis

## Running the Code

1. Ensure your DuckDB database (`delta_hedged_returns.duckdb`) contains all required IvyDB tables
2. Update the working directory path in the code
3. Run the entire script - it will process the data and display Table 1 results
4. Check the generated CSV files for detailed results

## Notes

- The code uses efficient bulk processing to handle large datasets
- All calculations follow academic finance conventions
- Results include statistical tests for significance
- Memory-efficient approach using DuckDB for large-scale option data processing
