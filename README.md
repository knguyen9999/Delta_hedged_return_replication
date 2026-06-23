# Option Return Replication Studies

Replication code for several academic papers on **delta-hedged option returns**
and option-return predictability, built on OptionMetrics IvyDB and CRSP data.

This repository contains **only my own analysis code and documentation**. The
underlying datasets are licensed (WRDS / OptionMetrics IvyDB and CRSP) and the
source papers are copyrighted, so neither is included here — see
[Data](#data-not-included) below.

## Papers replicated

- **Cao & Han** — *Cross-section of option returns and idiosyncratic stock
  volatility* (delta-hedged returns; see `cao_han_paper.R` and
  `README_TABLE1_REPLICATION.md`).
- **Muravyev & Ni** — *Why do option returns change sign from day to night?*
  (`Murav:Ni/`).
- **Duarte, Jones & Wang** — noisy-price / delta-hedging work (`Jeff & Wang/`).

## Repository structure

| Path | Description |
|------|-------------|
| `cao_han_paper.R` | Cao & Han Table 1 replication |
| `delta_hedged_preplication.R`, `getting_close.R`, `draft_8_01.R` | Core delta-hedged return pipelines |
| `delta_hedged_db_database_building.R` | Builds the DuckDB working database from raw IvyDB parquet files |
| `comparison_*.R`, `inner_vs_leftjoin.R` | Method comparisons and robustness checks |
| `delta_hedged_replication_documentation.qmd` | Quarto write-up of the methodology |
| `Murav:Ni/` | Muravyev & Ni day-vs-night replication |
| `Jeff & Wang/` | Duarte–Jones–Wang noisy-price code |
| `Delta_Hedges_Research/` | Earlier exploratory research scripts |
| `*_detailed.csv`, `summary_comparison.csv` | Small summary result tables |

## Data (not included)

The following are intentionally excluded via `.gitignore` and must be obtained
separately:

- OptionMetrics **IvyDB** option & security price files (`*.parquet`)
- **CRSP** stock data (`*.parquet`)
- The working **DuckDB** database (`delta_hedged_returns.duckdb`, ~46 GB)
- Copyrighted source papers (`*.pdf`)
- Generated outputs (`*.rds`, `*.html`, `*.png`)

Access to IvyDB and CRSP requires a **WRDS subscription**. Publicly available
Fama–French factors are included (`F-F_Research_Data_Factors_daily.csv`).

## Running the code

1. Obtain the IvyDB / CRSP data through WRDS and place the parquet files in the
   project root.
2. Run `delta_hedged_db_database_building.R` to build the DuckDB database.
3. Run the relevant paper script (e.g. `cao_han_paper.R`), updating the working
   directory path at the top to your machine.

Code is written in **R** (uses `duckdb`, `data.table`/`dplyr`, `arrow`).
