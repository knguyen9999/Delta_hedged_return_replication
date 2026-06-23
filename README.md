# Delta-Hedged Option Returns — Replication of Three Asset-Pricing Studies

Faculty-advised research project replicating and comparing three influential
studies on **delta-hedged option returns** using OptionMetrics IvyDB and CRSP
data. The goal: rebuild each paper's data pipeline and return calculation from
scratch in R, then run all three methodologies on the *same* underlying data to
see whether their central result holds.

> **Headline result:** all three methodologies independently produce
> **statistically significant negative delta-hedged returns** for both calls and
> puts — i.e., option buyers systematically overpay and the hedged position
> loses money on average. The finding is robust across very different filtering
> rules and sample sizes.

This repository contains **only my own analysis code and documentation.** The
underlying datasets are licensed (WRDS / OptionMetrics IvyDB and CRSP) and the
source papers are copyrighted, so neither is included — see
[Data](#data-not-included).

## Papers replicated

| Folder | Study | Focus |
|--------|-------|-------|
| `Cao & Han/` | **Cao & Han** — *Cross-section of option returns and idiosyncratic stock volatility* | Near-the-money, shortest-maturity (>30d) options; strictest filters |
| `Jeff & Wang/` | **Duarte, Jones & Wang** — noisy-price methodology | Broader 14–60 day sample with bias corrections (MR, RC, CEIV, SS) |
| `Murav & Ni/` | **Muravyev & Ni** — *Why do option returns change sign from day to night?* | Strict spread/delta filters, market-level aggregation |

## Key findings

Mean delta-hedged returns on S&P 500 index options, all three methods on the
same data:

| Method | Calls | (t-stat) | Puts | (t-stat) | Sample (calls/puts) |
|--------|------:|:--------:|-----:|:--------:|--------------------:|
| Cao-Han | −1.77% | −7.04 | −3.35% | −20.75 | 35 / 188 |
| Duarte-Jones-Wang | −0.87% | −20.66 | −0.69% | −16.94 | 169,401 / 198,738 |
| Muravyev-Ni | −0.87% | −58.86 | −0.39% | −24.77 | 471,495 / 423,731 |

The methodologies differ enormously in sample size (Cao-Han's conservative
filters keep only a few hundred contracts; the others keep hundreds of
thousands) yet all reach the same qualitative conclusion.

## Repository structure

| Path | Description |
|------|-------------|
| `delta_hedged_db_database_building.R` | Builds a reproducible **DuckDB** database from raw IvyDB/CRSP parquet files and Fama–French rates; assembles the SPX `master_data_sp500` table that feeds every replication |
| `Cao & Han/cao_han_replication.R` | Cao & Han Table 1 replication |
| `Jeff & Wang/duarte_jones_paper_replication.R` | Duarte–Jones–Wang replication with bias adjustments |
| `Jeff & Wang/very_noisy_code.R` | Noisy-price variant |
| `Murav & Ni/muravyev_ni_paper_replication.R` | Muravyev & Ni replication |
| `Murav & Ni/option_day_night-1.R` | Day-vs-night decomposition |
| `comparison_analysis_file.R`, `comparison_between_methods.R` | Standardize and compare results across the three methods |
| `check_data_size.R` | Data coverage / sanity checks |
| `delta_hedged_replication_documentation.qmd` | **Full methodology write-up** — per-paper filtering criteria (with page references), the inner-vs-left join decision, and the comparative analysis. Start here to understand the project. |
| `README_TABLE1_REPLICATION.md` | Detailed notes on the Cao-Han Table 1 replication |

## Methodology in brief

For each selected option, the daily delta-hedged gain follows the standard
formulation:

```
Π(t, t+1) = C(t+1) − C(t) − δ(t)·[S(t+1) − S(t)] − r·(C(t) − δ(t)·S(t))·Δt
```

i.e., the change in option value, minus the delta-neutralizing stock position,
minus interest on the financed cash. Gains are accumulated to expiration and
scaled by the initial capital required, then averaged and tested for
significance (t-tests). Each paper differs in *which* options enter the sample
(maturity, moneyness, liquidity, no-arbitrage, and ex-dividend filters) — those
differences are documented in detail in the `.qmd` write-up.

A representative design decision: before joining options to underlying prices,
`inner` vs `left` join was tested explicitly (coverage, rows lost, missing-date
forensics). Since coverage was effectively 100% and rows lost <5%, the
`inner_join` was justified and adopted.

## Data (not included)

Excluded via `.gitignore` and obtainable only through a **WRDS subscription**:

- OptionMetrics **IvyDB** option & security price files (`*.parquet`)
- **CRSP** daily stock files (`*.parquet`)
- The working **DuckDB** database (`delta_hedged_returns.duckdb`, ~46 GB)
- Copyrighted source papers (`*.pdf`) and generated outputs (`*.rds`, `*.html`, `*.png`)

## Running the code

1. Obtain the IvyDB / CRSP data through WRDS and place the parquet files in the
   project root.
2. Run `delta_hedged_db_database_building.R` to build the DuckDB database and the
   SPX master table.
3. Run any paper script (e.g. `Cao & Han/cao_han_replication.R`), updating the
   `setwd()` path at the top to your machine.
4. Run the `comparison_*.R` scripts to reproduce the cross-method comparison.

Written in **R** (`duckdb`, `arrow`, `tidyverse`; `sandwich`/`lmtest` for
inference). See `delta_hedged_replication_documentation.qmd` for the full
methodology.

---
*Author: Kai Nguyen · Faculty-advised research.*
