# sp500-industrial-production-cointegration-1991-2001

Empirical Analysis of the S&P 500 Index and the Industrial Production Index, 1991-2001.

## Overview

This repository reproduces a full cointegration workflow between the S&P 500 equity index and the U.S. Industrial Production Index (INDPRO) for 1991-2001. The analysis replicates the exploratory figures, unit root tests, Johansen cointegration procedure, VECM estimation, impulse response analysis, and forecast error variance decomposition described in the coursework script.

## Repository structure

```
├── R/
│   └── analysis.R             # Main analysis script
├── data/
│   └── raw/                   # Place the source CSV files here (see below)
├── output/                    # Generated charts will be written here
└── README.md
```

## Data preparation

The original CSV files are not distributed with this project. Before running the analysis, download the following datasets and place them into `data/raw/`:

1. **S&P 500.csv** – Daily closing prices for the S&P 500 index with at least the columns `Date` (formatted `YYYY-MM-DD`) and `Close`. Any reputable provider (e.g., Stooq, Yahoo Finance) can be used.
2. **INDPRO.csv** – Monthly U.S. Industrial Production Index data from the St. Louis Fed (FRED) with the default columns `observation_date` and `INDPRO`.

The script filters both series to 1991-01-01 through 2001-01-01 and performs its calculations on the logarithms of the level series.

## Running the analysis

1. Install the required R packages (run once):
   ```r
   install.packages(c(
     "dplyr", "ggplot2", "zoo", "forecast", "urca", "xts", "tidyverse",
     "lubridate", "vars", "aTSA", "tseries", "tsDyn", "texreg"
   ))
   ```
2. Execute the script from the project root:
   ```bash
   Rscript R/analysis.R
   ```

The script prints intermediate tables and diagnostics to the console and writes figures to the `output/` directory.

## Notes

- The analysis follows the structure of the original coursework script and preserves the same calculations while adding guardrails for missing data and storing charts to disk.
- Because the script uses bootstrap-based impulse responses, the exact numerical outputs can vary slightly between runs.

