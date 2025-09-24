# Raw data

Place the following CSV files in this directory before running the analysis script:

- `S&P 500.csv`: daily S&P 500 close prices downloaded from Stooq or other provider with columns `Date` (YYYY-MM-DD) and `Close`.
- `INDPRO.csv`: monthly U.S. Industrial Production Index data downloaded from FRED with columns `observation_date` (YYYY-MM-DD) and `INDPRO`.

The analysis expects log-level series covering 1991-01-01 through 2001-01-01. The repo does not ship the data because of licensing restrictions.
