# Compute rolling annualized tracking differences (CAGR or log-return)

For each fund–index pair in `fund_index_map`, computes a rolling,
annualized tracking-difference series over a backward window of
`n_days`. The result is added as new columns named `<fund>_rd`.

## Usage

``` r
roll_diffs(
  df,
  n_days,
  fund_index_map,
  date_col = "date",
  use_log = TRUE,
  annual_days = 365,
  silent_skip = FALSE
)
```

## Arguments

- df:

  Data frame containing the date column, fund columns, and
  benchmark/index columns referenced in `fund_index_map`.

- n_days:

  Rolling lookback window in calendar days.

- fund_index_map:

  Named character vector mapping fund column names to their
  corresponding benchmark/index column names.

- date_col:

  Name of the date column in `df`. Defaults to `"date"`.

- use_log:

  Logical; if `TRUE`, computes **log-return tracking differences**. If
  `FALSE`, computes **CAGR tracking differences**. Defaults to `TRUE`.

- annual_days:

  Number of days used for annualization. Defaults to `365`.

- silent_skip:

  Logical; whether to show messages when skipping funds due to missing
  fund or tracked benchmark column in df. Defaults to `FALSE`.

## Value

A data frame like `df` with one additional column per fund, named
`<fund>_rd`, containing the rolling tracking differences.

## Details

For each fund–index pair, the function locates a start point `n_days`
(or more) before each observation and computes:

- **log-return difference** \$\$ ( \ln(f\_{t}/f\_{t_0}) -
  \ln(i\_{t}/i\_{t_0}) ) \times \frac{annual\\days}{t - t_0} \$\$

- **CAGR difference** \$\$ f^{annual\\days/(t - t_0)} -
  i^{annual\\days/(t - t_0)} \$\$

where \\f\\ and \\i\\ are fund and index values, respectively.

The tracking difference is `NA` whenever the anchor point is missing,
the values are invalid (e.g., non-positive for log mode), or no past
observation lies within the required window.
