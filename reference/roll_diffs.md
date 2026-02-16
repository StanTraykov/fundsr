# Compute rolling annualized tracking difference statistics

For each fund–index pair in `fund_index_map`, computes rolling,
annualized tracking differences over a backward-looking window of
`n_days` calendar days. Both log-return and CAGR forms are returned.

## Usage

``` r
roll_diffs(
  df,
  n_days,
  fund_index_map,
  date_col = "date",
  index_level = c("net", "gross"),
  annual_days = 365,
  messages = c("roll", "skip"),
  gross_suffix = "-GR"
)
```

## Arguments

- df:

  Data frame containing a date column, fund columns, and benchmark/index
  columns referenced in `fund_index_map`.

- n_days:

  Rolling lookback window in calendar days.

- fund_index_map:

  Named character vector mapping fund column names to their
  corresponding benchmark/index base column names.

- date_col:

  Name of the date column in `df`. Must be of class `Date` and sorted
  (strictly increasing).

- index_level:

  Which index level to use, one of `"net"` or `"gross"`. If `"gross"`,
  `gross_suffix` is appended to the mapped index base name before lookup
  in `df`.

- annual_days:

  Number of days used for annualization.

- messages:

  Character vector controlling emitted messages. Any of `"roll"`
  (per-pair progress) and `"skip"` (skip reasons). Use
  `messages = "roll"` to show only progress, `messages = "skip"` to show
  only skip reasons, or `messages = character()` or `NULL` to silence
  all messages.

- gross_suffix:

  Suffix appended to the mapped index base name when
  `index_level = "gross"`.

## Value

A named list with two data frames, `cagr` and `log`. Each data frame
contains `date_col` followed by one column per fund (named as in
`fund_index_map`), holding the rolling annualized tracking differences.

## Details

For each date \\t\\, a target anchor threshold \\t - n\\days\\ is
formed. The anchor date \\t_0\\ is chosen as the **last available
observation at or before** \\t - n\\days\\ among rows where **both**
fund and index values are present. Let \\\Delta = t - t_0\\ in calendar
days (\\\Delta\\ can be greater than `n_days` when data are missing
around the threshold).

The annualized tracking differences are:

- Log-return difference: \$\$
  \left\[\ln\left(\frac{f_t}{f\_{t_0}}\right) -
  \ln\left(\frac{i_t}{i\_{t_0}}\right)\right\] \times
  \frac{annual\\days}{\Delta} \$\$

- CAGR difference: \$\$
  \left(\frac{f_t}{f\_{t_0}}\right)^{annual\\days/\Delta} -
  \left(\frac{i_t}{i\_{t_0}}\right)^{annual\\days/\Delta} \$\$

\#' Values are `NA` when an anchor cannot be found, current-date inputs
are missing, or inputs are invalid for the chosen formula (e.g. any
non-positive level for log returns, or non-finite / non-positive ratios
for CAGR).

Funds are skipped (optionally with a message) when the fund column is
missing, the mapped index column is missing (after applying
`index_level` / `gross_suffix`), or when `fund == index`
(self-tracking).

Emitted messages will be visible at verbosity level \>= 1 (option
`fundsr.verbosity`). Verbosity level \>= 4 forces both message types
regardless of the `messages` argument.

## See also

Other rolling difference functions:
[`plot_roll_diffs()`](https://stantraykov.github.io/fundsr/reference/plot_roll_diffs.md)
