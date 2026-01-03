# Rolling annualized tracking differences

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

  Name of the date column in `df`. Defaults to `"date"`. Must be of
  class `Date` and sorted in ascending order.

- index_level:

  Which index level to use, one of `"net"` or `"gross"`. If `"gross"`,
  `gross_suffix` is appended to the mapped index base name before lookup
  in `df`. Defaults to `"net"`.

- annual_days:

  Number of days used for annualization. Defaults to `365`.

- messages:

  Character vector controlling emitted messages. Any of `"roll"`
  (per-pair progress) and `"skip"` (skip reasons). Use
  `messages = "roll"` to show only progress, `messages = "skip"` to show
  only skip reasons, or `messages = character()` or `NULL` to silence
  all messages. Defaults to `c("roll", "skip")`.

- gross_suffix:

  Suffix appended to the mapped index base name when
  `index_level = "gross"`. Defaults to `"-GR"`.

## Value

A named list with two data frames, `log` and `cagr`. Each data frame
contains `date_col` followed by one column per fund (named as in
`fund_index_map`), holding the rolling annualized tracking differences.

## Details

For each date \\t\\, an anchor date \\t_0\\ is chosen as the last
available observation at or before \\t - n\\days\\ where both fund and
index values are present. Let \\\Delta = t - t_0\\ in calendar days.

The annualized tracking differences are:

- Log-return difference:
  \$\$\left\[\ln\left(\frac{f_t}{f\_{t_0}}\right) -
  \ln\left(\frac{i_t}{i\_{t_0}}\right)\right\] \times
  \frac{annual\\days}{\Delta}\$\$

- CAGR difference:
  \$\$\left(\frac{f_t}{f\_{t_0}}\right)^{annual\\days/\Delta} -
  \left(\frac{i_t}{i\_{t_0}}\right)^{annual\\days/\Delta}\$\$

Values are `NA` when an anchor cannot be found, required inputs are
missing, \\\Delta \le 0\\, or values are invalid for the chosen formula
(e.g. non-positive inputs for log returns).
