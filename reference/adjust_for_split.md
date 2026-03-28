# Adjust a time series for a stock split

Adjusts values in a numeric column for observations strictly before a
given split date by dividing them by the supplied split ratio.

## Usage

``` r
adjust_for_split(data, split_date, split_ratio, value_col, date_col = "date")
```

## Arguments

- data:

  A data frame.

- split_date:

  A single date coercible via
  [`lubridate::as_date()`](https://lubridate.tidyverse.org/reference/as_date.html).

- split_ratio:

  A positive numeric scalar giving the split ratio.

- value_col:

  String. Name of the numeric column to adjust.

- date_col:

  String. Name of the date column in `data`.

## Value

A data frame with the same columns as `data`, where `value_col` has been
adjusted for rows with dates strictly before `split_date`.

## Details

The function parses `split_date` and `data[[date_col]]` with
[`lubridate::as_date()`](https://lubridate.tidyverse.org/reference/as_date.html).
Rows with missing dates are left unchanged. Rows with unparseable
non-missing dates trigger an error.

For rows where the parsed date is strictly earlier than `split_date`,
the values in `value_col` are divided by `split_ratio`.

## See also

Other fund/index workflow functions:
[`add_data_loader()`](https://stantraykov.github.io/fundsr/reference/add_data_loader.md),
[`build_all_series()`](https://stantraykov.github.io/fundsr/reference/build_all_series.md),
[`clear_data_loaders()`](https://stantraykov.github.io/fundsr/reference/clear_data_loaders.md),
[`clear_storage()`](https://stantraykov.github.io/fundsr/reference/clear_storage.md),
[`get_storage()`](https://stantraykov.github.io/fundsr/reference/get_storage.md),
[`import_fund()`](https://stantraykov.github.io/fundsr/reference/import_fund.md),
[`join_env()`](https://stantraykov.github.io/fundsr/reference/join_env.md),
[`run_data_loaders()`](https://stantraykov.github.io/fundsr/reference/run_data_loaders.md),
[`store_timeseries()`](https://stantraykov.github.io/fundsr/reference/store_timeseries.md)

## Examples

``` r
df <- data.frame(
  date = c("2024-01-01", "2024-01-02", "2024-01-03"),
  price = c(300, 330, 120)
)

adjust_for_split(
  data = df,
  split_date = "2024-01-03",
  split_ratio = 3,
  value_col = "price"
)
#>         date price
#> 1 2024-01-01   100
#> 2 2024-01-02   110
#> 3 2024-01-03   120
```
