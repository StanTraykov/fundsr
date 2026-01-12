# Run all registered data loaders and join all loaded series into a big tibble.

Runs
[`run_data_loaders()`](https://stantraykov.github.io/fundsr/reference/run_data_loaders.md),
joins the resulting environment into a single data frame via
[`join_env()`](https://stantraykov.github.io/fundsr/reference/join_env.md),
and sorts the result by `by`.

## Usage

``` r
build_all_series(reload = FALSE, by = "date", ...)
```

## Arguments

- reload:

  Logical; if `TRUE`, forces a full reload by temporarily setting
  `options(fundsr.reload = TRUE)`.

- by:

  Column name to join by and to sort by.

- ...:

  Additional arguments forwarded to
  [`join_env()`](https://stantraykov.github.io/fundsr/reference/join_env.md)
  (e.g. `late`, `join_precedence`, etc.).

## Value

A tibble containing all joined series, sorted by `by`.

## Details

This function is a convenience wrapper for the most common workflow.

## See also

Other fund/index workflow functions:
[`add_data_loader()`](https://stantraykov.github.io/fundsr/reference/add_data_loader.md),
[`clear_data_loaders()`](https://stantraykov.github.io/fundsr/reference/clear_data_loaders.md),
[`clear_storage()`](https://stantraykov.github.io/fundsr/reference/clear_storage.md),
[`get_storage()`](https://stantraykov.github.io/fundsr/reference/get_storage.md),
[`join_env()`](https://stantraykov.github.io/fundsr/reference/join_env.md),
[`load_fund()`](https://stantraykov.github.io/fundsr/reference/load_fund.md),
[`run_data_loaders()`](https://stantraykov.github.io/fundsr/reference/run_data_loaders.md),
[`store_timeseries()`](https://stantraykov.github.io/fundsr/reference/store_timeseries.md)

## Examples

``` r
if (FALSE) { # \dontrun{

s1 <- build_all_series()
download_fund_data(redownload = TRUE)
s2 <- build_all_series(by = "date", late = "ftaw", join_precedence = c(".y", ".x")) %>%
  filter(date >= as_date("2013-01-01"))
} # }
```
