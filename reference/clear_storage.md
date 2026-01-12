# Clear storage

Removes all objects from the package's storage environment
(`.fundsr_storage`). Optionally also clears the fund-index map
(`.fundsr$fund_index_map`).

## Usage

``` r
clear_storage(clear_map = FALSE)
```

## Arguments

- clear_map:

  Logical scalar; if `TRUE`, also clears `.fundsr$fund_index_map`.

## Value

Invisibly returns `NULL`. Called for side effects.

## See also

Other fund/index workflow functions:
[`add_data_loader()`](https://stantraykov.github.io/fundsr/reference/add_data_loader.md),
[`build_all_series()`](https://stantraykov.github.io/fundsr/reference/build_all_series.md),
[`clear_data_loaders()`](https://stantraykov.github.io/fundsr/reference/clear_data_loaders.md),
[`get_storage()`](https://stantraykov.github.io/fundsr/reference/get_storage.md),
[`join_env()`](https://stantraykov.github.io/fundsr/reference/join_env.md),
[`load_fund()`](https://stantraykov.github.io/fundsr/reference/load_fund.md),
[`run_data_loaders()`](https://stantraykov.github.io/fundsr/reference/run_data_loaders.md),
[`store_timeseries()`](https://stantraykov.github.io/fundsr/reference/store_timeseries.md)

## Examples

``` r
clear_storage()
clear_storage(clear_map = TRUE)
```
