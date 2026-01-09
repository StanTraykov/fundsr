# Clear registered data loaders

Clears the internal data-loader registry (`.fundsr$data_loaders`),
removing all previously registered data loader functions.

## Usage

``` r
clear_data_loaders()
```

## Value

Invisibly returns `NULL`. Called for side effects.

## See also

Other fund/index workflow functions:
[`add_data_loader()`](https://stantraykov.github.io/fundsr/reference/add_data_loader.md),
[`build_all_series()`](https://stantraykov.github.io/fundsr/reference/build_all_series.md),
[`clear_storage()`](https://stantraykov.github.io/fundsr/reference/clear_storage.md),
[`get_storage()`](https://stantraykov.github.io/fundsr/reference/get_storage.md),
[`join_env()`](https://stantraykov.github.io/fundsr/reference/join_env.md),
[`load_fund()`](https://stantraykov.github.io/fundsr/reference/load_fund.md),
[`run_data_loaders()`](https://stantraykov.github.io/fundsr/reference/run_data_loaders.md),
[`store_timeseries()`](https://stantraykov.github.io/fundsr/reference/store_timeseries.md)

## Examples

``` r
clear_data_loaders()
```
