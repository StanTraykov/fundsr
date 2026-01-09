# Register a data loader

Appends `fun` to the internal data-loader registry
(`.fundsr$data_loaders`). Registered functions are intended to be run
sequentially in registration order.

## Usage

``` r
add_data_loader(fun)
```

## Arguments

- fun:

  A function to register. Must take no arguments.

## Value

Invisibly returns the updated `.fundsr$data_loaders` list.

## Details

If a loader with the same function body is already registered, `fun` is
not added again.

## See also

Other fund/index workflow functions:
[`build_all_series()`](https://stantraykov.github.io/fundsr/reference/build_all_series.md),
[`clear_data_loaders()`](https://stantraykov.github.io/fundsr/reference/clear_data_loaders.md),
[`clear_storage()`](https://stantraykov.github.io/fundsr/reference/clear_storage.md),
[`get_storage()`](https://stantraykov.github.io/fundsr/reference/get_storage.md),
[`join_env()`](https://stantraykov.github.io/fundsr/reference/join_env.md),
[`load_fund()`](https://stantraykov.github.io/fundsr/reference/load_fund.md),
[`run_data_loaders()`](https://stantraykov.github.io/fundsr/reference/run_data_loaders.md),
[`store_timeseries()`](https://stantraykov.github.io/fundsr/reference/store_timeseries.md)

## Examples

``` r
add_data_loader(function() NULL)
```
