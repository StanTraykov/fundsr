# Run registered data loaders

Runs the data loader registry (`.fundsr$data_loaders`) to populate (or
refresh) the package's storage environment (`.fundsr_storage`).

## Usage

``` r
run_data_loaders(reload = FALSE)
```

## Arguments

- reload:

  Logical scalar. If `TRUE`, forces a full reload by setting
  `options(fundsr.reload = TRUE)` for the duration of this call.

## Value

Invisibly returns `.fundsr_storage` after running the data loaders.

## Details

The function temporarily sets the `fundsr.reload` option so that data
loaders can decide whether to recompute cached objects.

The previous value of option "fundsr.reload" is restored on exit, even
if a data loader errors.

Data loaders are taken from `.fundsr$data_loaders` and are called
sequentially in registration order. Each registered function must take
no arguments.

## See also

Other fund/index workflow functions:
[`add_data_loader()`](https://stantraykov.github.io/fundsr/reference/add_data_loader.md),
[`build_all_series()`](https://stantraykov.github.io/fundsr/reference/build_all_series.md),
[`clear_data_loaders()`](https://stantraykov.github.io/fundsr/reference/clear_data_loaders.md),
[`clear_storage()`](https://stantraykov.github.io/fundsr/reference/clear_storage.md),
[`get_storage()`](https://stantraykov.github.io/fundsr/reference/get_storage.md),
[`join_env()`](https://stantraykov.github.io/fundsr/reference/join_env.md),
[`load_fund()`](https://stantraykov.github.io/fundsr/reference/load_fund.md),
[`store_timeseries()`](https://stantraykov.github.io/fundsr/reference/store_timeseries.md)
