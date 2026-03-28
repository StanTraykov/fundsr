# Register a data loader

Appends `fun` to the internal data-loader registry
(`session$state$data_loaders`). Registered functions are intended to be
run sequentially in registration order.

## Usage

``` r
add_data_loader(fun, session = NULL)
```

## Arguments

- fun:

  A function to register. Must take no arguments.

- session:

  Optional `fundsr_session` object. Defaults to the package default
  session when `NULL`.

## Value

Invisibly returns the updated `session$state$data_loaders` list.

## Details

If a loader with the same function body is already registered, `fun` is
not added again.

## See also

Other fund/index workflow functions:
[`adjust_for_split()`](https://stantraykov.github.io/fundsr/reference/adjust_for_split.md),
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
add_data_loader(function() NULL)
```
