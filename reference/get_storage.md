# Get the internal fund storage

Returns the fundsr's fund storage environment (`session$storage`).

## Usage

``` r
get_storage(session = NULL)
```

## Arguments

- session:

  Optional `fundsr_session` object. Defaults to the package default
  session when `NULL`.

## Value

The storage environment.

## See also

Other fund/index workflow functions:
[`add_data_loader()`](https://stantraykov.github.io/fundsr/reference/add_data_loader.md),
[`adjust_for_split()`](https://stantraykov.github.io/fundsr/reference/adjust_for_split.md),
[`build_all_series()`](https://stantraykov.github.io/fundsr/reference/build_all_series.md),
[`clear_data_loaders()`](https://stantraykov.github.io/fundsr/reference/clear_data_loaders.md),
[`clear_storage()`](https://stantraykov.github.io/fundsr/reference/clear_storage.md),
[`import_fund()`](https://stantraykov.github.io/fundsr/reference/import_fund.md),
[`join_env()`](https://stantraykov.github.io/fundsr/reference/join_env.md),
[`run_data_loaders()`](https://stantraykov.github.io/fundsr/reference/run_data_loaders.md),
[`store_timeseries()`](https://stantraykov.github.io/fundsr/reference/store_timeseries.md)
