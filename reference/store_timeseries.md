# Store a cached object in the package storage environment

Evaluate an expression and cache its result in the package storage
environment (`.fundsr_storage`) under a given name. The expression is
only re-evaluated when the cached value is missing, when
`overwrite = TRUE`, or when the global option `fundsr.reload` is `TRUE`.
Optionally merges additional fund/index mappings into
`.fundsr$fund_index_map`.

## Usage

``` r
store_timeseries(
  var_name,
  expr,
  fund_index_map = NULL,
  overwrite = FALSE,
  postprocess = identity
)
```

## Arguments

- var_name:

  Character scalar. Name of the variable to store in `.fundsr_storage`.

- expr:

  An expression. Evaluated in the caller's environment when
  (re)computing the cached value.

- fund_index_map:

  Optional named vector of fund/index pairs to merge into
  `.fundsr$fund_index_map`. Names are used as keys, values should be
  indices.

- overwrite:

  Logical scalar. If `TRUE`, recompute and replace any existing cached
  value, regardless of `fundsr.reload`.

- postprocess:

  Function applied to the computed value before caching. Only used when
  the value is (re)computed (i.e. not applied when a cached value is
  reused). Defaults to
  [`base::identity()`](https://rdrr.io/r/base/identity.html).

## Value

Invisibly returns `NULL` (called for its side effects).

## Details

`expr` is evaluated in the environment where `store_timeseries()` is
called (i.e. the caller's environment), then assigned into
`.fundsr_storage` under `var_name`.

Caching behavior is controlled by:

- `overwrite = TRUE` (always recompute),

- `options(fundsr.reload = TRUE)` (force recomputation globally), or

- absence of `var_name` in `.fundsr_storage` (compute once).

If `fund_index_map` is supplied, it is merged into
`.fundsr$fund_index_map` via name-based assignment: existing entries
with the same names are replaced.

## See also

Other fund/index workflow functions:
[`add_data_loader()`](https://stantraykov.github.io/fundsr/reference/add_data_loader.md),
[`build_all_series()`](https://stantraykov.github.io/fundsr/reference/build_all_series.md),
[`clear_data_loaders()`](https://stantraykov.github.io/fundsr/reference/clear_data_loaders.md),
[`clear_storage()`](https://stantraykov.github.io/fundsr/reference/clear_storage.md),
[`get_storage()`](https://stantraykov.github.io/fundsr/reference/get_storage.md),
[`join_env()`](https://stantraykov.github.io/fundsr/reference/join_env.md),
[`load_fund()`](https://stantraykov.github.io/fundsr/reference/load_fund.md),
[`run_data_loaders()`](https://stantraykov.github.io/fundsr/reference/run_data_loaders.md)
