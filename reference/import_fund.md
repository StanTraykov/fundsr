# Import a fund's NAV data and optionally register its benchmark mapping

Imports a fund's NAV time series from an Excel file and stores it in the
storage environment via
[`store_timeseries()`](https://stantraykov.github.io/fundsr/reference/store_timeseries.md).
Optionally, a benchmark column can also be imported, and a fund/index
mapping is recorded in `session$state$fund_index_map`.

## Usage

``` r
import_fund(
  ticker,
  file = NULL,
  sheet = 1,
  date_col = "^Date",
  nav_col = "^NAV",
  benchmark = NULL,
  benchmark_col = NULL,
  retrieve_benchmark = FALSE,
  date_order = "dmy",
  var_name = NULL,
  data_sheet = deprecated(),
  ...
)
```

## Arguments

- ticker:

  Fund ticker symbol. Used (in lower case) as the storage key and (in
  upper case) to derive the default filename.

- file:

  Optional filename. If `NULL` (the default), it is inferred from
  `ticker` as described above.

- sheet:

  Sheet index or name containing the NAV data.

- date_col:

  Regular expression identifying the date column.

- nav_col:

  Regular expression identifying the fund's NAV column.

- benchmark:

  Optional benchmark key that this fund should be associated with in the
  fund/index map. When `retrieve_benchmark = TRUE`, the same value is
  also used as the name under which the benchmark series is imported.

- benchmark_col:

  Regular expression identifying the benchmark column in the Excel
  sheet. Only used when `retrieve_benchmark = TRUE`.

- retrieve_benchmark:

  Logical; if `TRUE`, both `benchmark` and `benchmark_col` must be
  supplied and the benchmark column is imported alongside the fund.

- date_order:

  Date parsing order passed to the importer.

- var_name:

  Specify a custom variable name for the storage environment.

- data_sheet:

  Deprecated; use `sheet`.

- ...:

  Arguments passed on to
  [`store_timeseries`](https://stantraykov.github.io/fundsr/reference/store_timeseries.md)

  `overwrite`

  :   Logical scalar. If `TRUE`, recompute and replace any existing
      cached value, regardless of `fundsr.reload`.

  `postprocess`

  :   Function applied to the computed value before caching. Only used
      when the value is (re)computed (i.e. not applied when a cached
      value is reused). Defaults to
      [`base::identity()`](https://rdrr.io/r/base/identity.html).

  `session`

  :   Optional `fundsr_session` object. Defaults to the package default
      session when `NULL`.

## Value

Invisibly returns `NULL`. The imported data are stored in
`session$storage` under `tolower(ticker)`. A fund/index mapping is
recorded in `session$state$fund_index_map` when `benchmark` is supplied.

## Details

If `file` is `NULL`, the function searches `fundsr.data_dir` for exactly
one of `paste0(toupper(ticker), ".xlsx")` or
`paste0(toupper(ticker), ".xls")`.

The function builds a column-translation mapping from the fund NAV
column and, if requested, a benchmark column. It then calls
[`read_timeseries_excel()`](https://stantraykov.github.io/fundsr/reference/read_timeseries_excel.md)
to read the Excel file and
[`store_timeseries()`](https://stantraykov.github.io/fundsr/reference/store_timeseries.md)
to cache the imported object under `var_name`, if supplied, otherwise
`tolower(ticker)`. When `benchmark` is provided, a corresponding entry
is added to `session$state$fund_index_map` to link the fund to its
benchmark key.

## See also

Provider wrappers:
[`amun()`](https://stantraykov.github.io/fundsr/reference/fund_provider_wrappers.md),
[`hsbc()`](https://stantraykov.github.io/fundsr/reference/fund_provider_wrappers.md),
[`inve()`](https://stantraykov.github.io/fundsr/reference/fund_provider_wrappers.md),
[`ishs()`](https://stantraykov.github.io/fundsr/reference/fund_provider_wrappers.md),
[`spdr()`](https://stantraykov.github.io/fundsr/reference/fund_provider_wrappers.md),
[`ubs()`](https://stantraykov.github.io/fundsr/reference/fund_provider_wrappers.md),
[`vang()`](https://stantraykov.github.io/fundsr/reference/fund_provider_wrappers.md),
[`xtra()`](https://stantraykov.github.io/fundsr/reference/fund_provider_wrappers.md)

Other fund/index workflow functions:
[`add_data_loader()`](https://stantraykov.github.io/fundsr/reference/add_data_loader.md),
[`adjust_for_split()`](https://stantraykov.github.io/fundsr/reference/adjust_for_split.md),
[`build_all_series()`](https://stantraykov.github.io/fundsr/reference/build_all_series.md),
[`clear_data_loaders()`](https://stantraykov.github.io/fundsr/reference/clear_data_loaders.md),
[`clear_storage()`](https://stantraykov.github.io/fundsr/reference/clear_storage.md),
[`get_storage()`](https://stantraykov.github.io/fundsr/reference/get_storage.md),
[`join_env()`](https://stantraykov.github.io/fundsr/reference/join_env.md),
[`run_data_loaders()`](https://stantraykov.github.io/fundsr/reference/run_data_loaders.md),
[`store_timeseries()`](https://stantraykov.github.io/fundsr/reference/store_timeseries.md)

## Examples

``` r
fundsr_options(data_dir = fundsr_example_data())

import_fund("FNDA",
          "FNDA.xlsx",
          benchmark = "IDX1",
          sheet = "historical",
          date_col = "^As Of",
          nav_col = "^NAV")
#> ℹ *** Loading: fnda
#> ℹ Reading Excel: ‘/tmp/RtmprmZorq/temp_libpath1bfe26d430ce/fundsr/extdata/./FNDA.xlsx’
#> ℹ readxl succeeded. Returning data.
#> ✔ 730 rows x 2 cols (sheet='historical', date col ='^As Of').

import_fund("FNDB",
          benchmark = "IDX1",
          date_col = "^date",
          nav_col = "^net asset val",
          date_order = "mdy")
#> ℹ *** Loading: fndb
#> ℹ Reading Excel: ‘/tmp/RtmprmZorq/temp_libpath1bfe26d430ce/fundsr/extdata/./FNDB.xlsx’
#> ℹ readxl succeeded. Returning data.
#> ✔ 655 rows x 2 cols (sheet='1', date col ='^date').
```
