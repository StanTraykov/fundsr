# Deprecated alias for [`import_fund()`](https://stantraykov.github.io/fundsr/reference/import_fund.md).

`load_fund()` has been renamed to
[`import_fund()`](https://stantraykov.github.io/fundsr/reference/import_fund.md).

## Usage

``` r
load_fund(
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
  data_sheet = lifecycle::deprecated(),
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
  [`import_fund`](https://stantraykov.github.io/fundsr/reference/import_fund.md)

  :   

## Value

Invisibly returns `NULL`. The imported data are stored in
`session$storage` under `tolower(ticker)`. A fund/index mapping is
recorded in `session$state$fund_index_map` when `benchmark` is supplied.
