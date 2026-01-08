# Load a fund's NAV data and optionally register its benchmark mapping

Imports a fund's NAV time series from an Excel file and stores it in the
storage environment via
[`store_timeseries()`](https://stantraykov.github.io/fundsr/reference/store_timeseries.md).
Optionally, a benchmark column can also be imported, and a fund/index
mapping is recorded in `.fundsr$fund_index_map`.

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
  data_sheet = NULL
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

  Sheet index or name containing the NAV data. Defaults to `1`.

- date_col:

  Regular expression identifying the date column. Defaults to `"^Date"`.

- nav_col:

  Regular expression identifying the fund's NAV column. Defaults to
  `"^NAV"`.

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

  Date parsing order passed to the importer. Defaults to `"dmy"`.

- var_name:

  Specify a custom variable name for the storage environment.

- data_sheet:

  Deprecated; use `sheet`.

## Value

Invisibly returns `NULL`. The imported data are stored in
`.fundsr_storage` under `tolower(ticker)`. A fund/index mapping is
recorded in `.fundsr$fund_index_map` when `benchmark` is supplied.

## Details

If `file` is `NULL`, the function searches
`getOption("fundsr.data_dir")` for exactly one of
`paste0(toupper(ticker), ".xlsx")` or `paste0(toupper(ticker), ".xls")`.

The function builds a column-translation mapping from the fund NAV
column and, if requested, a benchmark column. It then calls
[`read_timeseries_excel()`](https://stantraykov.github.io/fundsr/reference/read_timeseries_excel.md)
to read the Excel file and
[`store_timeseries()`](https://stantraykov.github.io/fundsr/reference/store_timeseries.md)
to cache the imported object under `tolower(ticker)`. When `benchmark`
is provided, a corresponding entry is added to `.fundsr$fund_index_map`
to link the fund to its benchmark key.

## See also

Basic functions:
[`store_timeseries()`](https://stantraykov.github.io/fundsr/reference/store_timeseries.md),
[`read_timeseries_excel()`](https://stantraykov.github.io/fundsr/reference/read_timeseries_excel.md)

Vendor-specific wrappers:
[`amun()`](https://stantraykov.github.io/fundsr/reference/amun.md),
[`hsbc()`](https://stantraykov.github.io/fundsr/reference/hsbc.md),
[`inve()`](https://stantraykov.github.io/fundsr/reference/inve.md),
[`ishs()`](https://stantraykov.github.io/fundsr/reference/ishs.md),
[`spdr()`](https://stantraykov.github.io/fundsr/reference/spdr.md),
[`ubs()`](https://stantraykov.github.io/fundsr/reference/ubs.md),
[`vang()`](https://stantraykov.github.io/fundsr/reference/vang.md),
[`xtra()`](https://stantraykov.github.io/fundsr/reference/xtra.md)
