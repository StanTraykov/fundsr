# Index provider wrappers

Vendor-specific import wrappers for index total return levels (net or
gross).

## Usage

``` r
msci(file, col_trans, benchmarks = NULL, var_name = NULL, ...)

spdj(file, col_trans, benchmarks = NULL, var_name = NULL, ...)
```

## Arguments

- file:

  Filename of the Excel file to import.

- col_trans:

  Named vector specifying column translations.

- benchmarks:

  Optional benchmark mapping to record in the fund-index map (for
  example, to map gross to net indices).

- var_name:

  Storage key used in `session$storage`. If `NULL`, `tolower(file)` is
  used.

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

Invisibly returns `NULL`. Data are stored via
[`store_timeseries()`](https://stantraykov.github.io/fundsr/reference/store_timeseries.md).

## Details

Wrappers around
[`store_timeseries()`](https://stantraykov.github.io/fundsr/reference/store_timeseries.md)
and
[`read_timeseries_excel()`](https://stantraykov.github.io/fundsr/reference/read_timeseries_excel.md)
for index files.

## Functions

- `msci()`: Import an MSCI index sheet and register benchmark mappings

- `spdj()`: Import an S&P Dow Jones index sheet and register benchmark
  mappings

## See also

[Fund provider
wrappers](https://stantraykov.github.io/fundsr/reference/fund_provider_wrappers.md),
[`store_timeseries()`](https://stantraykov.github.io/fundsr/reference/store_timeseries.md),
[`read_timeseries_excel()`](https://stantraykov.github.io/fundsr/reference/read_timeseries_excel.md)
