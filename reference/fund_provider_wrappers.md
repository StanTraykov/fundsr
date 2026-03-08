# Fund provider wrappers

Vendor-specific import wrappers for NAV histories.

## Usage

``` r
ishs(ticker, file = NULL, benchmark = NULL, retrieve_benchmark = FALSE, ...)

spdr(ticker, file = NULL, benchmark = NULL, ...)

xtra(ticker, file = NULL, benchmark = NULL, retrieve_benchmark = FALSE, ...)

amun(ticker, file = NULL, benchmark = NULL, ...)

inve(ticker, file = NULL, benchmark = NULL, retrieve_benchmark = FALSE, ...)

vang(ticker, file = NULL, benchmark = NULL, ...)

ubs(ticker, file = NULL, benchmark = NULL, ...)

hsbc(ticker, file = NULL, benchmark = NULL, ...)

bnpp(ticker, file = NULL, benchmark = NULL, ...)

avan(ticker, file = NULL, benchmark = NULL, var_name = NULL, ...)
```

## Arguments

- ticker:

  Fund identifier/ticker.

- file:

  Optional filename. The default is `toupper(ticker)` with an extension
  determined by the vendor format, such as `.xls[x]` or `.csv`.

- benchmark:

  Optional benchmark identifier (to be added to the fund-index map).

- retrieve_benchmark:

  Logical. Also import the vendor-supplied benchmark series. Only
  supported for NAV histories from Invesco, iShares, and Xtrackers
  (`inve()`, `ishs()`, `xtra()`).

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

- var_name:

  Optional custom storage key. Supported by all wrapper functions,
  either as a formal argument or via `...` passed to the underlying
  helper
  ([`import_fund()`](https://stantraykov.github.io/fundsr/reference/import_fund.md)
  or
  [`store_timeseries()`](https://stantraykov.github.io/fundsr/reference/store_timeseries.md)).

## Value

Invisibly returns `NULL`. Data are stored via
[`store_timeseries()`](https://stantraykov.github.io/fundsr/reference/store_timeseries.md).

## Details

Most fund importers are wrappers around
[`import_fund()`](https://stantraykov.github.io/fundsr/reference/import_fund.md)
(which uses
[`store_timeseries()`](https://stantraykov.github.io/fundsr/reference/store_timeseries.md)
and
[`read_timeseries_excel()`](https://stantraykov.github.io/fundsr/reference/read_timeseries_excel.md)).
Some use
[`store_timeseries()`](https://stantraykov.github.io/fundsr/reference/store_timeseries.md)
directly, coupled with a reader like
[`read_timeseries()`](https://stantraykov.github.io/fundsr/reference/read_timeseries.md)
or
[`read_timeseries_excel()`](https://stantraykov.github.io/fundsr/reference/read_timeseries_excel.md).
Extra arguments (`...`) are passed to
[`store_timeseries()`](https://stantraykov.github.io/fundsr/reference/store_timeseries.md).
For wrappers built on
[`import_fund()`](https://stantraykov.github.io/fundsr/reference/import_fund.md),
`...` is first passed through
[`import_fund()`](https://stantraykov.github.io/fundsr/reference/import_fund.md).

## Functions

- `ishs()`: Import an iShares NAV history

- `spdr()`: Import an SPDR NAV history

- `xtra()`: Import an Xtrackers NAV history

- `amun()`: Import an Amundi NAV history

- `inve()`: Import an Invesco NAV history

- `vang()`: Import a Vanguard NAV history

- `ubs()`: Import a UBS NAV history

- `hsbc()`: Import an HSBC NAV history

- `bnpp()`: Import a BNP Paribas NAV history

- `avan()`: Import an Avantis NAV history

## See also

[Index provider
wrappers](https://stantraykov.github.io/fundsr/reference/index_provider_wrappers.md),
[`import_fund()`](https://stantraykov.github.io/fundsr/reference/import_fund.md),
[`store_timeseries()`](https://stantraykov.github.io/fundsr/reference/store_timeseries.md),
[`read_timeseries_excel()`](https://stantraykov.github.io/fundsr/reference/read_timeseries_excel.md),
[`read_timeseries()`](https://stantraykov.github.io/fundsr/reference/read_timeseries.md)

## Examples

``` r
if (FALSE) { # \dontrun{
xtra("SCWX", benchmark = "ACWI", file = "HistoricalData-LU2903252349.xlsx")

spdr(
    "SPYI",
    benchmark = "ACWI_IMI",
    postprocess = function(x) {
        adjust_for_split(x, lubridate::as_date("2026-02-23"), 25, "spyi")
    }
)

fs <- fundsr_session()
inve("FWRA", benchmark = "FTAW", retrieve_benchmark = TRUE, session = fs)
} # }
```
