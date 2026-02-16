# Import a Vanguard fund

Wrapper around
[`load_fund()`](https://stantraykov.github.io/fundsr/reference/load_fund.md)
for Vanguard files.

## Usage

``` r
vang(ticker, file = NULL, benchmark = NULL, ...)
```

## Arguments

- ticker:

  Fund ticker.

- file:

  Optional filename override.

- benchmark:

  Optional benchmark key.

- ...:

  Additional arguments passed to
  [`load_fund()`](https://stantraykov.github.io/fundsr/reference/load_fund.md)
  and
  [`store_timeseries()`](https://stantraykov.github.io/fundsr/reference/store_timeseries.md),
  such as `postprocess`.

## See also

[`load_fund()`](https://stantraykov.github.io/fundsr/reference/load_fund.md)

Other provider wrappers:
[`amun()`](https://stantraykov.github.io/fundsr/reference/amun.md),
[`avan()`](https://stantraykov.github.io/fundsr/reference/avan.md),
[`bnpp()`](https://stantraykov.github.io/fundsr/reference/bnpp.md),
[`hsbc()`](https://stantraykov.github.io/fundsr/reference/hsbc.md),
[`inve()`](https://stantraykov.github.io/fundsr/reference/inve.md),
[`ishs()`](https://stantraykov.github.io/fundsr/reference/ishs.md),
[`msci()`](https://stantraykov.github.io/fundsr/reference/msci.md),
[`spdr()`](https://stantraykov.github.io/fundsr/reference/spdr.md),
[`ubs()`](https://stantraykov.github.io/fundsr/reference/ubs.md),
[`xtra()`](https://stantraykov.github.io/fundsr/reference/xtra.md)
