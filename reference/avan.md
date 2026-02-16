# Import an Avantis fund

Wrapper around
[`read_timeseries()`](https://stantraykov.github.io/fundsr/reference/read_timeseries.md)
for Avantis files.

## Usage

``` r
avan(ticker, file, benchmark = NULL, ...)
```

## Arguments

- ticker:

  Fund ticker.

- file:

  Filename.

- benchmark:

  Optional benchmark key.

- ...:

  Additional arguments passed to
  [`store_timeseries()`](https://stantraykov.github.io/fundsr/reference/store_timeseries.md),
  such as `postprocess`.

## See also

[`read_timeseries()`](https://stantraykov.github.io/fundsr/reference/read_timeseries.md)

Other provider wrappers:
[`amun()`](https://stantraykov.github.io/fundsr/reference/amun.md),
[`bnpp()`](https://stantraykov.github.io/fundsr/reference/bnpp.md),
[`hsbc()`](https://stantraykov.github.io/fundsr/reference/hsbc.md),
[`inve()`](https://stantraykov.github.io/fundsr/reference/inve.md),
[`ishs()`](https://stantraykov.github.io/fundsr/reference/ishs.md),
[`msci()`](https://stantraykov.github.io/fundsr/reference/msci.md),
[`spdr()`](https://stantraykov.github.io/fundsr/reference/spdr.md),
[`ubs()`](https://stantraykov.github.io/fundsr/reference/ubs.md),
[`vang()`](https://stantraykov.github.io/fundsr/reference/vang.md),
[`xtra()`](https://stantraykov.github.io/fundsr/reference/xtra.md)
