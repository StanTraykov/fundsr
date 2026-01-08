# Add entries to the fund download list

Adds one or more named download specifications to the `fundsr.fund_urls`
option. Existing entries are preserved; entries in `x` replace any
existing entries with the same name.

## Usage

``` r
add_fund_urls(x)
```

## Arguments

- x:

  A named character vector mapping download identifiers to URLs.

## Value

A list with the previous value of `fundsr.fund_urls`.

## Details

Names are converted to uppercase before storing.

## See also

[`fundsr_options()`](https://stantraykov.github.io/fundsr/reference/fundsr_options.md)
to set `fundsr.fund_urls` (and other fundsr options) in one call.
[`download_fund_data()`](https://stantraykov.github.io/fundsr/reference/download_fund_data.md)
to download files from the configured URLs.
