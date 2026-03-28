# Add entries to the fund download list

Adds one or more named download specifications to the `fundsr.fund_urls`
option. Existing entries are preserved; entries in `fund_urls` replace
any existing entries with the same name.

## Usage

``` r
add_fund_urls(fund_urls)
```

## Arguments

- fund_urls:

  A named character vector mapping download identifiers to URLs.

## Value

Invisibly returns a named list (as returned by
[`fundsr_options()`](https://stantraykov.github.io/fundsr/reference/fundsr_options.md))
containing the previous value of `fundsr.fund_urls`.

## Details

Names are converted to uppercase before storing.

## See also

[`fundsr_options()`](https://stantraykov.github.io/fundsr/reference/fundsr_options.md)
to set `fundsr.fund_urls` and other fundsr options in one call.
[`download_fund_data()`](https://stantraykov.github.io/fundsr/reference/download_fund_data.md)
to download files from the added URLs.

Other download functions:
[`download_fund_data()`](https://stantraykov.github.io/fundsr/reference/download_fund_data.md)
