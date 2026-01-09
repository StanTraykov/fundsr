# Download fund data according to the configured download list

Retrieves all fund data files listed in the `fundsr.fund_urls` option
and saves them into the directory specified by `fundsr.data_dir`.

## Usage

``` r
download_fund_data(redownload = FALSE)
```

## Arguments

- redownload:

  Logical; if `TRUE`, existing files are overwritten. If `FALSE`, only
  missing files are downloaded.

## Value

Invisibly returns `NULL`. Files are written as a side effect.

## See also

[`add_fund_urls()`](https://stantraykov.github.io/fundsr/reference/add_fund_urls.md)
to add/update entries in `fundsr.fund_urls`.

Other download functions:
[`add_fund_urls()`](https://stantraykov.github.io/fundsr/reference/add_fund_urls.md)
