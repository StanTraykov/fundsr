# Import an Invesco fund

Wrapper around
[`load_fund()`](https://stantraykov.github.io/fundsr/reference/load_fund.md)
for Invesco files.

## Usage

``` r
inve(ticker, file = NULL, benchmark = NULL, retrieve_benchmark = FALSE)
```

## Arguments

- ticker:

  Fund ticker.

- file:

  Optional filename override.

- benchmark:

  Optional benchmark key.

- retrieve_benchmark:

  Logical; also import benchmark column.
