# Clear fund-index map

Clears the fund-index map stored in `session$state$fund_index_map`.

## Usage

``` r
clear_fund_index_map(session = NULL)
```

## Arguments

- session:

  Optional `fundsr_session` object. Defaults to the package default
  session when `NULL`.

## Value

Invisibly returns `NULL`. Called for side effects.

## See also

Other fund-index map functions:
[`add_fund_index_map()`](https://stantraykov.github.io/fundsr/reference/add_fund_index_map.md),
[`get_fund_index_map()`](https://stantraykov.github.io/fundsr/reference/get_fund_index_map.md)
