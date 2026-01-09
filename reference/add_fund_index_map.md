# Add to fund-index map

Merges fund-index pairs into the fund-index map
(`.fundsr$fund_index_map`). Existing entries with the same names are
replaced.

## Usage

``` r
add_fund_index_map(fund_index_map)
```

## Arguments

- fund_index_map:

  Named vector or list of fund-index pairs to merge into
  `.fundsr$fund_index_map`. Names are fund identifiers; values are index
  identifiers.

## Value

Invisibly returns `NULL`. Called for side effects.

## See also

Other fund-index map mutators:
[`clear_fund_index_map()`](https://stantraykov.github.io/fundsr/reference/clear_fund_index_map.md),
[`get_fund_index_map()`](https://stantraykov.github.io/fundsr/reference/get_fund_index_map.md)

## Examples

``` r
add_fund_index_map(c(fund1 = "INDEX1", fund2 = "INDEX2", fund3 = "INDEX2"))
```
