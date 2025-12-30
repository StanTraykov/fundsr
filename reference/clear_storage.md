# Clear storage

Removes all objects from the package's storage environment
(`.fundsr_storage`). Optionally also clears the fund/index map
(`.fundsr$fund_index_map`).

## Usage

``` r
clear_storage(clear_fund_index_map = FALSE)
```

## Arguments

- clear_fund_index_map:

  Logical scalar; if `TRUE`, also clears `.fundsr$fund_index_map`.

## Value

Invisibly returns `NULL`. Called for side effects.

## Examples

``` r
clear_storage()
clear_storage(clear_fund_index_map = TRUE)
```
