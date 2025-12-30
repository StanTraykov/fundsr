# Generate candidate date format strings for `as.Date()`

Builds a set of possible format strings suitable for
[`as.Date()`](https://rdrr.io/r/base/as.Date.html) for a given
day/month/year order. This is intended for fast "detect once per sheet,
then parse all" workflows.

## Usage

``` r
make_date_fmts(order)
```

## Arguments

- order:

  A single string specifying the component order as a permutation of
  `"d"`, `"m"`, `"y"` (e.g. `"dmy"`, `"mdy"`, `"ymd"`).

## Value

A character vector of unique date format strings.

## Details

The returned formats cover common separators (`"/"`, `"-"`, `"."`,
`" "`) and both 4-digit (`%Y`) and 2-digit (`%y`) years. Month tokens
include numeric months (`%m`) and abbreviated month names (`%b`). Full
month names (`%B`) are excluded.
