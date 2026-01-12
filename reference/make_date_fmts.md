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
  `"d"`, `"y"`, and exactly one of `"m"` or `"M"` (e.g. `"dmy"`,
  `"ymd"`, `"dMy"`, `"Mdy"`).

## Value

A character vector of unique date format strings.

## Details

The returned formats cover common separators (`"/"`, `"-"`, `"."`,
`" "`), unseparated dates with numeric months, and an optional comma
before a final 4- or 2-digit year element (`%y`, `%Y`) in
space-separated dates.

Month handling depends on `order`:

- If `order` uses `"m"`, month tokens include numeric months (`%m`) and
  abbreviated month names (`%b`).

- If `order` uses `"M"`, month tokens include full month names (`%B`).

Month name formats (`%b`, `%B`) are locale-dependent.
