# Load and join all series from registered data loaders

Runs
[`run_data_loaders()`](https://stantraykov.github.io/fundsr/reference/run_data_loaders.md),
joins the resulting environment into a single data frame via
[`join_env()`](https://stantraykov.github.io/fundsr/reference/join_env.md),
and sorts the result by `by`.

## Usage

``` r
load_all_series(by = "date", ...)
```

## Arguments

- by:

  Column name to join by and to sort by. Defaults to `"date"`.

- ...:

  Additional arguments forwarded to
  [`join_env()`](https://stantraykov.github.io/fundsr/reference/join_env.md)
  (e.g. `late =`, `coalesce_suffixed =`, etc.).

## Value

A tibble containing all joined series, sorted by `by`.

## Details

This function is a convenience wrapper for the common workflow used in
examples and vignettes.

## Examples

``` r
if (FALSE) { # \dontrun{

s1 <- load_all_series()
download_fund_data(redownload = TRUE)
s2 <- load_all_series(by = "date", late = "ftaw", coalesce_suffixed = c(".y", ".x")) %>%
  filter(date >= as_date("2013-01-01"))
} # }
```
