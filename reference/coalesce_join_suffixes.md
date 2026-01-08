# Coalesce suffixed join columns into unsuffixed base columns

Helper for post-processing join results that use suffixes such as `.x` /
`.y`. For each base name that appears with both suffixes (e.g. `FTAW.x`
and `FTAW.y`), this function creates a new column `FTAW` as
`dplyr::coalesce(FTAW.x, FTAW.y)` and drops the suffixed columns. The
order of `suffix` controls which column is preferred.

## Usage

``` r
coalesce_join_suffixes(df, suffixes = c(".x", ".y"))
```

## Arguments

- df:

  A data frame or tibble produced by joins.

- suffixes:

  Character vector of length 2 giving the suffixes to coalesce, in
  priority order. For example, `c(".x", ".y")` uses `.x` as the primary
  source and falls back to `.y` when `.x` is `NA`.

## Value

A tibble with the same rows as `df`, where any pairs of suffixed columns
(e.g. `name.x` / `name.y`) are replaced by a single unsuffixed column
(e.g. `name`) containing the coalesced values.
