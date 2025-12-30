# Join all tibbles in an environment with optional late left-joins

Performs a
[`dplyr::full_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html)
across all objects in an environment, except those listed in `late`.
Objects listed in `late` are instead joined afterwards using
[`dplyr::left_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html)
in the order given. Optionally, any columns created with join suffixes
(such as `.x` / `.y`) can be automatically coalesced into single
unsuffixed columns.

## Usage

``` r
join_env(env, by, late = NULL, coalesce_suffixed = NULL)
```

## Arguments

- env:

  Environment containing the tibbles to join.

- by:

  Character vector of join keys (passed to
  [`dplyr::full_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html)).

- late:

  Character vector of object names in `env` that should be *excluded*
  from the initial full join and instead left-joined afterwards.
  Defaults to `NULL`.

- coalesce_suffixed:

  Optional character vector of length 2 giving join suffixes to coalesce
  (for example, `c(".x", ".y")` or `c(".y", ".x")`). When non-`NULL`,
  any pairs of columns whose names end in these suffixes (and share the
  same base name) are replaced by a single unsuffixed column containing
  the coalesced values. If `NULL` (the default), no automatic coalescing
  is performed.

## Value

A tibble: the full join of all non-late objects, followed by sequential
left-joins of the late objects. If `coalesce_suffixed` is supplied,
suffixed join columns are coalesced into unsuffixed base columns as
described above.

## Details

This helper is designed for workflows where the majority of tables
should be fully joined, while certain sparse or auxiliary tables (e.g.
calendars, metadata) should only be attached via `left_join()`.

When `coalesce_suffixed` is provided, the function uses an internal
helper to replace pairs like `name.x` / `name.y` with a single `name`
column whose values are taken from the first suffix in
`coalesce_suffixed` and then, where missing, from the second.

If a name in `late` does not exist in `env`, it is ignored with a
warning.

## Examples

``` r
if (FALSE) { # \dontrun{
  e <- new.env()
  e$a <- tibble::tibble(date = 1:3, x = 1:3)
  e$b <- tibble::tibble(date = 1:3, x = c(NA, 20, 30))

  # Simple full join of a and b
  join_env(e, by = "date")

  # Full join with automatic coalescing of x.x / x.y into x
  join_env(e, by = "date", coalesce_suffixed = c(".x", ".y"))
} # }
```
