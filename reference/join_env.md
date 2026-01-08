# Join all tibbles in an environment with optional late left-joins

Performs a
[`dplyr::full_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html)
across all objects in an environment, except those listed in `late`.
Objects listed in `late` are instead joined afterwards using
[`dplyr::left_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html)
(or another function specified via `late_join`) in the order given.
Column clashes during the full join are resolved via join suffixes
`c(".x", ".y")` while the late joins use `c(".early", ".late")`.
Optionally, columns with join suffixes can be automatically coalesced
into single unsuffixed columns via precedence specification
(`join_precedence`).

## Usage

``` r
join_env(
  env,
  by = "date",
  late = NULL,
  join_precedence = NULL,
  coalesce_suffixed = NULL,
  late_join = dplyr::left_join
)
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

- join_precedence:

  Optional character vector of length 2 giving join suffixes to coalesce
  (for example, `c(".early", ".late")` or `c(".late", ".early")`). When
  non-`NULL`, any pairs of columns whose names end in these suffixes
  (and share the same base name) are replaced by a single unsuffixed
  column containing the coalesced values, preferring the left suffix
  when both values are available. If `NULL` (the default), no automatic
  coalescing is performed.

- coalesce_suffixed:

  Deprecated; use `join_precedence`.

- late_join:

  Function to use for joining late objects.

## Value

A tibble: the full join of all non-late objects, followed by sequential
left-joins of the late objects. If `join_precedence` is supplied,
suffixed join columns are coalesced into unsuffixed base columns as
described above.

## Examples

``` r
  e <- new.env()
  e$members <- dplyr::band_members
  e$instruments <- dplyr::band_instruments
  e$other_instr <- dplyr::band_instruments |>
      dplyr::mutate(plays = c("banjo", "mellotron", "harpsichord"))

  full <- join_env(e, by = "name")
#> Joining: instruments, other_instr, members
  late <- join_env(e, by = "name", late = "other_instr")
#> Joining: instruments, other_instr, members
  late_coalesced <- join_env(e,
                             by = "name",
                             late = "other_instr",
                             join_precedence = c(".late", ".early"))
#> Joining: instruments, other_instr, members
  print(list(full = full, late = late, late_coalesced = late_coalesced))
#> $full
#> # A tibble: 4 × 4
#>   name  plays.x plays.y     band   
#>   <chr> <chr>   <chr>       <chr>  
#> 1 John  guitar  banjo       Beatles
#> 2 Paul  bass    mellotron   Beatles
#> 3 Keith guitar  harpsichord NA     
#> 4 Mick  NA      NA          Stones 
#> 
#> $late
#> # A tibble: 4 × 4
#>   name  plays.early band    plays.late 
#>   <chr> <chr>       <chr>   <chr>      
#> 1 John  guitar      Beatles banjo      
#> 2 Paul  bass        Beatles mellotron  
#> 3 Keith guitar      NA      harpsichord
#> 4 Mick  NA          Stones  NA         
#> 
#> $late_coalesced
#> # A tibble: 4 × 3
#>   name  band    plays      
#>   <chr> <chr>   <chr>      
#> 1 John  Beatles banjo      
#> 2 Paul  Beatles mellotron  
#> 3 Keith NA      harpsichord
#> 4 Mick  Stones  NA         
#> 
```
