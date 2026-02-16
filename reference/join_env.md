# Join all data frames in an environment with optional late joins

Performs a
[`dplyr::full_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html)
across all objects in `env` (in alphabetical order), excluding any
listed in `late`. Late objects are then joined sequentially (via
[`dplyr::left_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html)
by default) in the order given. Full-join clashes use suffixes
`c(".x", ".y")`; late joins use `c(".early", ".late")`.

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

  Environment containing *only* data frames (incl. tibbles) to join.

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

  Deprecated; use `join_precedence` (same meaning).

- late_join:

  Function to use for joining late objects, e.g.
  [`dplyr::left_join`](https://dplyr.tidyverse.org/reference/mutate-joins.html)
  (the default). Must accept dplyr-style `suffix` and `by` arguments.

## Value

A tibble: the full join of all non-late objects, followed by sequential
left-joins (or other joins specified by `late_join`) of the late
objects. If `join_precedence` is supplied, suffixed join columns are
coalesced into unsuffixed base columns as described above.

## Details

Optionally, column pairs with specified suffixes can be coalesced into
unsuffixed base columns via `join_precedence`.

## See also

Other fund/index workflow functions:
[`add_data_loader()`](https://stantraykov.github.io/fundsr/reference/add_data_loader.md),
[`build_all_series()`](https://stantraykov.github.io/fundsr/reference/build_all_series.md),
[`clear_data_loaders()`](https://stantraykov.github.io/fundsr/reference/clear_data_loaders.md),
[`clear_storage()`](https://stantraykov.github.io/fundsr/reference/clear_storage.md),
[`get_storage()`](https://stantraykov.github.io/fundsr/reference/get_storage.md),
[`load_fund()`](https://stantraykov.github.io/fundsr/reference/load_fund.md),
[`run_data_loaders()`](https://stantraykov.github.io/fundsr/reference/run_data_loaders.md),
[`store_timeseries()`](https://stantraykov.github.io/fundsr/reference/store_timeseries.md)

## Examples

``` r
  e <- new.env()
  e$members <- dplyr::band_members
  e$instruments <- dplyr::band_instruments
  e$other_instr <- dplyr::band_instruments |>
      dplyr::mutate(plays = dplyr::case_match(name,
                                              "John" ~ "banjo",
                                              "Paul" ~ "mellotron",
                                              "Keith" ~ "harpsichord")) |>
      dplyr::add_row(name = "Mick", plays = "harmonica") |>
      dplyr::add_row(name = "Stu", plays = "piano")
#> Warning: There was 1 warning in `dplyr::mutate()`.
#> ℹ In argument: `plays = dplyr::case_match(...)`.
#> Caused by warning:
#> ! `case_match()` was deprecated in dplyr 1.2.0.
#> ℹ Please use `recode_values()` instead.

  full <- join_env(e, by = "name")
#> Joining: instruments, members, other_instr.
  late <- join_env(e, by = "name", late = "other_instr")
#> Joining: instruments, members (late = other_instr).
  late_coalesced <- join_env(e,
                             by = "name",
                             late = "other_instr",
                             join_precedence = c(".early", ".late"))
#> Joining: instruments, members (late = other_instr).
  print(list(full = full, late = late, late_coalesced = late_coalesced))
#> $full
#> # A tibble: 5 × 4
#>   name  plays.x band    plays.y    
#>   <chr> <chr>   <chr>   <chr>      
#> 1 John  guitar  Beatles banjo      
#> 2 Paul  bass    Beatles mellotron  
#> 3 Keith guitar  NA      harpsichord
#> 4 Mick  NA      Stones  harmonica  
#> 5 Stu   NA      NA      piano      
#> 
#> $late
#> # A tibble: 4 × 4
#>   name  plays.early band    plays.late 
#>   <chr> <chr>       <chr>   <chr>      
#> 1 John  guitar      Beatles banjo      
#> 2 Paul  bass        Beatles mellotron  
#> 3 Keith guitar      NA      harpsichord
#> 4 Mick  NA          Stones  harmonica  
#> 
#> $late_coalesced
#> # A tibble: 4 × 3
#>   name  band    plays    
#>   <chr> <chr>   <chr>    
#> 1 John  Beatles guitar   
#> 2 Paul  Beatles bass     
#> 3 Keith NA      guitar   
#> 4 Mick  Stones  harmonica
#> 
```
