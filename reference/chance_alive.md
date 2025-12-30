# Compute conditional survival (chance alive) by age

Computes the conditional probability of being alive at each age
`x >= age0`, given survival to `age0`, from an HMD-style period life
table. For each year, the returned series is:
`chance_alive(x | age0) = lx(x) / lx(age0)`.

## Usage

``` r
chance_alive(lt, pop_name, age0)
```

## Arguments

- lt:

  A life table tibble as returned by
  [`read_life_table()`](https://stantraykov.github.io/fundsr/reference/read_life_table.md),
  containing at least `PopName`, `Year`, `Age`, and `lx`.

- pop_name:

  Population code (HMD `PopName`) to filter within `lt` (e.g. `"BGR"`,
  `"USA"`, `"DEUTNP"`).

- age0:

  Baseline age (integer). Returned ages start at `age0`.

## Value

A tibble with columns `Year`, `Age`, and `chance_alive`, sorted by
`Year` then `Age`.

## Examples

``` r
if (FALSE) { # \dontrun{
lt_m <- read_life_table(file.path("data", "life"), sex = "m", look_back = 10)
ca <- chance_alive(lt_m, pop_name = "BGR", age0 = 27)
ca
} # }
```
