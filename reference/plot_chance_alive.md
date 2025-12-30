# Plot chance alive by age

Plots conditional survival curves produced by
[`chance_alive()`](https://stantraykov.github.io/fundsr/reference/chance_alive.md).
The most recent year is highlighted in black; earlier years are shown
with a teal-to-orange-to-light gradient. Horizontal reference lines mark
10% and 5% survival levels.

## Usage

``` r
plot_chance_alive(ca, sex = c("m", "f"), population)
```

## Arguments

- ca:

  A tibble as returned by
  [`chance_alive()`](https://stantraykov.github.io/fundsr/reference/chance_alive.md),
  with columns `Year`, `Age`, and `chance_alive`.

- sex:

  Sex code: `"m"` (male) or `"f"` (female). Used for labeling.

- population:

  Population label/code to display in the subtitle (e.g. `"BGR"`,
  `"USA"`, `"DEUTNP"`).

## Value

A ggplot object.

## Examples

``` r
if (FALSE) { # \dontrun{
lt_m <- read_life_table(file.path("data", "life"), sex = "m", look_back = 10)
ca <- chance_alive(lt_m, pop_name = "BGR", age0 = 27)
p <- plot_chance_alive(ca, sex = "m", population = "BGR")
p
} # }
```
