# Plot cohort-style survival from Eurostat EUROPOP2023 assumptions

Plots the conditional survival curves returned by
[`chance_alive_es_aasmr()`](https://stantraykov.github.io/fundsr/reference/chance_alive_es_aasmr.md).
The baseline projection (`BSL`) is shown in black and the
lower-mortality variant (`LMRT`) is shown in dark cyan.

## Usage

``` r
plot_chance_alive_es_aasmr(ca, sex = c("m", "f"), population)
```

## Arguments

- ca:

  A tibble as returned by
  [`chance_alive_es_aasmr()`](https://stantraykov.github.io/fundsr/reference/chance_alive_es_aasmr.md),
  with columns `projection`, `Age`, and `chance_alive` (and typically
  `geo`, `sex`, `Year`).

- sex:

  Sex code: `"m"` or `"f"` (case-insensitive). Used for labeling.

- population:

  Population label/code to display in the subtitle (e.g. `"BG"`).

## Value

A ggplot object.

## Examples

``` r
if (FALSE) { # \dontrun{
es_aasmr <- read_es_aasmr(file.path("data", "life"))
ca <- chance_alive_es_aasmr(es_aasmr, geo = "BG", sex = "m", age0 = 42, start_year = 2022)
p <- plot_chance_alive_es_aasmr(ca, sex = "m", population = "BG")
p
} # }
```
