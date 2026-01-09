# Read HMD period life tables (1x1) from disk

Reads a Human Mortality Database (HMD) period life table file (1x1, by
single year of age) for the selected sex and returns only the last
`look_back` years based on the latest year present in the file. The
open-ended age group (e.g. `"110+"`) is parsed as its numeric lower
bound (e.g. `110`).

## Usage

``` r
read_life_table(directory, sex = c("f", "m"), look_back = 20)
```

## Arguments

- directory:

  Directory containing the HMD life table files (`"mltper_1x1.txt"` /
  `"fltper_1x1.txt"` or gzipped variants `"mltper_1x1.txt.gz"` /
  `"fltper_1x1.txt.gz"`).

- sex:

  Sex code: `"m"` (male) or `"f"` (female).

- look_back:

  Number of most recent years to keep (inclusive of the latest available
  year). Must be \>= 1.

## Value

A tibble with columns: `PopName`, `Year`, `Age`, `mx`, `qx`, `ax`, `lx`,
`dx`, `Lx`, `Tx`, `ex`. `Age` is returned as integer.

## See also

Other survival curve functions:
[`chance_alive()`](https://stantraykov.github.io/fundsr/reference/chance_alive.md),
[`chance_alive_es_aasmr()`](https://stantraykov.github.io/fundsr/reference/chance_alive_es_aasmr.md),
[`plot_chance_alive()`](https://stantraykov.github.io/fundsr/reference/plot_chance_alive.md),
[`plot_chance_alive_es_aasmr()`](https://stantraykov.github.io/fundsr/reference/plot_chance_alive_es_aasmr.md),
[`read_es_aasmr()`](https://stantraykov.github.io/fundsr/reference/read_es_aasmr.md)

## Examples

``` r
if (FALSE) { # \dontrun{
lt_m <- read_life_table(file.path("data", "life"), sex = "m", look_back = 20)
lt_m %>% dplyr::distinct(PopName) %>% dplyr::arrange(PopName)
} # }
```
