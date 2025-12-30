# Compute cohort-style survival from Eurostat EUROPOP2023 mortality assumptions

Computes conditional survival (chance alive) for a person aged `age0` in
`start_year` using Eurostat EUROPOP2023 age-specific mortality rate
assumptions (dataset `proj_23naasmr`). The computation follows a cohort
path (diagonal): for age `age0 + k` it uses the mortality rate for year
`start_year + k`.

## Usage

``` r
chance_alive_es_aasmr(es, geo, sex, age0, start_year = NULL)
```

## Arguments

- es:

  A tibble as returned by
  [`read_es_aasmr()`](https://stantraykov.github.io/fundsr/reference/read_es_aasmr.md).

- geo:

  Eurostat geo code (e.g. `"BG"`, `"NL"`).

- sex:

  Sex code: `"m"` or `"f"` (case-insensitive).

- age0:

  Baseline age (integer).

- start_year:

  Starting calendar year (integer). If `NULL`, uses the earliest year
  available for the selected `geo/sex`.

## Value

A tibble with columns `geo`, `sex`, `projection`, `Year`, `Age`, `mx`,
`qx`, `chance_alive`.

## Details

The result includes two projection variants: baseline (`BSL`) and lower
mortality (`LMRT`).

## Examples

``` r
if (FALSE) { # \dontrun{
es <- read_es_aasmr(file.path("data", "life"))
ca <- chance_alive_es_aasmr(es, geo = "NL", sex = "m", age0 = 42, start_year = 2022)
p <- plot_chance_alive_es_aasmr(ca, sex = "m", population = "NL")
p
} # }
```
