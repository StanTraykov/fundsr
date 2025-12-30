# Read Eurostat EUROPOP2023 mortality-assumption table (proj_23naasmr)

Reads the Eurostat TSV export for EUROPOP2023 age-specific mortality
rate assumptions (dataset `proj_23naasmr`), typically downloaded as
`estat_proj_23naasmr.tsv.gz`. Returns a tidy long table with metadata
columns plus numeric `Age`, `Year`, and `mx`.

## Usage

``` r
read_es_aasmr(directory)
```

## Arguments

- directory:

  Directory containing `estat_proj_23naasmr.tsv.gz`.

## Value

A tibble with columns: `freq`, `projection`, `sex`, `unit`, `geo`,
`age`, `Age`, `Year`, `mx`.

## Details

Eurostat value flags (e.g. provisional/estimated markers) are tolerated:
the numeric part is parsed into `mx`, while missing values encoded as
`:` are returned as `NA`.

The Eurostat `age` dimension uses codes like `Y_LT1` (age \< 1), `Y1`
(age 1), and `Y_GE85` (age 85+). This function maps `Y_LT1` to `Age = 0`
and parses `Y{n}` and `Y_GE{n}` to integer ages.

## Examples

``` r
if (FALSE) { # \dontrun{
es_aasmr <- read_es_aasmr(file.path("data", "life"))
es_aasmr %>% dplyr::count(geo, sex, projection, sort = TRUE)
} # }
```
