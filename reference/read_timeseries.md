# Read a time series file (CSV/TSV) with a date + one or more value columns

Loads a delimited file from the directory specified by
`fundsr.data_dir`, converts the `date` column to a proper Date, and
coerces all other columns to numeric.

## Usage

``` r
read_timeseries(file, time_unit = c("ms", "s", "us", "ns"))
```

## Arguments

- file:

  Filename to read (relative to `getOption("fundsr.data_dir")`).

- time_unit:

  Character scalar giving the unit of the numeric `date` column (Unix
  epoch). One of `"ms"` (default), `"s"`, `"us"`, `"ns"`.

## Value

A tibble with parsed `date` and numeric value columns.

## Details

The reader is chosen by file extension: `.csv` uses
[`readr::read_csv()`](https://readr.tidyverse.org/reference/read_delim.html)
and `.tsv`/`.tab`/`.txt` uses
[`readr::read_tsv()`](https://readr.tidyverse.org/reference/read_delim.html).
Gzipped variants such as `.csv.gz` and `.tsv.gz` are also supported.

The function assumes a column named `date` exists and represents a Unix
timestamp. All non-`date` columns are coerced with
[`as.numeric()`](https://rdrr.io/r/base/numeric.html) (non-parsable
values become `NA`).
