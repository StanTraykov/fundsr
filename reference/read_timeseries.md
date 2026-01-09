# Read a time series file (CSV/TSV) with a date + one or more value columns

Loads a delimited file from the directory specified by
`fundsr.data_dir`, parses the date column into a proper `Date`, and
coerces all other columns to numeric.

## Usage

``` r
read_timeseries(
  file,
  date_col = "date",
  time_unit = c("ms", "s", "us", "ns"),
  orders = NULL,
  force_text_date = FALSE
)
```

## Arguments

- file:

  Filename to read (relative to `fundsr.data_dir` option).

- date_col:

  Name of the date column in the file.

- time_unit:

  Character scalar giving the unit of a *numeric* date column (Unix
  epoch). One of `"ms"` (default), `"s"`, `"us"`, `"ns"`.

- orders:

  Character vector of lubridate parsing orders for a *text* date column
  (passed to
  [`lubridate::parse_date_time()`](https://lubridate.tidyverse.org/reference/parse_date_time.html)).
  If `NULL`, a default set of common dmy-order formats is used.

- force_text_date:

  Logical scalar. If `TRUE`, the date column is always parsed as text
  using `orders` (no Unix-epoch numeric interpretation is attempted).

## Value

A tibble with parsed date column and numeric value columns.

## Details

The reader is chosen by file extension: `.csv` uses
[`readr::read_csv()`](https://readr.tidyverse.org/reference/read_delim.html)
and `.tsv`/`.tab`/`.txt` uses
[`readr::read_tsv()`](https://readr.tidyverse.org/reference/read_delim.html).
Gzipped variants such as `.csv.gz` and `.tsv.gz` are also supported.

The function assumes a date column exists (default: `date`). By default,
if the date column looks numeric (i.e., coercion to numeric yields at
least one non-`NA`), it is interpreted as a Unix timestamp (scaled by
`time_unit`). Otherwise it is parsed as text using `orders`. If
`force_text_date = TRUE`, it is always parsed as text using `orders`.

All non-date columns are coerced with
[`as.numeric()`](https://rdrr.io/r/base/numeric.html) (non-parsable
values become `NA`).

## See also

Other fund/index file readers:
[`read_msci_tsv()`](https://stantraykov.github.io/fundsr/reference/read_msci_tsv.md),
[`read_timeseries_excel()`](https://stantraykov.github.io/fundsr/reference/read_timeseries_excel.md)
