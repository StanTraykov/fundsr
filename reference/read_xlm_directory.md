# Read and combine XLM data from multiple Xetra XLSX files

Scans a directory for Xetra ETF XLSX files, extracts the XLM column and
associated metadata, and returns a combined data frame with standardized
column names and parsed dates.

## Usage

``` r
read_xlm_directory(directory, header_rows = c(4, 5), col_types = NULL)
```

## Arguments

- directory:

  Directory containing the XLSX files to read.

- header_rows:

  Integer vector indicating which rows contain header information for
  `read_xlsx_hdr()`.

- col_types:

  Optional column type specification passed to `read_xlsx_hdr()`.

## Value

A data frame containing combined XLM records from all files in the
directory.

## Details

Each XLSX file is processed with `read_xlsx_hdr()`, filtered to the
relevant XLM, ticker, and product-name fields (stored in columns `xlm`,
`ticker`, `name`, and `date`). The Month–Year embedded in the XLM column
name is parsed and used as the observation date.

## See also

Other XLM functions:
[`plot_xlms()`](https://stantraykov.github.io/fundsr/reference/plot_xlms.md)
