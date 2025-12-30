# Read an MSCI two-column TSV file

Extracts the data portion of an MSCI TSV file—skipping header noise—and
reads it as a two-column table containing a date and a numeric value.

## Usage

``` r
read_msci_tsv(file)
```

## Arguments

- file:

  Filename of the TSV to read (relative to
  `getOption("fundsr.data_dir")`).

## Value

A tibble with a `Date` column and one numeric column.

## Details

The function filters lines beginning with a digit (date rows) or the
literal `"Date"`, then parses them using a fixed `%m/%d/%Y` date format
and a numeric second field.
