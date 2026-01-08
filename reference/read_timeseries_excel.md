# Read a time series from an Excel workbook

Reads an Excel sheet, detects the header row by searching for a date
header, parses the date column, selects/renames value columns by regex,
and optionally coerces value columns to numeric.

## Usage

``` r
read_timeseries_excel(
  file,
  sheet,
  date_col,
  col_trans,
  date_order = "dmy",
  force_numeric = TRUE,
  comma_rep = "."
)
```

## Arguments

- file:

  Path to the Excel workbook. Typically you pass a filename relative to
  `getOption("fundsr.data_dir")`, or an absolute path.

- sheet:

  Sheet identifier to read from (sheet name or 1-based index).

- date_col:

  String used to detect the header row and identify the date column
  (matched via regex against cell contents for header-row detection, and
  against column names after headers are assigned).

- col_trans:

  Named character vector (or list) mapping output column names to regex
  patterns used to select columns from the sheet. Names are returned
  column names; values are patterns matched against header names.

- date_order:

  Character scalar indicating day/month/year order used to generate
  candidate date formats for parsing text dates (passed to
  [`make_date_fmts()`](https://stantraykov.github.io/fundsr/reference/make_date_fmts.md)).
  Default is `"dmy"`.

- force_numeric:

  Logical. If `TRUE` (default), always replace matched value columns
  with their numeric coercions (non-parsable values become `NA`). If
  `FALSE`, only replace when coercion succeeds for all non-`NA` values.

- comma_rep:

  Character scalar used when converting character numerics: commas are
  replaced by this string before conversion. Default `"."` (treat comma
  as decimal separator).

## Value

A tibble with a `date` column (class `Date`) and the selected value
columns (possibly numeric), with names determined by `col_trans`.

## Details

The sheet is read using `read_excel_or_xml()` (tries `readxl` first,
then an XML fallback). Completely empty columns are dropped. The first
row containing `date_col` (any cell match) is treated as the header row;
data starts below it.

Date parsing:

- If the detected date column is numeric (or looks numeric), it is
  interpreted as an Excel serial date with origin `"1899-12-30"`.

- Otherwise the date strings are cleaned (truncated to 24 chars,
  `"Sept"` → `"Sep"`, trailing `" 12:00:00 AM"` removed) and parsed with
  [`as.Date()`](https://rdrr.io/r/base/as.Date.html) using formats from
  `make_date_fmts(date_order)`. After parsing, the function drops all
  rows from the first unparseable date onward (i.e., it truncates at the
  first `NA` date), then filters remaining `NA` dates.

Column selection/renaming: `col_trans` maps desired output names to
regex patterns matched against the detected header names. If a pattern
matches multiple columns, they are kept and suffixed (`name`, `name2`,
`name3`, ...).

Numeric coercion: For non-date columns, character values have `"$"` /
`"USD "` stripped, commas replaced by `comma_rep`, then are converted
with [`as.numeric()`](https://rdrr.io/r/base/numeric.html). If
`force_numeric = TRUE`, the converted numeric column is kept even if
some values fail to parse; otherwise the column is only replaced when
all non-`NA` values parse successfully.

## See also

[`read_timeseries()`](https://stantraykov.github.io/fundsr/reference/read_timeseries.md)
for CSV/TSV time series import.

## Examples

``` r
if (FALSE) { # \dontrun{
x <- read_timeseries_excel(
  file = "example.xlsx",
  sheet = 1,
  date_col = "^Date$",
  col_trans = c(nav = "NAV", tr = "TR"),
  date_order = "dmy"
)
} # }
```
