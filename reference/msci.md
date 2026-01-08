# Import an MSCI index sheet and register benchmark mappings

Wrapper around
[`store_timeseries()`](https://stantraykov.github.io/fundsr/reference/store_timeseries.md)
and
[`read_timeseries_excel()`](https://stantraykov.github.io/fundsr/reference/read_timeseries_excel.md)
for MSCI index files.

## Usage

``` r
msci(var_name, col_trans, benchmarks = NULL, file)
```

## Arguments

- var_name:

  Storage key used in `.fundsr_storage`.

- col_trans:

  Named vector specifying column translations.

- benchmarks:

  Optional index mapping to record in the fund index map (used to map
  gross to net indices).

- file:

  Filename of the XLSX file to import.

## Value

Invisibly returns `NULL`. Data are stored via
[`store_timeseries()`](https://stantraykov.github.io/fundsr/reference/store_timeseries.md).

## See also

[`store_timeseries()`](https://stantraykov.github.io/fundsr/reference/store_timeseries.md),
[`read_timeseries_excel()`](https://stantraykov.github.io/fundsr/reference/read_timeseries_excel.md)
