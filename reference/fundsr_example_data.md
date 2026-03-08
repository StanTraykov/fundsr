# Get the path to an example file shipped with the package.

Get the path to an example file shipped with the package.

## Usage

``` r
fundsr_example_data(file = ".")
```

## Arguments

- file:

  The name of the example file.

## Examples

``` r
fundsr_example_data("FNDA.xlsx")
#> [1] "/tmp/Rtmp7szwZq/temp_libpath1ba9488eb9cd/fundsr/extdata/FNDA.xlsx"
fundsr_example_data()
#> [1] "/tmp/Rtmp7szwZq/temp_libpath1ba9488eb9cd/fundsr/extdata/."
```
