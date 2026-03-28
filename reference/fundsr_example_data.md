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
#> [1] "/tmp/RtmpOXdWjF/temp_libpath1e23798c0a23/fundsr/extdata/FNDA.xlsx"
fundsr_example_data()
#> [1] "/tmp/RtmpOXdWjF/temp_libpath1e23798c0a23/fundsr/extdata/."
```
