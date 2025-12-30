# Clear registered data loaders

Clears the internal data-loader registry (`.fundsr$data_loaders`),
removing all previously registered data loader functions.

## Usage

``` r
clear_data_loaders()
```

## Value

Invisibly returns `NULL`. Called for side effects.

## Examples

``` r
clear_data_loaders()
```
