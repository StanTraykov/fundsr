# Register a data loader

Appends `fun` to the internal data-loader registry
(`.fundsr$data_loaders`). Registered functions are intended to be run
sequentially in registration order.

## Usage

``` r
add_data_loader(fun)
```

## Arguments

- fun:

  A function to register. Must take no arguments.

## Value

Invisibly returns the updated `.fundsr$data_loaders` list.

## Details

If a loader with the same function body is already registered, `fun` is
not added again.

## Examples

``` r
add_data_loader(function() NULL)
```
