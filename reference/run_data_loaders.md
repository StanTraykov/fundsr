# Run registered data loaders

Runs the data loader registry (`.fundsr$data_loaders`) to populate (or
refresh) the package's storage environment (`.fundsr_storage`).

## Usage

``` r
run_data_loaders(reload = FALSE)
```

## Arguments

- reload:

  Logical scalar. If `TRUE`, forces a full reload by setting
  `options(fundsr.reload = TRUE)` for the duration of this call.

## Value

Invisibly returns `.fundsr_storage` after running the data loaders.

## Details

The function temporarily sets the `fundsr.reload` option so that data
loaders can decide whether to recompute cached objects.

The previous value of `getOption("fundsr.reload")` is restored on exit,
even if a data loader errors.

Data loaders are taken from `.fundsr$data_loaders` and are called
sequentially in registration order. Each registered function must take
no arguments.
