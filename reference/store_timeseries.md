# Store a cached object in the package storage environment

Evaluates an expression and assigns its result into the package storage
environment (`.fundsr_storage`) under the given variable name. The value
is recomputed only if it is missing or if the global option
`fundsr.reload` is set to `TRUE`. Optionally updates the fund/index map.

## Usage

``` r
store_timeseries(var_name, expr, fund_index_map = NULL)
```

## Arguments

- var_name:

  Name of the variable to store in `.fundsr_storage`.

- expr:

  An expression to evaluate when (re)computing the value.

- fund_index_map:

  Optional named vector or list of fund/index pairs to add to
  `.fundsr$fund_index_map`.

## Value

Invisibly returns `NULL`. Called for side effects.

## Details

The expression `expr` is evaluated in the caller's environment and then
stored in `.fundsr_storage`. The `fundsr.reload` option can be used to
force recomputation.
