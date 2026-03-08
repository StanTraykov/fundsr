# Create a fundsr session

Constructs a `fundsr_session` object, which encapsulates a mutable state
environment and a storage environment.

## Usage

``` r
fundsr_session(
  state = new.env(parent = emptyenv()),
  storage = new.env(parent = emptyenv())
)
```

## Arguments

- state:

  Environment for mutable fundsr state (fund-index map, loader registry,
  export queues, etc.).

- storage:

  Environment for cached series storage.

## Value

An object of class `"fundsr_session"`.

## See also

Other config functions:
[`fundsr_default_session()`](https://stantraykov.github.io/fundsr/reference/fundsr_default_session.md),
[`fundsr_options()`](https://stantraykov.github.io/fundsr/reference/fundsr_options.md),
[`reset_state()`](https://stantraykov.github.io/fundsr/reference/reset_state.md)
