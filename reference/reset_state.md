# Clear fundsr session state

Convenience helper that clears mutable internal fundsr state for a
session: storage, fund-index map, import-function registry, Inkscape
export queue, and the XLM bookkeeping vector.

## Usage

``` r
reset_state(session = NULL)
```

## Arguments

- session:

  Optional `fundsr_session` object. Defaults to the package default
  session when `NULL`.

## Value

Invisibly returns `NULL`. Called for side effects.

## See also

Other config functions:
[`fundsr_default_session()`](https://stantraykov.github.io/fundsr/reference/fundsr_default_session.md),
[`fundsr_options()`](https://stantraykov.github.io/fundsr/reference/fundsr_options.md),
[`fundsr_session()`](https://stantraykov.github.io/fundsr/reference/fundsr_session.md)

## Examples

``` r
reset_state()
```
