# Clear fundsr session state

Convenience helper that clears mutable internal fundsr state: the fund
storage (`.fundsr_storage`) and fund-index map
(`.fundsr$fund_index_map`), the import-function registry
(`.fundsr$data_loaders`), the Inkscape export queue
(`.fundsr$inkscape_queue`), and the XLM bookkeeping vector
(`.fundsr$done_xlm_sets`).

## Usage

``` r
reset_state()
```

## Value

Invisibly returns `NULL`. Called for side effects.

## Examples

``` r
reset_state()
```
