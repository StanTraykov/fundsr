# Clear queued Inkscape exports

Clears the internal Inkscape export queue (`.fundsr$inkscape_queue`),
removing all queued export commands.

## Usage

``` r
clear_inkscape_queue()
```

## Value

Invisibly returns `NULL`. Called for side effects.

## Examples

``` r
clear_inkscape_queue()
```
