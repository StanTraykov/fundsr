# Clear queued Inkscape exports

Clears the internal Inkscape export queue (`.fundsr$inkscape_queue`),
removing all queued export commands.

## Usage

``` r
clear_inkscape_queue()
```

## Value

Invisibly returns `NULL`. Called for side effects.

## See also

Other plot export utilities:
[`export_pngs()`](https://stantraykov.github.io/fundsr/reference/export_pngs.md),
[`run_plots()`](https://stantraykov.github.io/fundsr/reference/run_plots.md),
[`save_plot()`](https://stantraykov.github.io/fundsr/reference/save_plot.md)

## Examples

``` r
clear_inkscape_queue()
```
