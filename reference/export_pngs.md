# Export queued SVG files to PNG using Inkscape

Processes all pending items in the internal Inkscape export queue and
invokes Inkscape to generate PNG files from previously saved SVGs.

## Usage

``` r
export_pngs(background = "white")
```

## Arguments

- background:

  Background color to use for PNG export (passed to Inkscape as
  `export-background`). If `NULL`, no `export-background` action is
  added. Defaults to `"white"`.

## Value

The exit status returned by the call to Inkscape (0 indicates success).
Invisibly returns `NULL` if no items are queued or if no Inkscape
executable is configured.

## Details

This function reads queued Inkscape actions from
`.fundsr$inkscape_queue`, optionally prepends an
`export-background:{background}` action, and executes Inkscape using the
executable path in the `fundsr.inkscape` option via
[`base::system2()`](https://rdrr.io/r/base/system2.html).

On success, the queue is cleared via
[`clear_inkscape_queue()`](https://stantraykov.github.io/fundsr/reference/clear_inkscape_queue.md).
On failure, the queue is left intact and a message is printed.
