# Export queued SVG files to PNG using Inkscape

Processes all pending items in the internal Inkscape export queue and
invokes Inkscape to generate PNG files from previously saved SVGs.

## Usage

``` r
export_pngs()
```

## Value

The exit status returned by the system call to Inkscape. Invisibly
returns `NULL` if no items are queued.

## Details

This function collects all queued Inkscape actions stored in
`.fundsr$inkscape_queue`, constructs a single Inkscape command using the
executable specified by the `fundsr.inkscape` option, and executes it
via [`system()`](https://rdrr.io/r/base/system.html). On success, the
queue is cleared. On failure, the queue is preserved and a message is
printed.
