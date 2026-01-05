# Export queued SVGs to PNG via Inkscape

Runs all pending Inkscape export actions stored in the internal queue,
invoking Inkscape to produce PNG files from previously saved SVGs.

## Usage

``` r
export_pngs(background = "white")
```

## Arguments

- background:

  Background color for PNG export, passed to Inkscape as an
  `export-background:{...}` action. If `NULL`, no background action is
  added. Defaults to `"white"`.

## Value

The exit status returned by Inkscape (0 indicates success). Invisibly
returns `NULL` if the queue is empty or if Inkscape cannot be located.

## Details

This function reads queued Inkscape actions from
`.fundsr$inkscape_queue`, optionally prepends an
`export-background:{background}` action, and executes Inkscape using
[`base::system2()`](https://rdrr.io/r/base/system2.html).

The Inkscape executable is searched for in the following order:

1.  user-supplied config (`fundsr.inkscape` option or `INKSCAPE`
    environment variable)

2.  an `inkscape` executable available on the system `PATH`

3.  common installation locations (Windows, macOS, Linux, etc.)

On success, the internal queue is cleared. On failure, the queue is left
intact and a message is printed.

## See also

[`clear_inkscape_queue()`](https://stantraykov.github.io/fundsr/reference/clear_inkscape_queue.md),
[`save_plot()`](https://stantraykov.github.io/fundsr/reference/save_plot.md)
