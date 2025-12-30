# Save a plot as SVG and optionally queue PNG export via Inkscape

When `save_svg = TRUE`, saves `plot` as an SVG file and appends an
Inkscape export command to the internal queue (`.fundsr$inkscape_queue`)
so PNG generation can be performed later in batch (via
[`export_pngs()`](https://stantraykov.github.io/fundsr/reference/export_pngs.md)).

## Usage

``` r
save_plot(
  file,
  plot,
  px_width = getOption("fundsr.px_width", 1300),
  height = 12,
  width = 12,
  units = "in",
  out_dir = getOption("fundsr.out_dir", "output"),
  save_png = getOption("fundsr.internal_png", FALSE),
  save_svg = getOption("fundsr.export_svg", TRUE)
)
```

## Arguments

- file:

  Base filename (without extension) used for output files.

- plot:

  A plot object (typically a ggplot) to be saved.

- px_width:

  Width in pixels for the queued Inkscape PNG export, and (if
  `save_png = TRUE`) the target PNG pixel width used to compute the DPI.
  Defaults to `getOption("fundsr.px_width", 1300)`.

- height:

  Height of the saved plot in `units`. Defaults to `12`.

- width:

  Width of the saved plot in `units`. Defaults to `12`.

- units:

  Units for `width`/`height` (e.g. `"in"`). Defaults to `"in"`. Note:
  for immediate PNG saving, only `"in"`, `"cm"`, and `"mm"` are
  supported (to compute DPI from `px_width`).

- out_dir:

  Output directory where files are written. Defaults to
  `getOption("fundsr.out_dir", "output")`.

- save_png:

  Logical scalar; if `TRUE`, also saves a PNG immediately. Defaults to
  `getOption("fundsr.internal_png", FALSE)`.

- save_svg:

  Logical scalar; if `TRUE`, saves the SVG and queues an Inkscape export
  command. Defaults to `getOption("fundsr.export_svg", TRUE)`.

## Value

Invisibly returns `NULL`. Called for side effects.

## Details

Optionally, when `save_png = TRUE`, the function also saves a PNG
immediately via
[`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html)
(independently of queueing).

If `save_svg = TRUE`, the SVG is written as `"{file}.svg"` and an
Inkscape action string is stored as `.fundsr$inkscape_queue[file]` for
later batch export to `"{file}.png"` at `px_width` pixels wide.

If `save_png = TRUE`, a PNG is also written immediately as
`"{file}.png"`. The PNG uses the same `width`, `height`, and `units` as
the SVG, and sets `dpi = px_width / width_in` so that the pixel width is
approximately `px_width` while keeping the same physical-size
typography. The PNG background is set to white.

If both `save_svg` and `save_png` are `FALSE`, the function issues a
warning and returns without writing files or queueing exports.
