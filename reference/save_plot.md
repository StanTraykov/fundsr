# Save a plot as SVG and optionally save PNG immediately

Saves `plot` as an SVG file when `save_svg = TRUE`. When an SVG is
saved, an Inkscape export action is also queued so PNG generation can be
performed later in batch via
[`export_pngs()`](https://stantraykov.github.io/fundsr/reference/export_pngs.md).
Optionally, when `save_png = TRUE`, the function also saves a PNG
immediately via
[`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html)
(independently of queueing).

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
  save_svg = getOption("fundsr.export_svg", TRUE),
  background = "white"
)
```

## Arguments

- file:

  Base filename (without extension) used for output files.

- plot:

  A plot object (typically a ggplot) to be saved.

- px_width:

  Target width in pixels for PNG output. Used as the queued Inkscape
  `export-width`, and (if `save_png = TRUE`) used to compute the DPI for
  immediate PNG saving. Defaults to
  `getOption("fundsr.px_width", 1300)`.

- height:

  Height of the saved plot in `units`. Defaults to `12`.

- width:

  Width of the saved plot in `units`. Defaults to `12`.

- units:

  Units for `width`/`height` (e.g. `"in"`). Defaults to `"in"`. For
  immediate PNG saving, only `"in"`, `"cm"`, and `"mm"` are supported
  (to compute DPI from `px_width`).

- out_dir:

  Output directory where files are written. Defaults to
  `getOption("fundsr.out_dir", "output")`.

- save_png:

  Logical scalar; if `TRUE`, also saves a PNG immediately. Defaults to
  `getOption("fundsr.internal_png", FALSE)`.

- save_svg:

  Logical scalar; if `TRUE`, saves the SVG and queues an Inkscape export
  action. Defaults to `getOption("fundsr.export_svg", TRUE)`.

- background:

  Background color used for immediate PNG saving via
  [`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html)
  (`bg`). Defaults to `"white"`.

## Value

Invisibly returns `NULL`. Called for side effects.

## Details

If `save_svg = TRUE`, the SVG is written as `"{file}.svg"`. An Inkscape
action string is then stored in `.fundsr$inkscape_queue[file]` so the
SVG can later be exported to `"{file}.png"` at `px_width` pixels wide
when
[`export_pngs()`](https://stantraykov.github.io/fundsr/reference/export_pngs.md)
is run.

Queueing is refused if either output path contains a semicolon (`;`),
since Inkscape actions are separated by semicolons.

If `save_png = TRUE`, a PNG is also written immediately as
`"{file}.png"`. The PNG uses the same `width`, `height`, and `units` as
the SVG, and sets `dpi = px_width / width_in` so that the pixel width is
approximately `px_width` while keeping comparable physical-size
typography across outputs. The PNG background is set via `background`.

If both `save_svg` and `save_png` are `FALSE`, the function issues a
warning and returns without writing files or queueing exports.
