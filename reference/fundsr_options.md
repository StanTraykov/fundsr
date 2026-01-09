# Set fundsr package options

Convenience wrapper around
[`options()`](https://rdrr.io/r/base/options.html) for setting common
`fundsr.*` options.

## Usage

``` r
fundsr_options(
  data_dir = NULL,
  out_dir = NULL,
  px_width = NULL,
  internal_png = NULL,
  export_svg = NULL,
  xetra_map = NULL,
  inkscape = NULL,
  reload = NULL,
  fund_urls = NULL
)
```

## Arguments

- data_dir:

  Directory containing fund data files (sets `fundsr.data_dir`).

- out_dir:

  Output directory for plots/exports (sets `fundsr.out_dir`).

- px_width:

  Default PNG export width in pixels (sets `fundsr.px_width`).

- internal_png:

  Logical; whether to save an internal PNG immediately when exporting
  plots (sets `fundsr.internal_png`).

- export_svg:

  Logical; whether to save SVGs and queue Inkscape exports (sets
  `fundsr.export_svg`).

- xetra_map:

  Named character vector mapping "primary" tickers (series table column
  names) to Xetra tickers (sets `fundsr.xetra_map`).

- inkscape:

  Inkscape executable (path or command name) used by export helpers
  (sets `fundsr.inkscape`).

- reload:

  Logical; default value for forcing re-import of cached objects (sets
  `fundsr.reload`).

- fund_urls:

  Named character vector or named list of URLs for fund data downloads
  (sets `fundsr.fund_urls`).

## Value

Invisibly returns a named list of the previous values of the options
that were changed (as returned by
[`options()`](https://rdrr.io/r/base/options.html)).

## Details

All arguments default to `NULL`. If an argument is left as `NULL`, the
corresponding `fundsr.*` option is left unchanged.

## See also

[`add_fund_urls()`](https://stantraykov.github.io/fundsr/reference/add_fund_urls.md)
to add/update entries in `fundsr.fund_urls`.

Other config functions:
[`reset_state()`](https://stantraykov.github.io/fundsr/reference/reset_state.md)
