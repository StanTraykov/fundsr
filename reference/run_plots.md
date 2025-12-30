# Generate and export all configured rolling-difference plots

Iterates over plot specifications and produces rolling-difference plots
for both CAGR and log-return variants. Each plot is saved via
[`save_plot()`](https://stantraykov.github.io/fundsr/reference/save_plot.md),
and optional XLM plots may also be generated. All generated plot objects
are stored in an environment and returned.

## Usage

``` r
run_plots(
  rds_cagr,
  rds_log,
  n_days,
  plot_spec,
  xlm_data = NULL,
  add_gg_params = ggplot2::geom_blank(),
  bmark_type = c("net", "gross")
)
```

## Arguments

- rds_cagr:

  Data frame containing rolling CAGR differences.

- rds_log:

  Data frame containing rolling log-return differences.

- n_days:

  Rolling-window length in days (passed to
  [`plot_roll_diffs()`](https://stantraykov.github.io/fundsr/reference/plot_roll_diffs.md)
  for labelling).

- plot_spec:

  A data frame or a list of data frames describing plot parameters.
  Expected columns include: `plot_id`, `title`, `data_filter`,
  `gg_params`, `width`, `height`, `funds`.

- xlm_data:

  Optional data frame used to produce XLM plots, including `date`,
  `xlm`, `ticker`, and `name` columns.

- add_gg_params:

  Optional ggplot component (or list of components) appended to each
  generated plot in addition to the per-plot `gg_params` defined in
  `plot_spec`. Defaults to
  [`ggplot2::geom_blank()`](https://ggplot2.tidyverse.org/reference/geom_blank.html).

- bmark_type:

  Benchmark type used in plot titles: `"net"` or `"gross"`.

## Value

An environment containing ggplot objects. Objects are stored under names
corresponding to the base filenames used in
[`save_plot()`](https://stantraykov.github.io/fundsr/reference/save_plot.md):
`plot_id` for CAGR variants, `plot_id_L` for log-return variants, and
(when generated) `xlm_<plot_id>` for XLM plots.

## Details

For each row in `plot_spec`, the function constructs both a CAGR-based
and a log-return-based variant using
[`plot_roll_diffs()`](https://stantraykov.github.io/fundsr/reference/plot_roll_diffs.md)
and writes the resulting plots via
[`save_plot()`](https://stantraykov.github.io/fundsr/reference/save_plot.md),
using a filename suffix (`_L`) to distinguish the log-return variant.

If `plot_spec` is provided as a list of data frames, the function binds
them into a single specification. The `title` column may be provided as
a list column (e.g. to keep a multilingual named vector as a single
per-row value).

If `xlm_data` is supplied, an XLM plot is generated once per unique set
of Xetra tickers using
[`plot_xlms()`](https://stantraykov.github.io/fundsr/reference/plot_xlms.md).
Fund tickers are mapped to Xetra tickers via the `fundsr.xetra_map`
option (tickers not present in the map are used as-is). The first plot
specification encountered for a given ticker set determines the base
filename `xlm_<plot_id>` used for saving and storing the resulting XLM
plot.

## Examples

``` r
if (FALSE) { # \dontrun{
plots <- run_plots(rds_cagr, rds_log, n_days, plot_spec, xlm_data = xlm_data)
plots[["global"]]
plots[["global_L"]]
plots[["xlm_global"]]
} # }
```
