# Generate and export rolling-differences from a specification

Iterates over plot specifications and produces rolling-difference plots
for both CAGR and log-return variants. Each plot is saved via
[`save_plot()`](https://stantraykov.github.io/fundsr/reference/save_plot.md),
and optional extra plots based on `extra_data` may also be generated.
All generated plot objects are stored in an environment and returned.

## Usage

``` r
run_plots(
  roll_diffs,
  n_days,
  plot_spec,
  extra_data = NULL,
  add_gg_params = ggplot2::geom_blank(),
  bmark_type = c("net", "gross"),
  suffix = "",
  extra_prefix = "extra_",
  extra_fun = NULL,
  session = NULL,
  ...
)
```

## Arguments

- roll_diffs:

  A list of length 2 containing data frames, named `cagr` and `log`
  (CAGR differences and log-return differences).

- n_days:

  Rolling-window length in days (passed to
  [`plot_roll_diffs()`](https://stantraykov.github.io/fundsr/reference/plot_roll_diffs.md)
  for labelling).

- plot_spec:

  A data frame or a list of data frames describing plot parameters.
  Expected columns include: `plot_id`, `title`, `data_filter`,
  `gg_params`, `width`, `height`, `funds`.

- extra_data:

  Optional data frame used to produce extra plots. Defaults to `NULL`.

- add_gg_params:

  Optional ggplot component (or list of components) appended to each
  generated plot in addition to the per-plot `gg_params` defined in
  `plot_spec`. Defaults to
  [`ggplot2::geom_blank()`](https://ggplot2.tidyverse.org/reference/geom_blank.html).

- bmark_type:

  Benchmark type used in plot titles: `"net"` or `"gross"`.

- suffix:

  Character string appended to each `plot_id` when constructing
  filenames and names in the returned environment. Defaults to `""`.

- extra_prefix:

  Character string prepended to each `plot_id` when constructing
  filenames and names in the returned environment for extra plots.
  Defaults to `"extra_"`.

- extra_fun:

  Function to call for extra plots.

- session:

  Optional `fundsr_session` object. Defaults to the package default
  session when `NULL`.

- ...:

  Additional arguments passed to
  [`plot_roll_diffs()`](https://stantraykov.github.io/fundsr/reference/plot_roll_diffs.md).

## Value

An environment containing ggplot objects. Objects are stored under names
corresponding to the base filenames used in
[`save_plot()`](https://stantraykov.github.io/fundsr/reference/save_plot.md):

- `plot_id{suffix}` for the CAGR variant,

- `plot_id{suffix}_L` for the log-return variant,

- and (when generated) `{extra_prefix}_plot_id{suffix}` for extra plots.

## Details

For each row in `plot_spec`, the function constructs both a CAGR-based
and a log-return-based variant using
[`plot_roll_diffs()`](https://stantraykov.github.io/fundsr/reference/plot_roll_diffs.md)
and writes the resulting plots via
[`save_plot()`](https://stantraykov.github.io/fundsr/reference/save_plot.md),
using a filename suffix (`_L`) to distinguish the log-return variant.
The optional `suffix` is appended to `plot_id` before filenames (and
environment keys) are formed. Additional arguments in `...` are
forwarded to
[`plot_roll_diffs()`](https://stantraykov.github.io/fundsr/reference/plot_roll_diffs.md).

If `plot_spec` is provided as a list of data frames, the function binds
them into a single specification. The `title` column may be provided as
a list column (e.g. to keep a multilingual named vector as a single
per-row value).

If `extra_data` is supplied, an extra plot is generated once per unique
set of tickers using `extra_fun`. Fund tickers are translated using
mappings from the `fundsr.ticker_map` option (tickers not present in the
map are used as-is). The first plot specification encountered for a
given ticker set determines the base filename
`{extra_prefix}_<plot_id{suffix}>` used for saving and storing the
resulting extra plot.

## See also

Other plot export utilities:
[`clear_inkscape_queue()`](https://stantraykov.github.io/fundsr/reference/clear_inkscape_queue.md),
[`export_pngs()`](https://stantraykov.github.io/fundsr/reference/export_pngs.md),
[`save_plot()`](https://stantraykov.github.io/fundsr/reference/save_plot.md)

## Examples

``` r
if (FALSE) { # \dontrun{
plots <- run_plots(roll_diffs, n_days, plot_spec, extra_data = extra_data)
plots[["funds"]]
plots[["funds_L"]]
plots[["extra_funds"]]
} # }
```
