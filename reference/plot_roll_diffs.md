# Plot rolling return differences against benchmark

Plots rolling annualized tracking differences (CAGR-style or log-return)
for selected funds against their benchmark series. The plot uses
quantile-based y-limits and formats the y-axis in basis points.

## Usage

``` r
plot_roll_diffs(
  data,
  n_days,
  funds,
  use_log = FALSE,
  gg_params = NULL,
  title_add = NULL,
  date_brk = NULL,
  qprob = c(0.005, 0.995),
  bmark_type = c("net", "gross")
)
```

## Arguments

- data:

  Input data frame containing a `date` column and one rolling-difference
  column per fund, named `<fund>_rd`.

- n_days:

  Window length (in days) used to compute rolling differences (used for
  labelling).

- funds:

  Character vector of fund tickers/column prefixes to include.

- use_log:

  Logical; if `TRUE`, labels the plot as log-return differences. If
  `FALSE`, labels the plot as CAGR differences.

- gg_params:

  Optional ggplot components to add to the plot.

- title_add:

  Optional title suffix. Can be a single string or a named character
  vector specifying the title in multiple languages (e.g.
  `c(en=..., bg=...)`).

- date_brk:

  Optional date-break specification for the x-axis (e.g. `"3 months"`).
  If `NULL`, it is chosen automatically based on the time span.

- qprob:

  Two-element numeric vector giving lower and upper quantiles used to
  set the baseline y-axis limits. Defaults to `c(0.005, 0.995)`.

- bmark_type:

  Benchmark type used in the title: `"net"` or `"gross"`.

## Value

A ggplot object.

## Details

The function reshapes `data` to long format and produces a scatter plot
coloured by fund. The y-axis limits are primarily determined from
quantiles of the rolling differences (as specified by `qprob`), always
including 0.

To avoid clipping recent extremes, the y-limits are expanded (if needed)
to also include the full range observed in the most recent 30 days of
data, even when those values fall outside the `qprob` quantiles.
