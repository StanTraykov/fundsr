# Plot Xetra Liquidity Measure (XLM) time series

Generates a time-series plot of XLM values for one or more ETFs, with
optional back-translation of the displayed ticker labels via the
`fundsr.xetra_map` option.

## Usage

``` r
plot_xlms(xlm_data, tickers, rgx = FALSE, gg_params = NULL, back_trans = FALSE)
```

## Arguments

- xlm_data:

  A data frame containing XLM observations, including columns `date`,
  `xlm`, `ticker`, and `name`.

- tickers:

  Character vector of tickers to plot, or a pattern if `rgx = TRUE`.

- rgx:

  Logical; if `TRUE`, `tickers` is treated as a regular expression and
  matched against the `name` column; otherwise it is matched against the
  `ticker` column.

- gg_params:

  Optional list of additional ggplot components (such as themes, scales,
  or labels) to be added to the base plot.

- back_trans:

  Logical; if `TRUE`, replaces values in the `ticker` column *after
  filtering* using the inverse of the mapping stored in the
  `fundsr.xetra_map` option. This affects only what is displayed (legend
  labels and color grouping), not which rows are selected.

## Value

A ggplot object showing the XLM time-series curves.

## Details

If `rgx = FALSE`, the data are filtered by matching `tickers` against
the `ticker` column. If `rgx = TRUE`, `tickers` is treated as a regular
expression and matched against the `name` column.

When `back_trans = TRUE`, the function reads
`getOption("fundsr.xetra_map")`, expected to be a named character vector
mapping internal identifiers to Xetra tickers, for example:
`c(fwra = "fwia", acwi = "lyy0")`. The mapping is inverted and applied
to the already-filtered data so that occurrences of Xetra tickers in the
plotted data are replaced by their internal identifiers in the plot's
legend and grouping.

## Examples

``` r
if (FALSE) { # \dontrun{
options(fundsr.xetra_map = c(fwra = "fwia", acwi = "lyy0", iwda = "eunl", cw8 = "amew"))

# Filter by tickers as they appear in xlm_data$ticker, but display back-translated labels
plot_xlms(xlm_data, tickers = c("fwia", "eunl"), back_trans = TRUE)

# Regex match against the `name` column
# (back-translation, if enabled, still happens after filtering)
plot_xlms(xlm_data, tickers = "MSCI World", rgx = TRUE, back_trans = TRUE)
} # }
```
