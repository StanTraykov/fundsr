# Compare net indices vs gross indices
library(tidyverse)
library(fundsr)

script_dir <- system.file("scripts/examples", package = "fundsr")
stopifnot(nzchar(script_dir))
spec_src <- function(...) {
    source(file.path(script_dir, ...))
}
# Config
spec_src("common_spec.R")
spec_src("em_topc_spec.R")

# Get funds and indexes into a big tibble
series <- build_all_series() |>
    filter(date >= as_date("2012-12-29"))

# Calculate CAGR & log diffs and runs plots
nd <- 365
diffs <- roll_diffs(series, nd, get_fund_index_map(), messages = "roll")

xlm_data <- NULL

# Plots
plots <- run_plots(diffs, nd, spec_list, xlm_data, bmark_type = "gross")

# Optional high-quality PNG export
# export_pngs()
