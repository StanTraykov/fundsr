# Compare net indices vs gross indices
library(tidyverse)

# Config
fundsr::reset_state()
source("dev/examples/common_spec.R")
source("dev/examples/em_topc_spec.R")

# Get fund data into tibbles stored in the storage env
storage <- run_data_loaders()

# Join the environment into a big tibble, handle two FTSE All-World data sources
series <- join_env(storage, by = "date") %>%
    filter(date >= as_date("2012-12-29")) %>%
    arrange(date)

# Calculate CAGR & log diffs and runs plots
nd <- 365
diffs <- map(
    list(cagr = FALSE, log = TRUE),
    ~ roll_diffs(series, nd, get_fund_index_map(), use_log = .x, silent_skip = TRUE)
)

xlm_data <- NULL

# Plots
plots <- run_plots(diffs$cagr, diffs$log, nd, spec_list, xlm_data, bmark_type = "gross")

# Optional high-quality PNG export
# export_pngs()
