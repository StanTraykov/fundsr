library(tidyverse)
library(fundsr)

# Reset state
fun_id <- "examples_all_funds"
if (!exists("funiverse") || !identical(funiverse, fun_id))
    fundsr::reset_state()
funiverse <- fun_id
spec_list <- list()

# Config
source("common_spec.R")
source("glob_spec.R")
source("dm_spec.R")
source("em_spec.R")
source("usa_sel_spec.R")
source("exus_spec.R")
xlm_dir <- file.path("data", "xlm")

# Download missing files
download_fund_data(redownload = FALSE)

# Get fund data into tibbles stored in the storage env
storage <- run_data_loaders()

# Join the environment into a big tibble, handle two FTSE All-World data sources
series <- join_env(storage, by = "date", late = "ftaw", coalesce_suffixed = c(".y", ".x")) %>%
    filter(date >= as_date("2012-12-29")) %>%
    arrange(date)

# Calculate CAGR & log diffs and runs plots
nd <- 365
diffs <- map(
    list(cagr = FALSE, log = TRUE),
    ~ roll_diffs(series, nd, get_fund_index_map(), use_log = .x, silent_skip = TRUE)
)

# Get XLM data
if (dir.exists(xlm_dir)) {
    if (!exists("xlm_data")) xlm_data <- read_xlm_directory(xlm_dir)
} else {
    xlm_data <- NULL
}

# Plots
plots <- run_plots(diffs$cagr, diffs$log, nd, spec_list, xlm_data)

# Optional high-quality PNG export
# export_pngs()
