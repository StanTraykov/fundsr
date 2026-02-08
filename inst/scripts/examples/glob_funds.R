library(tidyverse)
library(fundsr)

script_dir <- system.file("scripts/examples", package = "fundsr")
stopifnot(nzchar(script_dir))
source_script <- function(...) {
    source(file.path(script_dir, ...))
}
# Config
source_script("common_config.R")
source_script("glob_spec.R")
xlm_dir <- file.path("data", "xlm")

# Download missing files
download_fund_data(redownload = FALSE)

# Get funds and indexes into a big tibble, handle two FTSE All-World data sources
series <- build_all_series(late = "ftaw", join_precedence = c(".late", ".early")) |>
    filter(date >= as_date("2012-12-29"))

# Calculate CAGR & log diffs and runs plots
nd <- 365
diffs <- roll_diffs(series, nd, get_fund_index_map(), index_level = "net", messages = NULL)

# Get XLM data
if (dir.exists(xlm_dir)) {
    if (!exists("xlm_data")) xlm_data <- read_xlm_directory(xlm_dir)
} else {
    xlm_data <- NULL
}

# Plots
plots <- run_plots(diffs, nd, spec_list, xlm_data)

# Optional high-quality PNG export
# export_pngs()
