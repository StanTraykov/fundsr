library(tidyverse)

# Config
source("dev/examples/common_spec.R")
# Specs
source("dev/examples/glob_spec.R")
source("dev/examples/dm_spec.R")
source("dev/examples/em_spec.R")
source("dev/examples/usa_sel_spec.R")
source("dev/examples/exus_spec.R")

xlm_dir <- file.path("data", "xlm")

# Download missing files
download_fund_data(redownload = FALSE)

# Get funds and indexes into a big tibble, handle two FTSE All-World data sources
series <- load_all_series(late = "ftaw", coalesce_suffixed = c(".y", ".x")) %>%
    filter(date >= as_date("2012-12-29"))

# Calculate CAGR & log diffs vs both net & gross variants
nd <- 365
diffs <- map(
    list(net = "net", gross = "gross"),
    ~ roll_diffs(series, nd, get_fund_index_map(), index_level = .x, messages = NULL)
)

# Get XLM data
if (dir.exists(xlm_dir)) {
    if (!exists("xlm_data")) xlm_data <- read_xlm_directory(xlm_dir)
} else {
    xlm_data <- NULL
}

# Plots
net_plots <- run_plots(diffs$net, nd, spec_list,
                       xlm_data = xlm_data)
gr_plots <- run_plots(diffs$gross, nd, spec_list,
                      bmark_type = "gross",
                      suffix = "_GR")

# Optional high-quality PNG export
# export_pngs()
