# Compare net indices vs gross indices
library(tidyverse)
library(fundsr)

##### Config ######
common_config <- system.file("scripts/examples/common_config.R", package = "fundsr")
stopifnot(nzchar(common_config))
source(common_config)

##### Plot specs #####
idx <- c("CHINA", "TAIWAN", "INDIA", "KOREA", "BRAZIL", "S_AFR")
title <- c(
    en = "EM net indices",
    bg = "нетни индески за разв. п-ри"
)
gg_par <- fund_colors(breaks = idx)

plot_spec <- tribble(
    ~plot_id, ~title, ~data_filter,
    ~gg_params, ~width,  ~height,
    ~funds,

    "em_topc", title, no_filter,
    gg_par, std_w, std_h,
    idx,

    "em_topcZ", title, zoom_filter,
    gg_par, std_w, std_h,
    idx
)
spec_list <- c(spec_list, list(plot_spec))

##### Data #####
add_data_loader(function() {
    msci(var_name = "msci-em-nt",
         col_trans = net_idx_trans,
         benchmarks = set_names(names(gross_idx_trans), names(net_idx_trans)),
         file = "MSCI-EM-NT.xls")
    msci(var_name = "msci-em-gr",
         col_trans = gross_idx_trans,
         file = "MSCI-EM-GR.xls")
})

##### Run #####
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
