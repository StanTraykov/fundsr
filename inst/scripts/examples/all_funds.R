library(tidyverse)
library(fundsr)

# Source specs
script_dir <- system.file("scripts/examples", package = "fundsr")
stopifnot(nzchar(script_dir))
spec_src <- function(...) {
    source(file.path(script_dir, ...))
}
preload_file <- "common_spec.R"
spec_files <- list.files(script_dir, pattern = "_spec\\.R$", full.names = FALSE)
spec_files <- setdiff(spec_files, preload_file )
all_specs <- sub("_spec\\.R$", "", spec_files)
if (!exists("only", inherits = FALSE)) {
    only <- Sys.getenv("FUNDSR_ONLY", "")
}
only_split <- if (!length(only) || all(is.na(only)) || !any(nzchar(only))) {
    character(0)
} else {
    trimws(unlist(strsplit(only, ",", fixed = TRUE), use.names = FALSE))
}
only_split <- only_split[!is.na(only_split) & nzchar(only_split)]
bad <- setdiff(only_split, all_specs)
if (length(bad)) warning("Unknown specs ignored: ", paste(bad, collapse = ", "), call. = FALSE)
source_specs <- if (!length(only_split)) all_specs else intersect(all_specs, only_split)
spec_src(preload_file)
purrr::walk(paste0(sort(source_specs), "_spec.R"), spec_src)

# Folder with monthly XLM Excel files
xlm_dir <- file.path("data", "xlm")

# Download missing files
download_fund_data(redownload = FALSE)

# Get funds and indexes into a big tibble, handle two FTSE All-World data sources
series <- build_all_series(late = "ftaw", join_precedence = c(".late", ".early")) |>
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
