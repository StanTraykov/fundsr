library(tidyverse)
library(fundsr)

source("common_spec.R")
source("glob_spec.R")
source("dm_spec.R")

xlm_dir <- file.path("data", "xlm")

# Get fund data into tibbles stored in the storage env
storage <- import_funds()
fund_index <- get_fund_index() # fund-index map resulting from above import

# Join the environment into a big tibble, handle two FTSE All-World data sources
series <- join_env(storage, by = "date", late = "ftaw", coalesce_suffixed = c(".y", ".x")) %>%
    filter(date >= as_date("2012-12-29")) %>%
    arrange(date)

# Calculate CAGR & log diffs and runs plots
nd <- 365
diffs <- map(
    list(cagr = FALSE, log = TRUE),
    ~ roll_diffs(series, nd, fund_index, use_log = .x)
)

# Get XLM data
if (dir.exists(xlm_dir)) {
    if (!exists("xlm_data")) xlm_data <- read_xlm_directory(xlm_dir)
} else {
    xlm_data <- NULL
}

# Plots
ensure_title_col <- function(df, col = "title") {
    if (!col %in% names(df)) return(df)
    x <- df[[col]]
    if (is.list(x)) return(df)
    if (is.character(x)) {
        is_multilang <- !is.null(names(x)) && any(nzchar(names(x)))
        # If it's a multilingual named vector, keep it as one element (common in 1-row specs)
        if (is_multilang && nrow(df) == 1L) {
            df[[col]] <- list(x)
            return(df)
        }
        # Otherwise treat as per-row scalar titles
        df[[col]] <- as.list(x)
        return(df)
    }
    stop(sprintf("`%s` must be character or list.", col), call. = FALSE)
}
spec_list_l <- lapply(spec_list, ensure_title_col)
master_spec <- dplyr::bind_rows(spec_list_l)
run_plots(diffs$cagr, diffs$log, nd, master_spec, xlm_data)

# Optional high-quality PNG export
# ggexport()
