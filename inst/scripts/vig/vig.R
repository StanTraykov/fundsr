library(tidyverse)
library(fundsr)

# Set up directories
dirs <- c(
    data = file.path("data", "funds"),
    xlm = file.path("data", "xlm"),
    out = "output"
)
for (d in dirs) {
    dir.create(d, recursive = TRUE)
}

# Populate the XLM directory with some monthly XLM reports
xlm_urls <- c(
    "https://www.cashmarket.deutsche-boerse.com/resource/blob/4844258/91ce589f5309cbbd1ad0c92b3e6cdbda/data/20251130-ETF-ETP-Statistic.xlsx",
    "https://www.cashmarket.deutsche-boerse.com/resource/blob/4795286/394c4451af562507f9def3f39da62242/data/20251031-ETF-ETP-Statistic.xlsx",
    "https://www.cashmarket.deutsche-boerse.com/resource/blob/4725636/2d62a1677b537d996aefc45df3ff21d3/data/20250930-ETF-ETP-Statistic.xlsx",
    "https://www.cashmarket.deutsche-boerse.com/resource/blob/4674370/0de847380722e8e87bc821cb5313ba41/data/20250831-ETF-ETP-Statistic.xlsx"
)
for (url in xlm_urls) {
    fname <- basename(url)
    dest_path <- file.path(dirs[["xlm"]], fname)
    if (!file.exists(dest_path)) {
        download.file(url, destfile = dest_path, mode = "wb", quiet = TRUE)
        Sys.sleep(stats::runif(1, 0.5, 1.0))
    }
}

# Import XLM data
if (!exists("xlm_data")) {
    xlm_data <- read_xlm_directory(dirs[["xlm"]])
}

# Set package options
options(fundsr.data_dir = dirs[["data"]])
options(fundsr.out_dir = dirs[["out"]])
# Helper to set fundsr.dl_list option
add_to_dl_list(c(
    IUSQ = "https://www.ishares.com/uk/individual/en/products/251850/ishares-msci-acwi-ucits-etf/1535604580409.ajax?fileType=xls&fileName=iShares-MSCI-ACWI-UCITS-ETF-USD-Acc_fund&dataType=fund",
    SPYY = "https://www.ssga.com/ie/en_gb/institutional/library-content/products/fund-data/etfs/emea/navhist-emea-en-spyy-gy.xlsx"
))

# Populate funds directory (download files in fundsr.dl_list option)
dl_funds()

# Specify import function
import_fun <- function() {
    spdr("SPYY", benchmark = "ACWI") # filename defaults to <ticker>.xlsx (.xls for iShares)
    ishs("IUSQ", benchmark = "ACWI", retrieve_benchmark = T) # also retrieve ACWI from file
}

# Get fund data into tibbles in a storage environment
storage <- import_funds(import_fun = import_fun)
fund_index_map <- get_fund_index() # fund-index map resulting from above import

# Join the environment into a big tibble, cut off & sort
series <- join_env(storage, by = "date") %>%
    filter(date >= as_date("2012-12-29")) %>%
    arrange(date)

# Calculate CAGR & log diffs
nd <- 365
diffs <- map(
    list(cagr = FALSE, log = TRUE),
    ~ roll_diffs(series, nd, fund_index_map, use_log = .x)
)

# Plot spec
no_filter <- NULL
zoom_filter <- function(x) {x %>% filter(date >= as_date("2022-01-01"))}
acwi_funds <- c("spyy", "iusq") # use lowercase here
fund_pal <- c("IUSQ" = "red", "SPYY" = "blue")

plot_spec <- tribble(
    ~plot_id, ~title, ~filter,
    ~gg_params, ~width,  ~height,
    ~funds,

    "ACWI", "SPYY & IUSQ", no_filter,
    scale_color_manual(values = fund_pal), 14, 9,
    acwi_funds,

    "ACWIz",
    c(en = "SPYY & IUSQ: recent years",
      bg = "SPYY & IUSQ: последни години"),
    zoom_filter,
    scale_color_manual(values = fund_pal), 14, 9,
    acwi_funds
)

# Run the plots (outputs svg and queues for optional PNG export)
Sys.setLanguage("en")
run_plots(diffs$cagr, diffs$log, nd, plot_spec, xlm_data)

# Plots in another language
Sys.setLanguage("bg")
plot_spec <- plot_spec %>%
    mutate(plot_id = paste0(plot_id, "_bg"))
run_plots(diffs$cagr, diffs$log, nd, plot_spec, xlm_data)

# Optional: high-quality PNG export
options(fundsr.inkscape = "C:/Program Files/Inkscape/bin/inkscape.exe/")
# options(fundsr.inkscape = "/usr/bin/inkscape")
ggexport()
