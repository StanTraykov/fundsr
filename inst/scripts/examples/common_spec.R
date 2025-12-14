fundsr_options(
    data_dir = file.path("data", "funds"), # where to find fund/index data
    out_dir = "output", # where to write plots
    px_width = 1300, # for optional PNG output
    # internal_png = TRUE, # whether to export PNGs via ggplot2 (lower quality)
    export_svg = TRUE, # whether to export SVGs (needed for Inkscape PNG export)

    # Inkscape executable for higher-quality PNG export
    # (uncomment depending on system or comment all to disable)
    inkscape = "C:/Program Files/Inkscape/bin/inkscape.exe",
    # inkscape = "/Applications/Inkscape.app/Contents/MacOS/Inkscape",
    # inkscape = "/usr/bin/inkscape",
    # inkscape = Sys.which("inkscape"), # if it's on PATH

    xetra_map = c(
        FWRA = "FWIA"
    )
)
std_w <- 14
std_h <- 9
no_filter <- NULL
zoom_filter <- function(x) {x %>% filter(date >= lubridate::as_date("2022-01-01"))}
fund_palette <- c("#11569B",
                  "#ED0000",
                  "#009900",
                  "#46B8DA",
                  "#DD7700",
                  "#880088",
                  "black",
                  "grey50",
                  "#206666")
fund_colors <- function(...) {
    scale_color_manual(values = fund_palette, ...)
}
spec_list <- list()
