options(fundsr.xetra_map = c(
    FWRA = "FWIA"
))

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
