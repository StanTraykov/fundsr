fundsr_options(
    data_dir = file.path("data", "funds"), # where to find fund/index data
    out_dir = "output", # where to write plots
    px_width = 1300, # for optional PNG output
    # internal_png = TRUE, # whether to export PNGs via ggplot2 (lower quality)
    export_svg = TRUE, # whether to export SVGs (also needed for Inkscape PNG export)
    # inkscape = "path/to/inkscape" # set only if auto-detection fails
)
spec_list <- list()
std_w <- 14
std_h <- 9
no_filter <- NULL
zoom_filter <- function(x) {
    x |> filter(date >= lubridate::as_date("2022-01-01"))
}
fund_palette <- c("#1156cB",
                  "#ED0000",
                  "#008800",
                  "#46B8DA",
                  "#DD7700",
                  "#991188",
                  "#22cc44",
                  "grey50",
                  "black")

fund_colors <- function(breaks,
                        special = NULL,
                        palette = fund_palette,
                        na_value = "grey70",
                        ...) {
    if (missing(breaks) || is.null(breaks)) {
        stop("`breaks` must be supplied.", call. = FALSE)
    }
    breaks <- as.character(breaks)
    if (is.null(special)) special <- set_names(character(), character())
    if (!is.character(special) || is.null(names(special)) || any(!nzchar(names(special)))) {
        stop("`special` must be NULL or a named character vector, e.g. c(foo = \"black\").",
             call. = FALSE)
    }
    present <- intersect(names(special), breaks)
    others  <- breaks[!breaks %in% present]

    # assign palette ONLY to non-special levels (no palette slots wasted)
    vals_others <- set_names(rep(palette, length.out = length(others)), others)
    vals <- c(vals_others, special[present])

    ggplot2::scale_color_manual(
        values   = vals,
        na.value = na_value,
        labels = toupper,
        ...
    )
}
