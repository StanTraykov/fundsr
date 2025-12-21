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
        fwra = "fwia",
        acwi = "lyy0",
        iwda = "eunl",
        cw8 = "amew",
        aeem = "amem",
        lem = "lym7",
        iema = "eunm",
        emim = "is3n",
        spxs = "p500"
    )
)
spec_list <- list()
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
                  "#206666",
                  "grey50",
                  "black")

fund_colors <- function(breaks,
                        special = NULL,
                        palette = fund_palette,
                        na.value = "grey70",
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
        na.value = na.value,
        labels = toupper,
        ...
    )
}

net_idx_trans <- c(
    WORLD = "^WORLD Standard",
    ACWI = "^ACWI Standard",
    ACWI_IMI = "^ACWI IMI",
    WxUSA = "^WORLD ex USA Standard",
    EM = "^EM \\(EMERGING MARKETS\\) Standard",
    EM_IMI = "^EM \\(EMERGING MARKETS\\) IMI",
    USA = "^USA Standard",
    EUR = "^EUROPE Standard",
    PACxJPN = "^PACIFIC ex JAPAN Standard",
    JAPAN = "^JAPAN Standard",
    JP_IMI = "^JAPAN IMI",
    WxUSPAR = "^WORLD EX USA CLIMATE PARIS ALIGNED Standard",
    CHINA = "^CHINA Standard",
    TAIWAN = "^TAIWAN Standard",
    INDIA = "^INDIA Standard",
    KOREA = "^KOREA Standard",
    BRAZIL = "^BRAZIL Standard",
    S_AFR = "^SOUTH AFRICA Standard"
)
gross_idx_trans <- set_names(net_idx_trans, paste0(names(net_idx_trans), "-GR"))
net_idx_trans_ccy <- function(ccy) {
    set_names(net_idx_trans, paste0(names(net_idx_trans), ccy))
}
gross_idx_trans_ccy <- function(ccy) {
    set_names(net_idx_trans, paste0(names(net_idx_trans_ccy(ccy)), "-GR"))
}
