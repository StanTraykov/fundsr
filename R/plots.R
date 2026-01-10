#' Save a plot as SVG and/or PNG and queue for Inkscape conversion
#'
#' Saves `plot` as an SVG file when `save_svg = TRUE`. When an SVG is saved, an
#' Inkscape export action is also queued so PNG generation can be performed later
#' in batch via [export_pngs()]. Optionally, when `save_png = TRUE`, the function
#' also saves a PNG immediately via [ggplot2::ggsave()] (independently of queueing).
#'
#' @param file Base filename (without extension) used for output files.
#' @param plot A plot object (typically a ggplot) to be saved.
#' @param px_width Target width in pixels for PNG output. Used as the queued
#'   Inkscape `export-width`, and (if `save_png = TRUE`) used to compute the DPI
#'   for immediate PNG saving. Defaults to option `fundsr.px_width` or 1300.
#' @param height Height of the saved plot in `units`. Defaults to `12`.
#' @param width Width of the saved plot in `units`. Defaults to `12`.
#' @param units Units for `width`/`height` (e.g. `"in"`). Defaults to `"in"`.
#'   For immediate PNG saving, only `"in"`, `"cm"`, and `"mm"` are supported (to
#'   compute DPI from `px_width`).
#' @param out_dir Output directory where files are written. Defaults to
#'   option `fundsr.out_dir`.
#' @param save_png Logical scalar; if `TRUE`, also saves a PNG immediately.
#'   Defaults to option `fundsr.internal_png` or `FALSE`.
#' @param save_svg Logical scalar; if `TRUE`, saves the SVG and queues an
#'   Inkscape export action. Defaults to option `fundsr.export_svg` or `TRUE`.
#' @param background Background color used for immediate PNG saving via
#'   [ggplot2::ggsave()] (`bg`). Defaults to `"white"`.
#'
#' @return Invisibly returns `NULL`. Called for side effects.
#'
#' @details
#' If `save_svg = TRUE`, the SVG is written as `"{file}.svg"`. An Inkscape action
#' string is then stored in `.fundsr$inkscape_queue[file]` so the SVG can later be
#' exported to `"{file}.png"` at `px_width` pixels wide when [export_pngs()] is run.
#'
#' Queueing is refused if either output path contains a semicolon (`;`), since
#' Inkscape actions are separated by semicolons.
#'
#' If `save_png = TRUE`, a PNG is also written immediately as `"{file}.png"`.
#' The PNG uses the same `width`, `height`, and `units` as the SVG, and sets
#' `dpi = px_width / width_in` so that the pixel width is approximately
#' `px_width` while keeping comparable physical-size typography across outputs.
#' The PNG background is set via `background`.
#'
#' If both `save_svg` and `save_png` are `FALSE`, the function issues a warning
#' and returns without writing files or queueing exports.
#'
#' @family plot export utilities
#' @export
save_plot <- function(file,
                  plot,
                  px_width = fundsr_get_option("px_width", 1300),
                  height = 12,
                  width = 12,
                  units = "in",
                  out_dir = fundsr_get_option("out_dir"),
                  save_png = fundsr_get_option("internal_png", FALSE),
                  save_svg = fundsr_get_option("export_svg", TRUE),
                  background = "white") {
    if (!is.character(file) || length(file) != 1L || !nzchar(file)) {
        stop("`file` must be a non-empty single string.", call. = FALSE)
    }
    if (!is.logical(save_png) || length(save_png) != 1L || is.na(save_png)) {
        stop("`save_png` must be TRUE or FALSE.", call. = FALSE)
    }
    if (!is.logical(save_svg) || length(save_svg) != 1L || is.na(save_svg)) {
        stop("`save_svg` must be TRUE or FALSE.", call. = FALSE)
    }
    if (!isTRUE(save_svg) && !isTRUE(save_png)) {
        warning("Nothing to do: both `save_svg` and `save_png` are FALSE.", call. = FALSE)
        return(invisible(NULL))
    }

    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
    fname <- function(fn, ext) file.path(out_dir, glue("{fn}.{ext}"))
    svgf <- fname(file, "svg")
    pngf <- fname(file, "png")

    if (isTRUE(save_svg)) {
        ggplot2::ggsave(
            svgf,
            plot = plot,
            height = height,
            width = width,
            units = units
        )
        bad <- grepl(";", c(svgf, pngf), fixed = TRUE)
        if (any(bad)) {
            bad_items <- unique(c(svgf, pngf)[bad])
            stop(
                "cannot queue Inkscape export; ';' in path(s):\n- ",
                paste(bad_items, collapse = "\n- "),
                call. = FALSE
            )
        }
        a <- glue(
            "file-open:{svgf};export-filename:{pngf};export-width:{px_width};export-do;file-close"
        )
        if (is.null(.fundsr$inkscape_queue)) clear_inkscape_queue()
        .fundsr$inkscape_queue[file] <- a
    }

    if (isTRUE(save_png)) {
        width_in <- switch(
            units,
            "in" = width,
            "cm" = width / 2.54,
            "mm" = width / 25.4,
            stop("For PNG saving, `units` must be one of: \"in\", \"cm\", \"mm\".", call. = FALSE)
        )
        dpi <- px_width / width_in
        ggplot2::ggsave(
            pngf,
            plot = plot,
            width = width,
            height = height,
            units = units,
            dpi = dpi,
            bg = background,
            limitsize = FALSE
        )
    }

    invisible(NULL)
}

#' Export queued SVGs to PNG via Inkscape
#'
#' Runs all pending Inkscape export actions stored in the internal queue,
#' invoking Inkscape to produce PNG files from previously saved SVGs.
#'
#' @param background Background color for PNG export, passed to Inkscape as an
#'   `export-background:{...}` action. If `NULL`, no background action is added.
#'   Defaults to `"white"`.
#'
#' @return
#' The exit status returned by Inkscape (0 indicates success). Invisibly returns
#' `NULL` if the queue is empty or if Inkscape cannot be located.
#'
#' @details
#' This function reads queued Inkscape actions from `.fundsr$inkscape_queue`,
#' optionally prepends an `export-background:{background}` action, and executes
#' Inkscape using [base::system2()].
#'
#' The Inkscape executable is searched for in the following order:
#' 1. user-supplied config (`fundsr.inkscape` option or `INKSCAPE` environment variable)
#' 2. an `inkscape` executable available on the system `PATH`
#' 3. common installation locations (Windows, macOS, Linux, etc.)
#'
#' On success, the internal queue is cleared. On failure, the queue is left
#' intact and a message is printed.
#'
#' @seealso [clear_inkscape_queue()], [save_plot()]
#' @family plot export utilities
#' @export
export_pngs <- function(background = "white") {
    if (length(.fundsr$inkscape_queue) == 0) {
        fundsr_msg("export_pngs: nothing queued.", level = 1L)
        return(invisible(NULL))
    }
    inkscape <- find_inkscape()
    if (is.na(inkscape) || !nzchar(inkscape)) {
        fundsr_msg("export_pngs: cannot find Inkscape", level = 0L)
        return(invisible(NULL))
    }
    acts <- paste(.fundsr$inkscape_queue, collapse = ";")
    if (!is.null(background)) {
        acts <- paste0(glue("export-background:{shQuote(background)};"), acts)
    }
    args <- c(sprintf('--actions=%s', shQuote(acts)))
    fundsr_msg(glue("Executing {shQuote(inkscape)} {paste(args, collapse = ' ')}"), level = 1L)
    exit_status <- system2(inkscape, args = args)
    if (exit_status == 0) {
        clear_inkscape_queue()
    } else {
        fundsr_msg(glue("export_pngs: Inkscape returned non-zero exit status: {exit_status}"),
                       level = 0L)
    }
    exit_status
}

#' Clear queued Inkscape exports
#'
#' Clears the internal Inkscape export queue (`.fundsr$inkscape_queue`), removing all
#' queued export commands.
#'
#' @return Invisibly returns `NULL`. Called for side effects.
#'
#' @family plot export utilities
#' @export
#'
#' @examples
#' clear_inkscape_queue()
clear_inkscape_queue <- function() {
    .fundsr$inkscape_queue <- character()
    invisible(NULL)
}

add_gg_params <- function(p, gg_params) {
    if (is.null(gg_params)) return(p)

    if (!is.list(gg_params)) gg_params <- list(gg_params)
    repeat {
        any_list <- any(vapply(gg_params, is.list, logical(1)))
        if (!any_list) break
        gg_params <- purrr::list_flatten(gg_params)
    }

    tryCatch(
        purrr::reduce(gg_params, `+`, .init = p),
        error = function(e) {
            stop(
                "Invalid `gg_params`: must contain ggplot components (layers/scales/themes/etc.).\n",
                "Underlying error: ", conditionMessage(e),
                call. = FALSE
            )
        }
    )
}

keep_supported_breaks <- function(breaks, min_date, max_date) {
    breaks <- sort(unique(breaks))
    if (length(breaks) <= 1L) return(breaks)

    b <- as.numeric(breaks)
    mids <- (b[-1] + b[-length(b)]) / 2

    left  <- c(-Inf, mids)
    right <- c(mids, Inf)

    mn <- as.numeric(min_date)
    mx <- as.numeric(max_date)

    breaks[mx >= left & mn <= right]
}

#' Plot rolling return differences against benchmark
#'
#' Plots rolling annualized tracking differences (CAGR-style or log-return) for
#' selected funds against their benchmark series. The plot uses quantile-based
#' y-limits and formats the y-axis in basis points.
#'
#' @param data Input data frame containing a `date` column and one rolling-difference
#'   column per fund (specified by `funds`).
#' @param n_days Window length (in days) used to compute rolling differences (used
#'   for labelling).
#' @param funds Character vector of fund column names to include.
#' @param use_log Logical; if `TRUE`, labels the plot as log-return differences.
#'   If `FALSE`, labels the plot as CAGR differences.
#' @param gg_params Optional ggplot components to add to the plot.
#' @param title_add Optional title suffix. Can be a single string or a named
#'   character vector specifying the title in multiple languages (e.g. `c(en=..., bg=...)`).
#' @param date_brk Optional date-break specification for the x-axis (e.g. `"3 months"`).
#'   If `NULL`, it is chosen automatically based on the time span and available data.
#' @param qprob Two-element numeric vector giving lower and upper quantiles used
#'   to set the baseline y-axis limits. Defaults to `c(0.005, 0.995)`.
#' @param bmark_type Benchmark type used in the title: `"net"` or `"gross"`.
#'
#' @return A ggplot object.
#'
#' @details
#' The function reshapes `data` to long format and produces a scatter plot coloured
#' by fund. The y-axis limits are primarily determined from quantiles of the rolling
#' differences (as specified by `qprob`), always including 0.
#'
#' To avoid clipping recent extremes, the y-limits are expanded (if needed) to also
#' include the full range observed in the most recent 30 days of data, even when
#' those values fall outside the `qprob` quantiles.
#'
#' The x-axis breaks are chosen as follows when `date_brk` is `NULL`: for spans of
#' up to 3 years, breaks default to `"3 months"`. For longer spans, breaks are
#' anchored to calendar months (semiannual or quarterly depending on span) and
#' are included only when the data range extends beyond the midpoint to the
#' neighboring break.
#'
#' @family rolling difference functions
#' @export

plot_roll_diffs <- function(data,
                      n_days,
                      funds,
                      use_log = FALSE,
                      gg_params = NULL,
                      title_add = NULL,
                      date_brk = NULL,
                      qprob = c(0.005, 0.995),
                      bmark_type = c("net", "gross")) {
    bmark_type <- match.arg(bmark_type)
    bmark_type <- if (bmark_type == "net") gettext("net") else gettext("gross")
    title_add <- pick_user_trans(title_add)
    ttl <- if (is.null(title_add)) "" else paste(":", title_add)
    title_msg <- if (use_log) {
        gettext("{n_days}d rolling log-return differences vs {bmark_type} benchmark{ttl}")
    } else {
        gettext("{n_days}d rolling CAGR differences vs {bmark_type} benchmark{ttl}")
    }
    title <- glue(title_msg)
    fundsr_msg(paste("plot_roll_diffs:", title), level = 1L)

    vals <- select(data, all_of(funds))
    has_data <- rowSums(is.finite(data.matrix(vals))) > 0
    if (!any(has_data)) {
        # fallback: use full date span
        warning("plot_roll_diffs: no finite data for selected funds.",
                call. = FALSE)
        min_date <- min(data[["date"]], na.rm = TRUE)
        max_date <- max(data[["date"]], na.rm = TRUE)
    } else {
        min_date <- min(data[["date"]][has_data], na.rm = TRUE)
        max_date <- max(data[["date"]][has_data], na.rm = TRUE)
    }
    min_yr <- lubridate::year(min_date)
    max_yr <- lubridate::year(max_date)
    n_yrs <- max_yr - min_yr
    if (!is.null(date_brk)) {
        date_breaks <- date_brk
        date_break_dates <- NULL
    } else if (n_yrs <= 3) {
        date_breaks <- "3 months"
        date_break_dates <- NULL
    } else {
        yrs <- (min_yr - 1):(max_yr + 1)
        months <- if (n_yrs >= 8) c(1, 7) else c(1, 4, 7, 10)
        date_break_dates <- as.Date(sprintf(
            "%d-%02d-01",
            rep(yrs, each = length(months)),
            rep(months, times = length(yrs))
        ))
        date_break_dates <- keep_supported_breaks(date_break_dates, min_date, max_date)
        date_breaks <- NULL
    }

    cdata <- longer(data, funds, values_to = "roll_diff")
    qlims <- stats::quantile(cdata$roll_diff,
                      probs = qprob,
                      na.rm = TRUE)
    # force-include last 30 days (even if outside quantiles)
    cutoff <- max(cdata$date, na.rm = TRUE) - lubridate::days(30)
    recent_range <- cdata %>%
        filter(.data$date >= cutoff) %>%
        summarize(
            lo = suppressWarnings(min(.data$roll_diff, na.rm = TRUE)),
            hi = suppressWarnings(max(.data$roll_diff, na.rm = TRUE))
        )
    recent_lo <- recent_range$lo
    recent_hi <- recent_range$hi
    if (!is.finite(recent_lo) || !is.finite(recent_hi)) {
        recent_lo <- NA_real_
        recent_hi <- NA_real_
    }
    y_lims <- range(c(qlims, 0, recent_lo, recent_hi), na.rm = TRUE)

    p <- ggplot2::ggplot(cdata,
                         ggplot2::aes(x = .data$date, y = .data$roll_diff, color = .data$fund)) +
        ggplot2::geom_point(alpha = 0.5, size = 1.5) +
        ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(alpha = 1, size = 3.2))) +
        ggplot2::scale_x_date(
            breaks      = if (is.null(date_break_dates)) ggplot2::waiver() else date_break_dates,
            date_breaks = if (is.null(date_breaks)) ggplot2::waiver() else date_breaks,
            date_labels = "%Y-%m"
        ) +
        ggplot2::scale_y_continuous(
            labels = function(x) x * 10000
        ) +
        ggplot2::coord_cartesian(ylim = y_lims) +
        ggplot2::expand_limits(y = 0) +
        # scale_color_distinct() +
        ggplot2::theme_minimal() +
        ggplot2::theme(
            text = ggplot2::element_text(size = 12),
            panel.grid.minor.x = ggplot2::element_blank(),
            axis.text.x = ggplot2::element_text(angle = 30, hjust = 1)
        ) +
        ggplot2::geom_hline(yintercept = 0,
                            color = "black",
                            linewidth = 0.7,
                            linetype = "solid",
                            alpha = 0.5) +
        ggplot2::labs(
            title = title,
            color = gettext("fund"),
            x = NULL,
            y = if (use_log) {
                gettext("rolling annualized log-return difference (bps)")
            } else {
                gettext("rolling CAGR difference (bps)")
            }
        )
    add_gg_params(p, gg_params)
}

vec_key <- function(x, ignore_order = FALSE) {
    if (ignore_order) x <- sort(x)
    paste0(x, collapse = ";")
}

#' Generate and export rolling-difference and XLM plots from a specification
#'
#' Iterates over plot specifications and produces rolling-difference plots for
#' both CAGR and log-return variants. Each plot is saved via [save_plot()], and
#' optional XLM plots may also be generated. All generated plot objects are
#' stored in an environment and returned.
#'
#' @param roll_diffs A list of length 2 containing data frames, named `cagr`
#'   and `log` (CAGR differences and log-return differences).
#' @param n_days Rolling-window length in days (passed to [plot_roll_diffs()]
#'   for labelling).
#' @param plot_spec A data frame or a list of data frames describing plot
#'   parameters. Expected columns include: `plot_id`, `title`, `data_filter`,
#'   `gg_params`, `width`, `height`, `funds`.
#' @param xlm_data Optional data frame used to produce XLM plots, including
#'   `date`, `xlm`, `ticker`, and `name` columns. Defaults to `NULL`.
#' @param add_gg_params Optional ggplot component (or list of components)
#'   appended to each generated plot in addition to the per-plot `gg_params`
#'   defined in `plot_spec`. Defaults to [ggplot2::geom_blank()].
#' @param bmark_type Benchmark type used in plot titles: `"net"` or `"gross"`.
#' @param suffix Character string appended to each `plot_id` when constructing
#'   filenames and names in the returned environment. Defaults to `""`.
#' @param ... Additional arguments passed to [plot_roll_diffs()].
#'
#' @return
#' An environment containing ggplot objects. Objects are stored under names
#' corresponding to the base filenames used in [save_plot()]:
#'
#' * `plot_id{suffix}` for the CAGR variant,
#' * `plot_id{suffix}_L` for the log-return variant,
#' * and (when generated) `xlm_plot_id{suffix}` for XLM plots.
#'
#' @details
#' For each row in `plot_spec`, the function constructs both a CAGR-based and a
#' log-return-based variant using [plot_roll_diffs()] and writes the resulting
#' plots via [save_plot()], using a filename suffix (`_L`) to distinguish the
#' log-return variant. The optional `suffix` is appended to `plot_id` before
#' filenames (and environment keys) are formed. Additional arguments in `...`
#' are forwarded to [plot_roll_diffs()].
#'
#' If `plot_spec` is provided as a list of data frames, the function binds them
#' into a single specification. The `title` column may be provided as a list
#' column (e.g. to keep a multilingual named vector as a single per-row value).
#'
#' If `xlm_data` is supplied, an XLM plot is generated once per unique set of
#' Xetra tickers using [plot_xlms()]. Fund tickers are mapped to Xetra tickers
#' via the `fundsr.xetra_map` option (tickers not present in the map are used
#' as-is). The first plot specification encountered for a given ticker set
#' determines the base filename `xlm_<plot_id{suffix}>` used for saving and
#' storing the resulting XLM plot.
#' @family plot export utilities
#' @export
#'
#' @examples
#' \dontrun{
#' plots <- run_plots(roll_diffs, n_days, plot_spec, xlm_data = xlm_data)
#' plots[["global"]]
#' plots[["global_L"]]
#' plots[["xlm_global"]]
#' }
run_plots <- function(roll_diffs,
                      n_days,
                      plot_spec,
                      xlm_data = NULL,
                      add_gg_params = ggplot2::geom_blank(),
                      bmark_type = c("net", "gross"),
                      suffix = "",
                      ...) {
    if (!is.list(roll_diffs) ||
        length(roll_diffs) != 2L ||
        !setequal(names(roll_diffs), c("cagr", "log"))) {
        stop("roll_diffs must be a list with names `cagr` and `log`.", call. = FALSE)
    }
    variants <- names(roll_diffs)
    if (is.list(plot_spec) && !inherits(plot_spec, "data.frame")) {
        ensure_title_col <- function(df, col = "title") {
            if (!col %in% names(df)) return(df)
            x <- df[[col]]
            if (is.list(x)) return(df)
            if (is.character(x)) {
                is_multilang <- !is.null(names(x)) && any(nzchar(names(x)))
                # If it's a multilingual named vector, keep it as one element
                # (common in 1-row specs)
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
        plot_spec <- bind_rows(lapply(plot_spec, ensure_title_col))
    }
    runs <- tidyr::crossing(plot_spec, variants)
    plots_env <- new.env(parent = emptyenv())
    xetra_map <- fundsr_get_option("xetra_map")
    bmark_type <- match.arg(bmark_type)
    .fundsr$done_xlm_sets <- character()
    purrr::pwalk(runs, function(plot_id,
                                title,
                                data_filter,
                                gg_params,
                                width,
                                height,
                                funds,
                                variants) {
        plot_id <- paste0(plot_id, suffix)
        use_log <- variants == "log"
        data <- if (use_log) roll_diffs$log else roll_diffs$cagr
        if (!is.null(data_filter)) data <- data %>% data_filter()
        plot <- plot_roll_diffs(data,
                         n_days,
                         funds = funds,
                         use_log = use_log,
                         gg_params = list(gg_params, add_gg_params),
                         title_add = title,
                         bmark_type = bmark_type,
                         ...)
        fname <- paste0(plot_id, if (use_log) "_L" else "")
        save_plot(fname, plot, width = width, height = height)
        plots_env[[fname]] <- plot

        if (!is.null(xlm_data)) {
            funds_xet <- ifelse(funds %in% names(xetra_map),
                                xetra_map[funds],
                                funds)
            key <- vec_key(funds_xet, ignore_order = TRUE)
            if (!key %in% .fundsr$done_xlm_sets) {
                xlm_plot <- plot_xlms(xlm_data,
                                     funds_xet,
                                     gg_params = list(gg_params, add_gg_params),
                                     back_trans = TRUE)
                xlm_fname <- paste0("xlm_", plot_id)
                save_plot(xlm_fname, xlm_plot, width = width, height = height)
                plots_env[[xlm_fname]] <- xlm_plot
                .fundsr$done_xlm_sets <- c(.fundsr$done_xlm_sets, key)
            }
        }
    })
    plots_env
}
