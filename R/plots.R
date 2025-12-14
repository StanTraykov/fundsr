# © 2023–2025 Stanislav Traykov <st@gmuf.com>.
# Personal use and non-commercial result sharing permitted; commercial use requires permission.

#' Save a plot as SVG and queue it for optional PNG export via Inkscape
#'
#' Saves `plot` as an SVG file and appends an Inkscape export command to the
#' internal queue (`.fundsr$ink_queue`) so PNG generation can be performed later
#' in batch (via \code{ggexport()}).
#'
#' Optionally, the function can also save a PNG immediately via
#' \code{ggplot2::ggsave()} (in addition to queueing the Inkscape command).
#'
#' @param file Base filename (without extension) used for both the SVG and PNG.
#' @param plot A plot object (typically a ggplot) to be saved.
#' @param px_width Width in pixels for the queued PNG export command, and (if
#'   `save_png = TRUE`) for the PNG saved immediately. Defaults to
#'   `getOption("fundsr.px_width", 1300)`.
#' @param height Height of the saved SVG. Defaults to `12`.
#' @param width Width of the saved SVG. Defaults to `12`.
#' @param units Units for `height` and `width` (e.g. `"in"`). Defaults to `"in"`.
#' @param out_dir Output directory where files are written. Defaults to
#'   `getOption("fundsr.out_dir", "output")`.
#' @param save_png Logical scalar; if `TRUE`, also saves a PNG immediately.
#'   Defaults to `getOption("fundsr.internal_png", FALSE)`.
#'
#' @return Invisibly returns `NULL`. Called for side effects.
#'
#' @details
#' The SVG is always written to `out_dir` as `"{file}.svg"`. An Inkscape export
#' action string is stored in `.fundsr$ink_queue[[file]]` for later batch PNG
#' export to `"{file}.png"` at `px_width` pixels wide.
#'
#' If `save_png = TRUE`, a PNG is also written immediately using
#' \code{ggplot2::ggsave()} with `units = "px"`. The PNG height is computed as
#' `round(px_width * (height / width))` to preserve the aspect ratio implied by
#' the SVG sizing.
#'
#' @export
ggink <- function(file,
                  plot,
                  px_width = getOption("fundsr.px_width", 1300),
                  height = 12,
                  width = 12,
                  units = "in",
                  out_dir = getOption("fundsr.out_dir", "output"),
                  save_png = getOption("fundsr.internal_png", FALSE)) {
    if (!is.logical(save_png) || length(save_png) != 1L || is.na(save_png)) {
        stop("`save_png` must be TRUE or FALSE.", call. = FALSE)
    }
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
    fname <- function(fn, ext) file.path(out_dir, glue("{fn}.{ext}"))
    svgf <- fname(file, "svg")
    pngf <- fname(file, "png")

    ggplot2::ggsave(
        svgf,
        plot = plot,
        height = height,
        width = width,
        units = units
    )

    a <- glue(
        "file-open:{svgf};export-filename:{pngf};export-width:{px_width};export-do;file-close"
    )
    .fundsr$ink_queue[file] <- a

    if (isTRUE(save_png)) {
        width_in <- switch(
            units,
            `in` = width,
            cm = width / 2.54,
            mm = width / 25.4,
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
            bg = "white",
            limitsize = FALSE
        )
    }

    invisible(NULL)
}

#' Export queued SVG files to PNG using Inkscape
#'
#' Processes all pending items in the internal Inkscape export queue and
#' invokes Inkscape to generate PNG files from previously saved SVGs.
#'
#' @return The exit status returned by the system call to Inkscape.
#'   Invisibly returns `NULL` if no items are queued.
#'
#' @details
#' This function collects all queued Inkscape actions stored in
#' `.fundsr$ink_queue`, constructs a single Inkscape command using the
#' executable specified by the `fundsr.inkscape` option, and executes it
#' via `system()`. On success, the queue is cleared. On failure, the queue
#' is preserved and a message is printed.
#'
#' @export
ggexport <- function() {
    if (length(.fundsr$ink_queue) == 0) {
        message("ggexport: nothing queued.")
        return(invisible(NULL))
    }
    inkscape <- getOption("fundsr.inkscape", "/usr/bin/inkscape")
    acts <- paste(.fundsr$ink_queue, collapse = ";")
    acts <- paste0('export-background:white;', acts)
    args <- c(sprintf('--actions=%s', shQuote(acts)))
    message(glue("Executing {shQuote(inkscape)} {paste(args, collapse = ' ')}"))
    exit_status <- system2(inkscape, args = args)
    if (exit_status == 0) {
        .fundsr$ink_queue <- character()
    } else {
        message(glue("ggexport: Inkscape returned non-zero exit status: {exit_status}"))
    }
    exit_status
}

add_gg_params <- function(p, gg_params) {
    if (is.null(gg_params)) return(p)
    if (!is.list(gg_params)) {
        gg_params <- list(gg_params)
    }
    # reduce over the list to add them one by one
    p <- purrr::reduce(gg_params, `+`, .init = p)
}

#' Plot rolling return differences against benchmark
#'
#' Creates a scatter plot of rolling CAGR or log-return differences for the
#' selected funds, including quantile-based y-limits, optional title additions,
#' and customizable ggplot parameters.
#'
#' @param data Input data frame containing dates and rolling-difference columns.
#' @param n_days Window length (in days) used to compute rolling differences.
#' @param funds Character vector of fund tickers to include.
#' @param use_log Logical; if `TRUE`, use log-return differences instead of CAGR.
#' @param gg_params Optional list of additional ggplot layers or modifications.
#' @param title_add Optional string appended to the title.
#' @param date_brk Optional date-break specification for the x-axis; computed
#'   automatically if omitted.
#' @param qprob Two-element numeric vector giving lower and upper quantiles used
#'   to set the y-axis limits.
#'
#' @return A ggplot object.
#'
#' @details
#' The function reshapes input data into long format, computes quantile-based
#' y-limits, selects appropriate date breaks, and produces a scatter plot of
#' rolling differences coloured by fund. Additional ggplot layers may be added
#' via `gg_params`.
#'
#' @export
rcd_plot <- function(data,
                      n_days,
                      funds,
                      use_log = FALSE,
                      gg_params = NULL,
                      title_add = NULL,
                      date_brk = NULL,
                      qprob = c(0.005, 0.995)) {
    title_add <- pick_user_trans(title_add)
    ttl <- if (is.null(title_add)) "" else paste(":", title_add)
    title_msg <- if (use_log) {
        gettext("{n_days}d rolling log-return differences vs net benchmark{ttl}")
    } else {
        gettext("{n_days}d rolling CAGR differences vs net benchmark{ttl}")
    }
    title <- glue(title_msg)
    message(paste("rcd_plot:", title))
    if (is.null(date_brk)) {
        years <- data %>%
            summarize(start_year = lubridate::year(first(date)),
                      end_year   = lubridate::year(last(date))) %>%
            unlist() %>%
            diff()
        date_brk <- ifelse(years >= 10, "6 months", "3 months")
    }
    cdata <- longer(data, funds, "_rcd", "roll_diff")
    qlims <- stats::quantile(cdata$roll_diff,
                      probs = qprob,
                      na.rm = TRUE)
    y_lims <- range(c(qlims, 0))
    p <- ggplot(cdata, aes(x = .data$date, y = .data$roll_diff, color = .data$fund)) +
        geom_point(alpha = 0.5, size = 1.5) +
        guides(color = guide_legend(override.aes = list(alpha = 1, size = 3.2))) +
        scale_x_date(
            date_breaks = date_brk,
            date_labels = "%Y-%m"     # Format labels 2024-01
        ) +
        scale_y_continuous(
            labels = function(x) x * 10000
        ) +
        coord_cartesian(ylim = y_lims) +
        expand_limits(y = 0) +
        # scale_color_distinct() +
        theme_minimal() +
        theme(
            text = element_text(size = 12),
            panel.grid.minor.x = element_blank(),
            axis.text.x = element_text(angle = 30, hjust = 1)
        ) +
        geom_hline(yintercept = 0,
                   color = "black",
                   linewidth = 0.7,
                   linetype = "solid",
                   alpha = 0.5) +
        labs(
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

#' Generate and export all configured rolling-difference plots
#'
#' Iterates over the plot specifications and produces rolling-difference
#' plots for both CAGR and log-return variants. Each plot is saved via
#' [ggink()], and optional XLM comparison plots may also be generated.
#'
#' @param rcds Data frame containing rolling CAGR differences.
#' @param rcds_log Data frame containing rolling log-return differences.
#' @param n_days Rolling-window length in days.
#' @param plot_spec A data frame describing plot parameters such as plot
#'   IDs, titles, filters, sizing, and fund sets.
#' @param xlm_data Optional data frame used to produce XLM comparison plots;
#'   typically includes `date`, `xlm`, `ticker`, and `name` columns.
#' @param add_gg_params Optional ggplot component (or list of components)
#'   appended to each generated plot in addition to the per-plot `gg_params`
#'   defined in `plot_spec`. Defaults to [ggplot2::geom_blank()].
#'
#' @return Invisibly returns `NULL`. Plots are saved as a side effect.
#'
#' @details
#' For each row in `plot_spec`, the function constructs both a CAGR-based
#' and a log-return-based variant using [rcd_plot()] and writes the
#' resulting plots via [ggink()], using a filename suffix to distinguish
#' the log-return variant. The `plot_spec` is expected to provide columns
#' such as `plot_id`, `title`, `filter`, `gg_params`, `width`, `height`,
#' and `funds`.
#'
#' If `xlm_data` is supplied, a separate Xetra Liquidity Measure (XLM)
#' plot is generated once per unique combination of funds, using
#' [xlm_plot()]. The mapping from fund tickers to Xetra tickers is taken
#' from the option `fundsr.xetra_map`.
#'
#' @export

run_plots <- function(rcds,
                       rcds_log,
                       n_days,
                       plot_spec,
                       xlm_data = NULL,
                       add_gg_params = geom_blank()) {
    variants <- c("CAGR", "log")
    runs <- tidyr::crossing(plot_spec, variants)
    .fundsr$done_xlms <- character()
    purrr::pwalk(runs, function(plot_id,
                                title,
                                filter,
                                gg_params,
                                width,
                                height,
                                funds,
                                variants) {
        use_log = variants == "log"
        data <- if (use_log) rcds_log else rcds
        if (!is.null(filter)) data <- data %>% filter()
        plot <- rcd_plot(data,
                          n_days,
                          funds = funds,
                          use_log = use_log,
                          gg_params = list(gg_params, add_gg_params),
                          title_add = title)
        fname <- paste0(plot_id, if (use_log) "_L" else "")
        ggink(fname, plot, width = width, height = height)
        if (!is.null(xlm_data)) {
            funds_u <- toupper(funds)
            xetra_map <- getOption("fundsr.xetra_map")
            funds_xet <- ifelse(funds_u %in% names(xetra_map),
                                xetra_map[funds_u],
                                funds_u)
            key <- vec_key(funds_xet, ignore_order = TRUE)
            if (!key %in% .fundsr$done_xlms) {
                xlm_plot <- xlm_plot(xlm_data,
                                     funds_xet,
                                     gg_params = list(gg_params, add_gg_params))
                ggink(paste0("xlm_", plot_id), xlm_plot, width = width, height = height)
                .fundsr$done_xlms <- c(.fundsr$done_xlms, key)
            }
        }
    })
}
