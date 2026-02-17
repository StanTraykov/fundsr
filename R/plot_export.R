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
#'   for immediate PNG saving.
#' @param height Height of the saved plot in `units`.
#' @param width Width of the saved plot in `units`.
#' @param units Units for `width`/`height` (e.g. `"in"`).
#'   For immediate PNG saving, only `"in"`, `"cm"`, and `"mm"` are supported (to
#'   compute DPI from `px_width`).
#' @param out_dir Output directory where files are written.
#' @param save_png Logical scalar; if `TRUE`, also saves a PNG immediately.
#' @param save_svg Logical scalar; if `TRUE`, saves the SVG and queues an
#'   Inkscape export action.
#' @param background Background color used for immediate PNG saving via
#'   [ggplot2::ggsave()] (`bg`).
#' @param session Optional `fundsr_session` object. Defaults to the package
#'   default session when `NULL`.
#'
#' @return Invisibly returns `NULL`. Called for side effects.
#'
#' @details
#' If `save_svg = TRUE`, the SVG is written as `"{file}.svg"`. An Inkscape action
#' string is then stored in `session$state$inkscape_queue[file]` so the SVG can later be
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
                      background = "white",
                      session = NULL) {
    check_string(file)
    check_logical(save_png)
    check_logical(save_svg)
    if (!save_svg && !save_png) {
        fundsr_warn("Nothing to do: both `save_svg` and `save_png` are FALSE.")
        return(invisible(NULL))
    }

    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
    fname <- function(fn, ext) file.path(out_dir, glue("{fn}.{ext}"))
    svgf <- fname(file, "svg")
    pngf <- fname(file, "png")

    if (save_svg) {
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
            fundsr_abort(
                msg = c(
                    "Cannot queue Inkscape export: path(s) contain ';'.",
                    paste0("- ", bad_items),
                    "Rename the affected file(s) (or export to a path without ';') and try again."
                ),
                class = c("fundsr_export_error", "fundsr_bad_path")
            )
        }
        a <- glue(
            "file-open:{svgf};export-filename:{pngf};export-width:{px_width};export-do;file-close"
        )
        st <- fundsr_require_state(session = session)$state
        if (is.null(st$inkscape_queue)) clear_inkscape_queue(session = session)
        st$inkscape_queue[file] <- a
    }

    if (save_png) {
        width_in <- switch(
            units,
            "in" = width,
            "cm" = width / 2.54,
            "mm" = width / 25.4,
            stop_bad_arg("units", "must be one of: \"in\", \"cm\", \"mm\" for saving PNG.")

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
#' @param session Optional `fundsr_session` object. Defaults to the package
#'   default session when `NULL`.
#'
#' @return
#' The exit status returned by Inkscape (0 indicates success). Invisibly returns
#' `NULL` if the queue is empty or if Inkscape cannot be located.
#'
#' @details
#' This function reads queued Inkscape actions from `session$state$inkscape_queue`,
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
export_pngs <- function(background = "white", session = NULL) {
    check_string(background, allow_null = TRUE)
    st <- fundsr_require_state(session = session)$state

    if (length(st$inkscape_queue) == 0) {
        fundsr_msg("export_pngs: nothing queued.", level = 1L)
        return(invisible(NULL))
    }
    inkscape <- find_inkscape()
    if (is.na(inkscape) || !nzchar(inkscape)) {
        fundsr_msg("export_pngs: cannot find Inkscape", level = 0L)
        return(invisible(NULL))
    }
    acts <- paste(st$inkscape_queue, collapse = ";")
    if (!is.null(background)) {
        acts <- paste0(glue("export-background:{shQuote(background)};"), acts)
    }
    args <- c(sprintf("--actions=%s", shQuote(acts)))
    fundsr_msg(glue("Executing {shQuote(inkscape)} {paste(args, collapse = ' ')}"), level = 1L)
    exit_status <- system2(inkscape, args = args)
    if (exit_status == 0) {
        clear_inkscape_queue(session = session)
    } else {
        fundsr_msg(glue("export_pngs: Inkscape returned non-zero exit status: {exit_status}"),
                   level = 0L)
    }
    exit_status
}

#' Clear queued Inkscape exports
#'
#' Clears the internal Inkscape export queue (`session$state$inkscape_queue`), removing all
#' queued export commands.
#'
#' @param session Optional `fundsr_session` object. Defaults to the package
#'   default session when `NULL`.
#'
#' @return Invisibly returns `NULL`. Called for side effects.
#'
#' @family plot export utilities
#' @export
#'
#' @examples
#' clear_inkscape_queue()
clear_inkscape_queue <- function(session = NULL) {
    session <- fundsr_get_session(session)
    st <- session$state

    if (!is.environment(st)) {
        return(invisible(NULL))
    }

    st$inkscape_queue <- character()
    invisible(NULL)
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
#' @param session Optional `fundsr_session` object. Defaults to the package
#'   default session when `NULL`.
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
                      session = NULL,
                      ...) {
    check_numeric_scalar(n_days, whole_num = TRUE, ge = 1)
    if (!is.list(roll_diffs) ||
            length(roll_diffs) != 2L ||
            !setequal(names(roll_diffs), c("cagr", "log"))) {
        stop_bad_arg("roll_diffs", "must be a list with names `cagr` and `log`.")
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
                if (is_multilang && nrow(df) == 1L) {
                    df[[col]] <- list(x)
                    return(df)
                }
                # Otherwise treat as per-row scalar titles
                df[[col]] <- as.list(x)
                return(df)
            }
            stop_bad_arg("plot_spec", sprintf("column %s must be character or list.", sQuote(col)))
        }
        plot_spec <- bind_rows(lapply(plot_spec, ensure_title_col))
    }
    runs <- tidyr::crossing(plot_spec, variants)
    plots_env <- new.env(parent = emptyenv())
    xetra_map <- fundsr_get_option("xetra_map")
    bmark_type <- match.arg(bmark_type)
    st <- fundsr_require_state(session = session)$state
    st$done_xlm_sets <- character()
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
        save_plot(fname, plot, width = width, height = height, session = session)
        plots_env[[fname]] <- plot

        if (!is.null(xlm_data)) {
            funds_xet <- ifelse(funds %in% names(xetra_map),
                                xetra_map[funds],
                                funds)
            key <- vec_key(funds_xet, ignore_order = TRUE)
            if (!key %in% st$done_xlm_sets) {
                xlm_plot <- plot_xlms(xlm_data,
                                      funds_xet,
                                      gg_params = list(gg_params, add_gg_params),
                                      back_trans = TRUE)
                xlm_fname <- paste0("xlm_", plot_id)
                save_plot(xlm_fname, xlm_plot, width = width, height = height, session = session)
                plots_env[[xlm_fname]] <- xlm_plot
                st$done_xlm_sets <- c(st$done_xlm_sets, key)
            }
        }
    })
    plots_env
}
