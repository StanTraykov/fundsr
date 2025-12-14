#' Set fundsr package options
#'
#' Convenience wrapper around `options()` for setting common `fundsr.*` options.
#'
#' All arguments default to `NULL`. If an argument is left as `NULL`, the
#' corresponding `fundsr.*` option is left unchanged.
#'
#' @param data_dir Directory containing fund data files (sets `fundsr.data_dir`).
#' @param out_dir Output directory for plots/exports (sets `fundsr.out_dir`).
#' @param px_width Default PNG export width in pixels (sets `fundsr.px_width`).
#' @param internal_png Logical; whether to save an internal PNG immediately when
#'   exporting plots (sets `fundsr.internal_png`).
#' @param export_svg Logical; whether to save SVGs and queue Inkscape exports
#'   (sets `fundsr.export_svg`).
#' @param xetra_map Named character vector mapping “primary” tickers to Xetra
#'   tickers (sets `fundsr.xetra_map`).
#' @param inkscape Inkscape executable (path or command name) used by export
#'   helpers (sets `fundsr.inkscape`).
#' @param redo_import Logical; default value for forcing re-import of cached
#'   objects (sets `fundsr.redo_import`).
#'
#' @return Invisibly returns a named list of the previous values of the options
#'   that were changed (as returned by `options()`).
#' @export
fundsr_options <- function(data_dir = NULL,
                           out_dir = NULL,
                           px_width = NULL,
                           internal_png = NULL,
                           export_svg = NULL,
                           xetra_map = NULL,
                           inkscape = NULL,
                           redo_import = NULL) {
    set <- list()

    if (!is.null(data_dir)) {
        if (!is.character(data_dir) || length(data_dir) != 1L || is.na(data_dir)) {
            stop("`data_dir` must be a single non-missing string.", call. = FALSE)
        }
        set$fundsr.data_dir <- data_dir
    }

    if (!is.null(out_dir)) {
        if (!is.character(out_dir) || length(out_dir) != 1L || is.na(out_dir)) {
            stop("`out_dir` must be a single non-missing string.", call. = FALSE)
        }
        set$fundsr.out_dir <- out_dir
    }

    if (!is.null(px_width)) {
        if (!is.numeric(px_width) || length(px_width) != 1L || is.na(px_width) || px_width <= 0) {
            stop("`px_width` must be a single positive number.", call. = FALSE)
        }
        set$fundsr.px_width <- as.integer(px_width)
    }

    if (!is.null(internal_png)) {
        if (!is.logical(internal_png) || length(internal_png) != 1L || is.na(internal_png)) {
            stop("`internal_png` must be TRUE or FALSE.", call. = FALSE)
        }
        set$fundsr.internal_png <- internal_png
    }

    if (!is.null(export_svg)) {
        if (!is.logical(export_svg) || length(export_svg) != 1L || is.na(export_svg)) {
            stop("`export_svg` must be TRUE or FALSE.", call. = FALSE)
        }
        set$fundsr.export_svg <- export_svg
    }

    if (!is.null(xetra_map)) {
        if (!is.character(xetra_map) || is.null(names(xetra_map)) || any(!nzchar(names(xetra_map)))) {
            stop("`xetra_map` must be a named character vector.", call. = FALSE)
        }
        set$fundsr.xetra_map <- xetra_map
    }

    if (!is.null(inkscape)) {
        if (!is.character(inkscape) || length(inkscape) != 1L || is.na(inkscape) || !nzchar(inkscape)) {
            stop("`inkscape` must be a single non-empty string.", call. = FALSE)
        }
        set$fundsr.inkscape <- inkscape
    }

    if (!is.null(redo_import)) {
        if (!is.logical(redo_import) || length(redo_import) != 1L || is.na(redo_import)) {
            stop("`redo_import` must be TRUE or FALSE.", call. = FALSE)
        }
        set$fundsr.redo_import <- redo_import
    }

    if (length(set) == 0L) {
        return(invisible(list()))
    }

    do.call(options, set)
}
