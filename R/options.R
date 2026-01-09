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
#' @param xetra_map Named character vector mapping "primary" tickers (series
#'   table column names) to Xetra tickers (sets `fundsr.xetra_map`).
#' @param inkscape Inkscape executable (path or command name) used by export
#'   helpers (sets `fundsr.inkscape`).
#' @param reload Logical; default value for forcing re-import of cached
#'   objects (sets `fundsr.reload`).
#' @param fund_urls Named character vector or named list of URLs for fund data
#'   downloads (sets `fundsr.fund_urls`).
#'
#' @return Invisibly returns a named list of the previous values of the options
#'   that were changed (as returned by `options()`).
#' @seealso
#' [add_fund_urls()] to add/update entries in `fundsr.fund_urls`.
#'
#' @family config functions
#' @export
fundsr_options <- function(data_dir = NULL,
                           out_dir = NULL,
                           px_width = NULL,
                           internal_png = NULL,
                           export_svg = NULL,
                           xetra_map = NULL,
                           inkscape = NULL,
                           reload = NULL,
                           fund_urls = NULL) {
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

    if (!is.null(reload)) {
        if (!is.logical(reload) || length(reload) != 1L || is.na(reload)) {
            stop("`reload` must be TRUE or FALSE.", call. = FALSE)
        }
        set$fundsr.reload <- reload
    }

    if (!is.null(fund_urls)) {
        if ((!is.character(fund_urls) && !is.list(fund_urls)) ||
            (length(fund_urls) > 0 &&
                (is.null(names(fund_urls)) || any(!nzchar(names(fund_urls)))))) {
            stop("`fund_urls` must be a named character vector or named list (or empty).",
                 call. = FALSE)
        }
        set$fundsr.fund_urls <- fund_urls
    }

    if (length(set) == 0L) {
        return(invisible(list()))
    }

    do.call(options, set)
}

#' Add entries to the fund download list
#'
#' Adds one or more named download specifications to the `fundsr.fund_urls`
#' option. Existing entries are preserved; entries in `x` replace any
#' existing entries with the same name.
#'
#' Names are converted to uppercase before storing.
#'
#' @param x A named character vector mapping download identifiers to URLs.
#'
#' @return A list with the previous value of `fundsr.fund_urls`.
#' @seealso
#' [fundsr_options()] to set `fundsr.fund_urls` (and other fundsr options) in one call.
#' [download_fund_data()] to download files from the configured URLs.
#'
#' @family download functions
#' @export
add_fund_urls <- function(x) {
    stopifnot(is.character(x), !is.null(names(x)), all(nzchar(names(x))))

    names(x) <- toupper(names(x))
    cur <- getOption("fundsr.fund_urls", character())
    if (length(cur) && !is.null(names(cur))) {
        names(cur) <- toupper(names(cur))
    }

    new <- c(cur, x)
    new <- new[!duplicated(names(new), fromLast = TRUE)]
    options(fundsr.fund_urls = new)
}

find_inkscape <- function(
        candidates = NULL,
        env_var = "INKSCAPE",
        option_name = "fundsr.inkscape"
) {
    safe_norm <- function(x) {
        tryCatch(normalizePath(x, winslash = "/", mustWork = FALSE), error = function(e) x)
    }

    override <- getOption(option_name, Sys.getenv(env_var, unset = NA_character_))
    if (!is.na(override) && nzchar(override) && file.exists(override)) {
        return(safe_norm(override))
    }

    p <- Sys.which("inkscape")
    if (nzchar(p) && file.exists(p)) {
        return(safe_norm(p))
    }

    if (is.null(candidates)) {
        sys <- Sys.info()[["sysname"]]
        if (identical(sys, "Windows")) {
            pf   <- Sys.getenv("ProgramFiles", "C:/Program Files")
            pf86 <- Sys.getenv("ProgramFiles(x86)", "C:/Program Files (x86)")
            candidates <- c(
                file.path(pf,   "Inkscape/bin/inkscape.exe"),
                file.path(pf,   "Inkscape/inkscape.exe"),
                file.path(pf86, "Inkscape/bin/inkscape.exe"),
                file.path(pf86, "Inkscape/inkscape.exe")
            )
        } else { # Mac, Linux, etc.
            candidates <- c(
                "/Applications/Inkscape.app/Contents/MacOS/inkscape",
                "/Applications/Inkscape.app/Contents/MacOS/Inkscape",
                "/opt/homebrew/bin/inkscape",
                "/usr/local/bin/inkscape",
                "/opt/local/bin/inkscape",
                "/usr/bin/inkscape",
                "/snap/bin/inkscape"
            )
        }
    }

    hit <- candidates[file.exists(candidates)][1]
    if (!is.na(hit) && nzchar(hit)) {
        return(safe_norm(hit))
    }

    NA_character_
}
