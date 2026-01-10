#' Clear registered data loaders
#'
#' Clears the internal data-loader registry (`.fundsr$data_loaders`), removing
#' all previously registered data loader functions.
#'
#' @return Invisibly returns `NULL`. Called for side effects.
#'
#' @family fund/index workflow functions
#' @export
#' @examples
#' clear_data_loaders()
clear_data_loaders <- function() {
    .fundsr$data_loaders <- list()
    invisible(NULL)
}

#' Register a data loader
#'
#' Appends `fun` to the internal data-loader registry (`.fundsr$data_loaders`).
#' Registered functions are intended to be run sequentially in registration
#' order.
#'
#' If a loader with the same function body is already registered, `fun` is not
#' added again.
#'
#' @param fun A function to register. Must take no arguments.
#'
#' @return Invisibly returns the updated `.fundsr$data_loaders` list.
#'
#' @family fund/index workflow functions
#' @export
#'
#' @examples
#' add_data_loader(function() NULL)
add_data_loader <- function(fun) {
    if (!is.function(fun)) {
        stop("`fun` must be a function.", call. = FALSE)
    }
    if (length(formals(fun)) != 0L) {
        stop("`fun` must take no arguments.", call. = FALSE)
    }
    if (is.null(.fundsr$data_loaders)) {
        .fundsr$data_loaders <- list()
    }
    if (!is.list(.fundsr$data_loaders)) {
        stop("Internal registry `.fundsr$data_loaders` must be a list.", call. = FALSE)
    }

    sig <- function(f) paste(deparse(body(f)), collapse = "\n")

    fun_sig <- sig(fun)
    already <- any(vapply(.fundsr$data_loaders, function(g) {
        is.function(g) && identical(sig(g), fun_sig)
    }, logical(1)))

    if (!already) {
        .fundsr$data_loaders <- c(.fundsr$data_loaders, list(fun))
    }

    invisible(.fundsr$data_loaders)
}

#' Run registered data loaders
#'
#' Runs the data loader registry (`.fundsr$data_loaders`) to populate (or refresh)
#' the package's storage environment (`.fundsr_storage`).
#'
#' The function temporarily sets the `fundsr.reload` option so that data loaders
#' can decide whether to recompute cached objects.
#'
#' @param reload Logical scalar. If `TRUE`, forces a full reload by setting
#'   `options(fundsr.reload = TRUE)` for the duration of this call.
#'
#' @return Invisibly returns `.fundsr_storage` after running the data loaders.
#'
#' @details
#' The previous value of option "fundsr.reload" is restored on exit,
#' even if a data loader errors.
#'
#' Data loaders are taken from `.fundsr$data_loaders` and are called
#' sequentially in registration order. Each registered function must take
#' no arguments.
#'
#' @family fund/index workflow functions
#' @export
run_data_loaders <- function(reload = FALSE) {
    if (!is.logical(reload) || length(reload) != 1L || is.na(reload)) {
        stop("`reload` must be TRUE or FALSE.", call. = FALSE)
    }
    if (!exists(".fundsr_storage", inherits = TRUE) || !is.environment(.fundsr_storage)) {
        stop("Fundsr storage is not initialised.", call. = FALSE)
    }
    fns <- .fundsr$data_loaders
    if (is.null(fns)) fns <- list()
    if (!is.list(fns)) {
        stop("Internal registry `.fundsr$data_loaders` must be a list of functions.", call. = FALSE)
    }
    for (i in seq_along(fns)) {
        fn <- fns[[i]]
        if (!is.function(fn)) {
            stop(sprintf("`.fundsr$data_loaders[[%d]]` is not a function.", i), call. = FALSE)
        }
        if (length(formals(fn)) != 0L) {
            stop(sprintf("`.fundsr$data_loaders[[%d]]` must take no arguments.", i), call. = FALSE)
        }
    }
    old <- fundsr_get_option("reload")
    options(fundsr.reload = reload)
    on.exit(options(fundsr.reload = old), add = TRUE)

    for (fn in fns) fn()
    invisible(.fundsr_storage)
}
