#' Clear registered data loaders
#'
#' Clears the internal data-loader registry (`session$state$data_loaders`), removing
#' all previously registered data loader functions.
#'
#' @param session Optional `fundsr_session` object. Defaults to the package
#'   default session when `NULL`.
#'
#' @return Invisibly returns `NULL`. Called for side effects.
#'
#' @family fund/index workflow functions
#' @export
#' @examples
#' clear_data_loaders()
clear_data_loaders <- function(session = NULL) {
    session <- fundsr_get_session(session)
    st <- session$state

    if (!is.environment(st)) {
        return(invisible(NULL))
    }

    st$data_loaders <- list()
    invisible(NULL)
}

#' Register a data loader
#'
#' Appends `fun` to the internal data-loader registry (`session$state$data_loaders`).
#' Registered functions are intended to be run sequentially in registration
#' order.
#'
#' If a loader with the same function body is already registered, `fun` is not
#' added again.
#'
#' @param fun A function to register. Must take no arguments.
#' @param session Optional `fundsr_session` object. Defaults to the package
#'   default session when `NULL`.
#'
#' @return Invisibly returns the updated `session$state$data_loaders` list.
#'
#' @family fund/index workflow functions
#' @export
#'
#' @examples
#' add_data_loader(function() NULL)
add_data_loader <- function(fun, session = NULL) {
    if (!identical(typeof(fun), "closure")) {
        stop_bad_arg("fun", "must be an R function (a closure, not a primitive/builtin).")
    }
    if (length(formals(fun)) != 0L) {
        stop_bad_arg("fun", "must take no arguments.")
    }

    st <- fundsr_require_state(session = session)$state
    if (is.null(st$data_loaders)) {
        st$data_loaders <- list()
    }
    if (!is.list(st$data_loaders)) {
        fundsr_abort(
            msg   = "Internal registry `session$state$data_loaders` must be a list.",
            class = "fundsr_bad_state"
        )
    }

    fns <- st$data_loaders
    sig <- function(f) paste(deparse(body(f)), collapse = "\n")
    fun_sig <- sig(fun)

    already <- any(vapply(fns, function(g) {
        if (!identical(typeof(g), "closure")) {
            fundsr_abort(
                msg   = "Internal registry `session$state$data_loaders` must contain only closures.",
                class = "fundsr_bad_state"
            )
        }
        identical(sig(g), fun_sig)
    }, logical(1)))

    if (!already) {
        st$data_loaders <- c(fns, list(fun))
    }

    invisible(st$data_loaders)
}

#' Run registered data loaders
#'
#' Runs the data loader registry (`session$state$data_loaders`) to populate (or refresh)
#' the package's storage environment (`session$storage`).
#'
#' The function temporarily sets the `fundsr.reload` option so that data loaders
#' can decide whether to recompute cached objects.
#'
#' @param reload Logical scalar. If `TRUE`, forces a full reload by setting
#'   `options(fundsr.reload = TRUE)` for the duration of this call.
#' @param session Optional `fundsr_session` object. Defaults to the package
#'   default session when `NULL`.
#'
#' @return Invisibly returns `session$storage` after running the data loaders.
#'
#' @details
#' The previous value of option "fundsr.reload" is restored on exit,
#' even if a data loader errors.
#'
#' Data loaders are taken from `session$state$data_loaders` and are called
#' sequentially in registration order. Each registered function must take
#' no arguments.
#'
#' @family fund/index workflow functions
#' @export
run_data_loaders <- function(reload = FALSE, session = NULL) {
    check_logical(reload)
    session <- fundsr_require_state(storage = TRUE, session = session)
    st <- session$state
    storage <- session$storage

    fns <- st$data_loaders
    if (is.null(fns)) fns <- list()

    if (!is.list(fns)) {
        fundsr_abort(
            msg   = "Internal registry `session$state$data_loaders` must be a list of functions.",
            class = "fundsr_bad_state"
        )
    }

    for (i in seq_along(fns)) {
        fn <- fns[[i]]
        if (!identical(typeof(fn), "closure")) {
            fundsr_abort(
                msg   = sprintf("`session$state$data_loaders[[%d]]` is not a closure.", i),
                class = "fundsr_bad_state"
            )
        }
        if (length(formals(fn)) != 0L) {
            fundsr_abort(
                msg   = sprintf("`session$state$data_loaders[[%d]]` must take no arguments.", i),
                class = "fundsr_bad_state"
            )
        }
    }

    old <- fundsr_get_option("reload")
    options(fundsr.reload = reload)
    on.exit(options(fundsr.reload = old), add = TRUE)

    for (i in seq_along(fns)) {
        fn <- fns[[i]]
        tryCatch(
            fn(),
            error = function(e) {
                fundsr_abort(
                    msg    = sprintf("Data loader %d failed.", i),
                    class  = "fundsr_loader_failed",
                    parent = e
                )
            }
        )
    }

    invisible(storage)
}
