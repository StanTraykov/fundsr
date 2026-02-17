.fundsr_default_session <- NULL

#' Create a fundsr session
#'
#' Constructs a `fundsr_session` object, which encapsulates a mutable state
#' environment and a storage environment.
#'
#' @param state Environment for mutable fundsr state (fund-index map, loader
#'   registry, export queues, etc.).
#' @param storage Environment for cached series storage.
#'
#' @return An object of class `"fundsr_session"`.
#' @family config functions
#' @export
fundsr_session <- function(state = new.env(parent = emptyenv()),
                           storage = new.env(parent = emptyenv())) {
    if (!is.environment(state)) {
        stop_bad_arg("state", "must be an environment.")
    }
    if (!is.environment(storage)) {
        stop_bad_arg("storage", "must be an environment.")
    }

    if (is.null(state$data_loaders)) state$data_loaders <- list()
    if (is.null(state$fund_index_map)) state$fund_index_map <- character()
    if (is.null(state$inkscape_queue)) state$inkscape_queue <- character()
    if (is.null(state$done_xlm_sets)) state$done_xlm_sets <- character()

    structure(
        list(state = state, storage = storage),
        class = "fundsr_session"
    )
}

#' Get the default fundsr session
#'
#' Returns the package-global default `fundsr_session` object.
#'
#' @return An object of class `"fundsr_session"`.
#' @family config functions
#' @export
fundsr_default_session <- function() {
    if (!inherits(.fundsr_default_session, "fundsr_session")) {
        .fundsr_default_session <<- fundsr_session()
    }
    .fundsr_default_session
}

fundsr_get_session <- function(session = NULL,
                                 call = rlang::caller_env(n = 2L)) {
    if (is.null(session)) {
        session <- fundsr_default_session()
    } else if (!inherits(session, "fundsr_session")) {
        stop_bad_arg("session", "must be a fundsr_session object.", call = call)
    }
    session
}

#' Clear fundsr session state
#'
#' Convenience helper that clears mutable internal fundsr state for a session:
#' storage, fund-index map, import-function registry, Inkscape export queue,
#' and the XLM bookkeeping vector.
#'
#' @param session Optional `fundsr_session` object. Defaults to the package
#'   default session when `NULL`.
#'
#' @return Invisibly returns `NULL`. Called for side effects.
#' @family config functions
#' @export
#'
#' @examples
#' reset_state()
reset_state <- function(session = NULL) {
    session <- fundsr_get_session(session)

    clear_storage(clear_map = TRUE, session = session)
    clear_data_loaders(session = session)
    clear_inkscape_queue(session = session)

    st <- session$state
    if (!is.environment(st)) {
        return(invisible(NULL))
    }

    st$done_xlm_sets <- character()
    invisible(NULL)
}

fundsr_require_state <- function(storage = FALSE,
                                 session = NULL,
                                 call = rlang::caller_env(n = 2L)) {
    check_logical(storage)
    if (!is.environment(call)) {
        stop_bad_arg(
            "call",
            "must be an environment (a caller frame).",
            call = rlang::caller_env(n = 1L)
        )
    }
    session <- fundsr_get_session(session = session, call = call)

    if (!is.environment(session$state)) {
        fundsr_abort(
            msg   = "fundsr session is not initialized.",
            class = "fundsr_bad_state",
            call  = call
        )
    }

    if (storage && !is.environment(session$storage)) {
        fundsr_abort(
            msg   = "fundsr session storage is not initialized.",
            class = "fundsr_bad_state",
            call  = call
        )
    }

    session
}
