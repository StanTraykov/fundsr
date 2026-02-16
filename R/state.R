.fundsr <- NULL
.fundsr_storage <- NULL

#' Clear fundsr session state
#'
#' Convenience helper that clears mutable internal fundsr state: the fund storage
#' (`.fundsr_storage`) and fund-index map (`.fundsr$fund_index_map`), the import-function
#' registry (`.fundsr$data_loaders`), the Inkscape export queue (`.fundsr$inkscape_queue`),
#' and the XLM bookkeeping vector (`.fundsr$done_xlm_sets`).
#'
#' @return Invisibly returns `NULL`. Called for side effects.
#' @family config functions
#' @export
#'
#' @examples
#' reset_state()
reset_state <- function() {
    clear_storage(clear_map = TRUE)
    clear_data_loaders()
    clear_inkscape_queue()
    if (!is.environment(.fundsr)) {
        return(invisible(NULL))
    }
    .fundsr$done_xlm_sets <- character()
    invisible(NULL)
}

fundsr_require_state <- function(storage = FALSE, caller_n = 1L) {
    check_logical(storage)
    env <- rlang::caller_env(n = caller_n)

    if (!is.environment(.fundsr)) {
        fundsr_abort(
            msg   = "Fundsr state environment is not initialised.",
            class = "fundsr_bad_state",
            call  = env
        )
    }

    if (storage && !is.environment(.fundsr_storage)) {
        fundsr_abort(
            msg   = "Fundsr storage is not initialised.",
            class = "fundsr_bad_state",
            call  = env
        )
    }

    list(
        state = .fundsr,
        storage = if (storage) .fundsr_storage else NULL
    )
}
