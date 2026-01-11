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
    .fundsr$done_xlm_sets <- character()
    invisible(NULL)
}
