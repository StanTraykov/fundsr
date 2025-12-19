.fundsr <- NULL
.fundsr_storage <- NULL

#' Clear fundsr session state
#'
#' Convenience helper that clears mutable internal fundsr state: the fund storage
#' (`.fundsr_storage`) and fund-index map (`.fundsr$fund_index`), the import-function
#' registry (`.fundsr$import_funs`), the Inkscape export queue (`.fundsr$ink_queue`),
#' and the XLM bookkeeping vector (`.fundsr$done_xlms`).
#'
#' @return Invisibly returns `NULL`. Called for side effects.
#' @export
#'
#' @examples
#' reset_state()
reset_state <- function() {
    clear_storage(clear_fund_index_map = TRUE)
    clear_data_loaders()
    clear_inkscape_queue()
    .fundsr$done_xlm_sets <- character()
    invisible(NULL)
}
