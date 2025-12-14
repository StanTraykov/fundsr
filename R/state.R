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
#' clear_session()
clear_session <- function() {
    clear_fund_storage(clear_fund_index = TRUE)
    clear_import_funs()
    clear_ink_queue()
    .fundsr$done_xlms <- character()
    invisible(NULL)
}


