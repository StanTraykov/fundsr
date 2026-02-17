#' Add to fund-index map
#'
#' Merges fund-index pairs into the session fund-index map (`session$state$fund_index_map`).
#' Existing entries with the same names are replaced.
#'
#' @param fund_index_map Named character vector of fund-index pairs to merge into
#'   `session$state$fund_index_map`. Names are fund identifiers; values are index
#'   identifiers.
#' @param session Optional `fundsr_session` object. Defaults to the package
#'   default session when `NULL`.
#'
#' @return Invisibly returns `NULL`. Called for side effects.
#' @family fund-index map functions
#' @export
add_fund_index_map <- function(fund_index_map, session = NULL) {
    fund_index_map <- check_mapping(
        fund_index_map,
        allow_null = TRUE,
        allow_empty = TRUE,
        type = "character",
        scalar_values = TRUE
    )
    if (is.null(fund_index_map) || length(fund_index_map) == 0L) {
        return(invisible(NULL))
    }
    st <- fundsr_require_state(session = session)$state

    cur <- st$fund_index_map
    cur <- tryCatch(
        check_mapping(
            cur,
            arg = "session$state$fund_index_map",
            allow_null = TRUE,
            allow_empty = TRUE,
            type = "character",
            allow_na_values = FALSE,
            allow_empty_values = FALSE
        ),
        fundsr_bad_arg = function(e) {
            fundsr_abort(
                msg    = "The internal fund index map has an invalid value.",
                class  = "fundsr_bad_state",
                parent = e
            )
        }
    )

    if (is.null(cur)) {
        cur <- character()
    }

    cur[names(fund_index_map)] <- fund_index_map
    st$fund_index_map <- cur

    invisible(NULL)
}

#' Get the internal fund index map
#'
#' Returns the package's fund index lookup table stored in
#' `session$state$fund_index_map`.
#'
#' @param session Optional `fundsr_session` object. Defaults to the package
#'   default session when `NULL`.
#'
#' @return A named character vector representing the internal fund index mapping.
#' @family fund-index map functions
#' @export
get_fund_index_map <- function(session = NULL) {
    st <- fundsr_require_state(session = session)$state
    st$fund_index_map
}

#' Clear fund-index map
#'
#' Clears the fund-index map stored in `session$state$fund_index_map`.
#'
#' @param session Optional `fundsr_session` object. Defaults to the package
#'   default session when `NULL`.
#'
#' @return Invisibly returns `NULL`. Called for side effects.
#' @family fund-index map functions
#' @export
clear_fund_index_map <- function(session = NULL) {
    session <- fundsr_get_session(session)
    st <- session$state

    if (!is.environment(st)) {
        return(invisible(NULL))
    }

    st$fund_index_map <- character()
    invisible(NULL)
}
