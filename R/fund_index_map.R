#' Add to fund-index map
#'
#' Merges fund-index pairs into the fund-index map (`.fundsr$fund_index_map`).
#' Existing entries with the same names are replaced.
#'
#' @param fund_index_map Named vector or list of fund-index pairs to merge into
#'   `.fundsr$fund_index_map`. Names are fund identifiers; values are index
#'   identifiers.
#'
#' @return Invisibly returns `NULL`. Called for side effects.
#' @family fund-index map mutators
#' @export
#'
#' @examples
#' add_fund_index_map(c(fund1 = "INDEX1", fund2 = "INDEX2", fund3 = "INDEX2"))
add_fund_index_map <- function(fund_index_map) {
    if (is.null(fund_index_map)) {
        return(invisible(NULL))
    }
    if (!exists(".fundsr", inherits = TRUE) || !is.environment(.fundsr)) {
        stop("Fundsr state environment is not initialised.", call. = FALSE)
    }

    if (!(is.atomic(fund_index_map) || is.list(fund_index_map))) {
        stop("`fund_index_map` must be a named vector or list.", call. = FALSE)
    }
    nms <- names(fund_index_map)
    if (is.null(nms) || any(is.na(nms)) || any(nms == "")) {
        stop("`fund_index_map` must have non-empty names.", call. = FALSE)
    }
    if (is.null(.fundsr$fund_index_map)) {
        .fundsr$fund_index_map <- character()
    }

    .fundsr$fund_index_map[nms] <- fund_index_map
    invisible(NULL)
}

#' Get the internal fund index map
#'
#' Returns the package's fund index lookup table stored in
#' `.fundsr$fund_index_map`.
#'
#' @return A character vector or named list representing the internal
#'   fund index mapping.
#'
#' @family fund-index map mutators
#' @export
get_fund_index_map <- function() {
    .fundsr$fund_index_map
}

#' Clear fund-index map
#'
#' Clears the fund-index map stored in `.fundsr$fund_index_map`.
#'
#' @return Invisibly returns `NULL`. Called for side effects.
#' @family fund-index map mutators
#' @export
#'
#' @examples
#' clear_fund_index_map()
clear_fund_index_map <- function() {
    if (!exists(".fundsr", inherits = TRUE) || !is.environment(.fundsr)) {
        stop("Fundsr state environment is not initialised.", call. = FALSE)
    }
    .fundsr$fund_index_map <- character()
    invisible(NULL)
}
