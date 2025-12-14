"%||%" <- function(a, b) {
    if (!is.null(a)) a else b
}

#' Generate candidate date format strings for `as.Date()`
#'
#' Builds a set of possible format strings suitable for `as.Date()` for a
#' given day/month/year order. This is intended for fast "detect once per sheet,
#' then parse all" workflows.
#'
#' The returned formats cover common separators (`"/"`, `"-"`, `"."`, `" "`)
#' and both 4-digit (`%Y`) and 2-digit (`%y`) years. Month tokens include
#' numeric months (`%m`) and abbreviated month names (`%b`). Full month names
#' (`%B`) are excluded.
#'
#' @param order A single string specifying the component order as a permutation
#'   of `"d"`, `"m"`, `"y"` (e.g. `"dmy"`, `"mdy"`, `"ymd"`).
#'
#' @return A character vector of unique date format strings.
#'
#' @keywords internal
make_date_fmts <- function(order) {
    stopifnot(is.character(order), length(order) == 1L, nzchar(order))
    chars <- strsplit(order, "")[[1]]

    if (!all(chars %in% c("d", "m", "y"))) {
        stop("`order` must use only 'd', 'm', 'y' (e.g. 'dmy', 'ymd').", call. = FALSE)
    }
    if (length(chars) != 3L || length(unique(chars)) != 3L) {
        stop("`order` must be a permutation of 'd', 'm', 'y' (length 3, no repeats).", call. = FALSE)
    }

    seps <- c("/", "-", ".", " ")
    yrs  <- c("%Y", "%y")
    mons <- c("%m", "%b")  # numeric + abbreviated (locale-dependent)

    tokens_for <- function(y_tok, m_tok) {
        vapply(chars, function(ch) {
            switch(ch,
                   d = "%d",
                   m = m_tok,
                   y = y_tok
            )
        }, character(1))
    }

    out <- character()

    for (y_tok in yrs) {
        for (m_tok in mons) {
            toks <- tokens_for(y_tok, m_tok)

            for (sep in seps) {
                out <- c(out, paste(toks, collapse = sep))

                # Optional comma before the year when year is last and separator is space
                if (chars[3] == "y" && sep == " ") {
                    out <- c(out, paste0(paste(toks[1:2], collapse = " "), ", ", toks[3]))
                }
            }

            # No separator (only sensible for numeric months)
            if (m_tok == "%m") {
                out <- c(out, paste(toks, collapse = ""))
            }
        }
    }

    unique(out)
}
