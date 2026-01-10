"%||%" <- function(a, b) {
    if (!is.null(a)) a else b
}

#' Generate candidate date format strings for `as.Date()`
#'
#' Builds a set of possible format strings suitable for `as.Date()` for a
#' given day/month/year order. This is intended for fast "detect once per sheet,
#' then parse all" workflows.
#'
#' The returned formats cover common separators (`"/"`, `"-"`, `"."`, `" "`),
#' unseparated dates with numeric months, and an optional comma before a final
#' 4- or 2-digit year element (`%y`, `%Y`) in space-separated dates.
#'
#' Month handling depends on `order`:
#' - If `order` uses `"m"`, month tokens include numeric months (`%m`) and
#'   abbreviated month names (`%b`).
#' - If `order` uses `"M"`, month tokens include full month names (`%B`).
#'
#' Month name formats (`%b`, `%B`) are locale-dependent.
#'
#' @param order A single string specifying the component order as a permutation
#'   of `"d"`, `"y"`, and exactly one of `"m"` or `"M"`
#'   (e.g. `"dmy"`, `"ymd"`, `"dMy"`, `"Mdy"`).
#'
#' @return A character vector of unique date format strings.
#'
#' @keywords internal
make_date_fmts <- function(order) {
    stopifnot(is.character(order), length(order) == 1L, nzchar(order))

    chars <- strsplit(order, "", fixed = TRUE)[[1]]

    allowed <- c("d", "y", "m", "M")
    if (!all(chars %in% allowed)) {
        stop("`order` must use only 'd', 'y', and one of 'm' or 'M' (e.g. 'dmy', 'dMy').",
             call. = FALSE)
    }
    if (length(chars) != 3L || length(unique(chars)) != 3L) {
        stop("`order` must be length 3 with no repeats.", call. = FALSE)
    }
    if (!all(c("d", "y") %in% chars)) {
        stop("`order` must include 'd' and 'y'.", call. = FALSE)
    }
    if (!xor("m" %in% chars, "M" %in% chars)) {
        stop("`order` must include exactly one of 'm' or 'M'.", call. = FALSE)
    }

    seps <- c("/", "-", ".", " ")
    yrs  <- c("%Y", "%y")

    # month tokens depend on whether order uses 'm' or 'M'
    mons <- if ("M" %in% chars) c("%B") else c("%m", "%b")

    tokens_for <- function(y_tok, m_tok) {
        vapply(chars, function(ch) {
            switch(ch,
                d = "%d",
                y = y_tok,
                m = m_tok,
                M = m_tok
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

fundsr_verbosity <- function() {
    v <- fundsr_get_option("verbosity")
    v <- suppressWarnings(as.integer(v))
    if (length(v) != 1L || is.na(v) || v < 0L) 1L else v
}

fundsr_msg <- function(..., level = 1L) {
    level <- suppressWarnings(as.integer(level))
    if (length(level) != 1L || is.na(level) || level < 0L) level <- 1L
    if (level == 0L || fundsr_verbosity() >= level) message(...)
    invisible(NULL)
}
