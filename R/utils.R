##### Generic #####

"%||%" <- function(a, b) {
    if (!is.null(a)) a else b
}

vec_key <- function(x, ignore_order = FALSE) {
    check_logical(ignore_order)
    if (is.null(x)) return("<NULL>")
    if (!is.atomic(x) || is.object(x)) {
        stop_bad_arg("x", "must be an atomic vector.")
    }
    x_type <- typeof(x)
    x_chr  <- as.character(x)  # NAs remain NA_character_
    if (ignore_order && length(x_chr) > 1L) {
        ord <- if (is.numeric(x) || is.logical(x)) {
            order(x, na.last = TRUE, method = "radix")
        } else {
            order(x_chr, na.last = TRUE, method = "radix")
        }
        x_chr <- x_chr[ord]
    }
    lens <- nchar(x_chr, type = "bytes")
    lens[is.na(x_chr)] <- -1L
    parts <- paste0(lens, ":", ifelse(is.na(x_chr), "", x_chr))
    paste0(x_type, "|", length(x_chr), "|", paste(parts, collapse = "|"))
}

##### Plot helpers #####
add_gg_params <- function(p, gg_params) {
    if (is.null(gg_params)) return(p)

    if (!is.list(gg_params)) gg_params <- list(gg_params)

    while (any(vapply(gg_params, rlang::is_bare_list, logical(1)))) {
        gg_params <- purrr::list_flatten(gg_params, is_node = rlang::is_bare_list)
    }

    tryCatch(
        purrr::reduce(gg_params, `+`, .init = p),
        error = function(e) {
            stop_bad_arg(
                "gg_params",
                c(
                    "must contain ggplot components (scales, themes, etc.).",
                    "Underlying error:",
                    conditionMessage(e)
                )
            )
        }
    )
}

keep_supported_breaks <- function(breaks, min_date, max_date) {
    breaks <- breaks[!is.na(breaks)]
    breaks <- sort(unique(breaks))
    if (length(breaks) <= 1L) return(breaks)

    b <- as.numeric(breaks)
    mids <- (b[-1] + b[-length(b)]) / 2

    left  <- c(-Inf, mids)
    right <- c(mids, Inf)

    mn <- as.numeric(min_date)
    mx <- as.numeric(max_date)

    breaks[mx >= left & mn <= right]
}

##### Date parsing #####

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
    check_string(order, n_chars = 3L, pattern = "^[dmyM]{3}$")
    chars <- strsplit(order, "", fixed = TRUE)[[1]]
    if (length(unique(chars)) != 3L) {
        stop_bad_arg("order", "must be length 3 with no repeats.")
    }
    if (!all(c("d", "y") %in% chars)) {
        stop_bad_arg("order", "must include 'd' and 'y'.")
    }
    if (!xor("m" %in% chars, "M" %in% chars)) {
        stop_bad_arg("order", "must include exactly one of 'm' or 'M'.")
    }

    seps <- c("/", "-", ".", " ")
    yrs  <- c("%Y", "%y")

    # month tokens depend on whether order uses 'm' or 'M'
    mons <- if ("M" %in% chars) c("%B") else c("%m", "%b")

    tokens_for <- function(y_tok, m_tok) {
        vapply(chars, function(ch) {
            switch(
                ch,
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
