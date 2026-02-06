"%||%" <- function(a, b) {
    if (!is.null(a)) a else b
}

vec_key <- function(x, ignore_order = FALSE) {
    if (is.null(x)) return("<NULL>")
    if (!is.atomic(x) || is.object(x)) {
        stop("`x` must be an atomic vector.", call. = FALSE)
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

stop_bad_arg <- function(arg, msg) {
    stop(sprintf("`%s` %s", arg, msg), call. = FALSE)
}

check_logical <- function(x,
                          arg = deparse(substitute(x)),
                          allow_null = FALSE,
                          allow_na = FALSE) {
    if (!missing(allow_null)) {
        allow_na <- check_logical(allow_null)
    }
    if (!missing(allow_na)) {
        allow_na <- check_logical(allow_na)
    }

    if (is.null(x)) {
        if (allow_null) return(NULL)
        stop_bad_arg(arg, "must not be NULL.")
    }
    if (!is.logical(x) || length(x) != 1L) {
        stop_bad_arg(arg, "must be a logical scalar.")
    }
    if (!allow_na && is.na(x)) {
        stop_bad_arg(arg, "must not be NA.")
    }

    x
}

check_numeric_scalar <- function(x,
                                 arg = deparse(substitute(x)),
                                 allow_null = FALSE,
                                 allow_na = FALSE,
                                 finite = TRUE,
                                 le = NULL,
                                 lt = NULL,
                                 ge = NULL,
                                 gt = NULL,
                                 whole_num = FALSE,
                                 as_integer = FALSE,
                                 as_integer_or_inf = FALSE) {
    allow_null <- check_logical(allow_null)
    allow_na <- check_logical(allow_na)
    finite <- check_logical(finite)
    whole_num <- check_logical(whole_num)
    as_integer <- check_logical(as_integer)
    as_integer_or_inf <- check_logical(as_integer_or_inf)

    if (as_integer && as_integer_or_inf) {
        stop_bad_arg(arg, "cannot set both as_integer and as_integer_or_inf.")
    }
    if (as_integer_or_inf && finite) {
        stop_bad_arg("finite", "must be FALSE when as_integer_or_inf is TRUE.")
    }

    if (is.null(x)) {
        if (allow_null) return(NULL)
        stop_bad_arg(arg, "must not be NULL.")
    }

    if (!is.numeric(x) || length(x) != 1L) {
        stop_bad_arg(arg, "must be a numeric scalar.")
    }

    if (is.na(x)) {
        if (allow_na) return(x)
        stop_bad_arg(arg, "must not be NA.")
    }

    if ((finite || as_integer) && !is.finite(x)) {
        stop_bad_arg(arg, "must be finite.")
    }

    if ((whole_num || as_integer || as_integer_or_inf) && is.finite(x) && x != trunc(x)) {
        stop_bad_arg(arg, "must be a whole number.")
    }

    le <- check_numeric_scalar(le, allow_null = TRUE, finite = finite)
    lt <- check_numeric_scalar(lt, allow_null = TRUE, finite = finite)
    ge <- check_numeric_scalar(ge, allow_null = TRUE, finite = finite)
    gt <- check_numeric_scalar(gt, allow_null = TRUE, finite = finite)

    if (!is.null(ge) && !is.null(le) && ge > le) {
        stop_bad_arg(arg, "has inconsistent bounds: ge > le.")
    }
    if (!is.null(ge) && !is.null(lt) && ge >= lt) {
        stop_bad_arg(arg, "has inconsistent bounds: ge >= lt.")
    }
    if (!is.null(gt) && !is.null(le) && gt >= le) {
        stop_bad_arg(arg, "has inconsistent bounds: gt >= le.")
    }
    if (!is.null(gt) && !is.null(lt) && gt >= lt) {
        stop_bad_arg(arg, "has inconsistent bounds: gt >= lt.")
    }

    if (!is.null(le) && !(x <= le)) stop_bad_arg(arg, sprintf("must be <= %s.", format(le)))
    if (!is.null(lt) && !(x <  lt)) stop_bad_arg(arg, sprintf("must be < %s.",  format(lt)))
    if (!is.null(ge) && !(x >= ge)) stop_bad_arg(arg, sprintf("must be >= %s.", format(ge)))
    if (!is.null(gt) && !(x >  gt)) stop_bad_arg(arg, sprintf("must be > %s.",  format(gt)))

    if (as_integer_or_inf && !is.finite(x)) {
        return(x)
    }

    if (as_integer || as_integer_or_inf) {
        lo <- -(.Machine$integer.max + 1)
        hi <- .Machine$integer.max
        if (x < lo || x > hi) {
            stop_bad_arg(arg, sprintf("must be in integer range [%d, %d].", lo, hi))
        }
        return(as.integer(x))
    }

    x
}

check_string <- function(x,
                         arg = deparse(substitute(x)),
                         allow_null = FALSE,
                         allow_na = FALSE,
                         allow_empty = FALSE,
                         trim = FALSE,
                         n = 1L,
                         min_chars = NULL,
                         max_chars = Inf,
                         pattern = NULL) {
    allow_null  <- check_logical(allow_null)
    allow_na    <- check_logical(allow_na)
    allow_empty <- check_logical(allow_empty)
    trim        <- check_logical(trim)

    n <- check_numeric_scalar(n, allow_null = TRUE, ge = 0, as_integer = TRUE)

    min_chars <- check_numeric_scalar(min_chars, allow_null = TRUE, ge = 0, as_integer = TRUE)
    min_chars <- min_chars %||% (if (allow_empty) 0L else 1L)

    max_chars <- check_numeric_scalar(
        max_chars,
        finite = FALSE,
        ge = 0,
        whole_num = TRUE,
        as_integer_or_inf = TRUE
    )

    if (is.null(x)) {
        if (allow_null) return(NULL)
        stop_bad_arg(arg, "must not be NULL.")
    }

    if (!is.character(x)) {
        stop_bad_arg(arg, "must be a character vector.")
    }

    if (!is.null(n) && length(x) != n) {
        stop_bad_arg(arg, sprintf("must be a character vector of length %d.", n))
    }

    if (!allow_na && anyNA(x)) {
        stop_bad_arg(arg, "must not contain NA.")
    }

    if (trim) {
        x <- trimws(x)
    }

    if (!allow_empty) {
        bad_empty <- !is.na(x) & !nzchar(x)
        if (any(bad_empty)) {
            stop_bad_arg(arg, "must not be empty.")
        }
    }

    n_chars <- nchar(x, type = "chars", allowNA = TRUE)
    if (min_chars > 0L && any(n_chars < min_chars, na.rm = TRUE)) {
        stop_bad_arg(arg, sprintf("must have at least %d character(s).", min_chars))
    }
    if (is.finite(max_chars) && any(n_chars > max_chars, na.rm = TRUE)) {
        stop_bad_arg(arg, sprintf("must have at most %d character(s).", max_chars))
    }

    if (!is.null(pattern)) {
        pattern <- check_string(
            pattern,
            arg = "pattern",
            allow_empty = FALSE,
            trim = FALSE,
            n = 1L,
            min_chars = 1L,
            max_chars = Inf,
            pattern = NULL
        )

        ok <- grepl(pattern, x)
        if (allow_na) ok[is.na(x)] <- TRUE
        if (!all(ok)) {
            stop_bad_arg(arg, sprintf("must match pattern %s.", sQuote(pattern)))
        }
    }

    x
}

check_mapping <- function(x,
                          arg = deparse(substitute(x)),
                          allow_null = FALSE,
                          allow_empty = TRUE,
                          type = c("character", "list", "either"),
                          name_case = c("asis", "upper", "lower"),
                          unique_case_insensitive = FALSE,
                          allow_na_values = FALSE,
                          allow_empty_values = FALSE,
                          scalar_values = FALSE) {
    type <- match.arg(type)
    name_case <- match.arg(name_case)
    check_logical(allow_null)
    check_logical(allow_empty)
    check_logical(unique_case_insensitive)
    check_logical(allow_na_values)
    check_logical(allow_empty_values)
    check_logical(scalar_values)

    if (is.null(x)) {
        if (allow_null) return(NULL)
        stop_bad_arg(arg, "must not be NULL.")
    }

    ok_type <- switch(
        type,
        character = is.character(x),
        list      = is.list(x),
        either    = is.character(x) || is.list(x)
    )
    if (!ok_type) {
        stop_bad_arg(arg, switch(type,
                                 character = "must be a named character vector (or empty).",
                                 list      = "must be a named list (or empty).",
                                 either    = "must be a named character vector or named list (or empty)."))
    }

    if (!allow_empty && length(x) == 0L) {
        stop_bad_arg(arg, "must not be empty.")
    }

    if (length(x) == 0L) {
        return(x)
    }

    nms <- names(x)
    if (is.null(nms)) stop_bad_arg(arg, "must be named.")
    if (anyNA(nms) || any(!nzchar(nms))) {
        stop_bad_arg(arg, "must have non-NA, non-empty names.")
    }

    if (name_case == "upper") nms <- toupper(nms)
    if (name_case == "lower") nms <- tolower(nms)
    names(x) <- nms

    if (unique_case_insensitive || name_case != "asis") {
        if (anyDuplicated(tolower(nms))) {
            stop_bad_arg(arg, "must not contain duplicate names (case-insensitive).")
        }
    } else {
        if (anyDuplicated(nms)) {
            stop_bad_arg(arg, "must not contain duplicate names.")
        }
    }

    if (is.character(x)) {
        check_string(
            x,
            n = NULL,
            allow_na = allow_na_values,
            allow_empty = allow_empty_values
        )
        return(x)
    }

    # list: values must be character vectors (optionally scalar)
    for (i in seq_along(x)) {
        xi <- x[[i]]
        if (is.null(xi)) stop_bad_arg(arg, "must not contain NULL values.")
        if (!is.character(xi)) stop_bad_arg(arg, "must contain only character values.")
        check_string(
            xi,
            n = if (scalar_values) 1L else NULL,
            allow_na = allow_na_values,
            allow_empty = allow_empty_values
        )
    }

    x
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
    check_string(order)
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

stop_if_dup_nm <- function(nms, context) {
    if (!is.character(context) || length(context) != 1L || is.na(context) || !nzchar(context)) {
        context <- "<unknown>"
    }
    if (is.null(nms) || !length(nms)) {
        return(invisible(NULL))
    }
    nms <- as.character(nms)

    d <- anyDuplicated(nms)
    if (d == 0L) {
        return(invisible(NULL))
    }
    tab <- table(nms, useNA = "ifany")
    dup <- names(tab)[tab > 1L]
    msg <- paste0(dup, " (", as.integer(tab[dup]), "x)")
    stop(
        "Duplicate names in ", context, ": ",
        paste(msg, collapse = ", "),
        call. = FALSE
    )
}
