stop_bad_arg <- function(arg, msg) {
    stop(sprintf("`%s` %s", arg, msg), call. = FALSE)
}

check_logical <- function(x,
                          arg = deparse(substitute(x)),
                          allow_null = FALSE,
                          allow_na = FALSE) {
    if (!missing(allow_null)) {
        allow_null <- check_logical(allow_null)
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
                         min_n = NULL,
                         max_n = NULL,
                         min_chars = NULL,
                         max_chars = Inf,
                         n_chars = NULL,
                         pattern = NULL) {
    allow_null  <- check_logical(allow_null)
    allow_na    <- check_logical(allow_na)
    allow_empty <- check_logical(allow_empty)
    trim        <- check_logical(trim)
    if (missing(n) && (!is.null(min_n) || !is.null(max_n))) {
        n <- NULL
    }
    n     <- check_numeric_scalar(n,     allow_null = TRUE, ge = 0, as_integer = TRUE)
    min_n <- check_numeric_scalar(min_n, allow_null = TRUE, ge = 0, as_integer = TRUE)
    max_n <- check_numeric_scalar(max_n, allow_null = TRUE, ge = 0, as_integer = TRUE)
    n_chars <- check_numeric_scalar(n_chars, allow_null = TRUE, ge = 0, as_integer = TRUE)

    if (!is.null(n) && (!is.null(min_n) || !is.null(max_n))) {
        stop_bad_arg("n", "cannot be used together with min_n/max_n.")
    }
    if (!is.null(min_n) && !is.null(max_n) && min_n > max_n) {
        stop_bad_arg("min_n", "must be <= max_n.")
    }
    if (!is.null(n_chars) && (!is.null(min_chars) || !identical(max_chars, Inf))) {
        stop_bad_arg("n_chars", "cannot be used together with min_chars/max_chars.")
    }
    if (is.null(x)) {
        if (allow_null) return(NULL)
        stop_bad_arg(arg, "must not be NULL.")
    }

    if (!is.character(x)) {
        stop_bad_arg(arg, "must be a character vector.")
    }

    len <- length(x)
    if (!is.null(n) && len != n) {
        stop_bad_arg(arg, sprintf("must be a character vector of length %d.", n))
    }
    if (!is.null(min_n) && len < min_n) {
        stop_bad_arg(arg, sprintf("must have length >= %d.", min_n))
    }
    if (!is.null(max_n) && len > max_n) {
        stop_bad_arg(arg, sprintf("must have length <= %d.", max_n))
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

    if (is.null(n_chars)) {
        min_chars <- check_numeric_scalar(min_chars, allow_null = TRUE, ge = 0, as_integer = TRUE)
        min_chars <- min_chars %||% (if (allow_empty) 0L else 1L)

        max_chars <- check_numeric_scalar(
            max_chars,
            finite = FALSE,
            ge = 0,
            whole_num = TRUE,
            as_integer_or_inf = TRUE
        )
    } else {
        min_chars <- n_chars
        max_chars <- n_chars
    }

    nch <- nchar(x, type = "chars", allowNA = TRUE)
    if (min_chars > 0L && any(nch < min_chars, na.rm = TRUE)) {
        stop_bad_arg(arg, sprintf("must have at least %d character(s).", min_chars))
    }
    if (is.finite(max_chars) && any(nch > max_chars, na.rm = TRUE)) {
        stop_bad_arg(arg, sprintf("must have at most %d character(s).", max_chars))
    }

    if (!is.null(pattern)) {
        pattern <- check_string(
            pattern,
            arg = "pattern",
            allow_empty = FALSE,
            trim = FALSE,
            n = 1L,
            min_n = NULL,
            max_n = NULL,
            min_chars = 1L,
            max_chars = Inf,
            n_chars = NULL,
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
            arg = arg,
            n = NULL,
            allow_na = allow_na_values,
            allow_empty = allow_empty_values
        )
        return(x)
    }

    # list: values must be character vectors (optionally scalar)
    for (i in seq_along(x)) {
        nm <- nms[i]
        arg_i <- sprintf("%s[[%s]]", arg, shQuote(nm))

        xi <- x[[i]]
        if (is.null(xi)) stop_bad_arg(arg_i, "must not be NULL.")
        if (!is.character(xi)) stop_bad_arg(arg_i, "must be a character vector.")

        check_string(
            xi,
            arg = arg_i,
            n = if (scalar_values) 1L else NULL,
            allow_na = allow_na_values,
            allow_empty = allow_empty_values
        )
    }

    x
}
