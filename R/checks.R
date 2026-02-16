fundsr_abort <- function(msg,
                         class,
                         call = rlang::caller_env(),
                         parent = NULL,
                         arg = NULL) {
    intern_err_cls <- c("fundsr_internal_error", "fundsr_error")
    msg <- collapse_msg(msg)

    cls <- as.character(class)
    cls <- cls[!is.na(cls) & nzchar(cls)]
    if (!length(cls)) {
        cls <- "fundsr_error"
    }
    cls <- unique(c(cls, "fundsr_error"))

    call0 <- rlang::caller_env()
    if (!is.environment(call)) {
        rlang::abort(
            message = "`call` must be an environment (a caller frame).",
            class   = intern_err_cls,
            call    = call0,
            .frame = call0
        )
    }

    if (!is.null(arg)) {
        arg <- as.character(arg)
        if (length(arg) != 1L || is.na(arg) || !nzchar(arg)) {
            rlang::abort(
                message = "`arg` must be a single non-empty string when supplied.",
                class   = intern_err_cls,
                call    = call0,
                .frame = call0
            )
        }
    }

    if (!is.null(parent) && !inherits(parent, "condition")) {
        rlang::abort(
            message = "`parent` must inherit from 'condition'.",
            class   = intern_err_cls,
            call    = call0,
            .frame = call0
        )
    }

    rlang::abort(
        message = msg,
        class   = cls,
        call    = call,
        .frame  = call,
        parent  = parent,
        arg     = arg
    )
}

stop_bad_arg <- function(arg, msg, call = rlang::caller_env(n = 2)) {
    arg <- as.character(arg)
    if (length(arg) != 1L || is.na(arg) || !nzchar(arg)) {
        call0 <- rlang::caller_env()
        fundsr_abort(
            msg = "`arg` must be a single non-empty string.",
            class   = "fundsr_internal_error",
            call    = call0
        )
    }
    msg <- collapse_msg(msg)
    fundsr_abort(
        msg = sprintf("`%s` %s", arg, msg),
        class   = "fundsr_bad_arg",
        call    = call,
        arg     = arg
    )
}

collapse_msg <- function(msg) {
    msg <- as.character(msg)
    if (!length(msg)) {
        msg <- "<no message>"
    } else {
        msg[is.na(msg)] <- "<NA>"
        if (length(msg) != 1L) {
            msg <- paste(msg, collapse = "\n")
        }
    }
    msg
}

check_logical <- function(x,
                          arg = deparse(substitute(x)),
                          allow_null = FALSE,
                          allow_na = FALSE,
                          call = rlang::caller_env(n = 2)) {
    call0 <- rlang::caller_env()
    if (!missing(allow_null)) {
        allow_null <- check_logical(allow_null, call = call0)
    }
    if (!missing(allow_na)) {
        allow_na <- check_logical(allow_na, call = call0)
    }

    if (is.null(x)) {
        if (allow_null) return(NULL)
        stop_bad_arg(arg, "must not be NULL.", call = call)
    }
    if (!is.logical(x) || length(x) != 1L) {
        stop_bad_arg(arg, "must be a logical scalar.", call = call)
    }
    if (!allow_na && is.na(x)) {
        stop_bad_arg(arg, "must not be NA.", call = call)
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
                                 as_integer_or_inf = FALSE,
                                 call = rlang::caller_env(n = 2)) {
    call0 <- rlang::caller_env()
    allow_null <- check_logical(allow_null, call = call0)
    allow_na <- check_logical(allow_na, call = call0)
    finite <- check_logical(finite, call = call0)
    whole_num <- check_logical(whole_num, call = call0)
    as_integer <- check_logical(as_integer, call = call0)
    as_integer_or_inf <- check_logical(as_integer_or_inf, call = call0)
    if (as_integer && as_integer_or_inf) {
        stop_bad_arg(arg, "cannot set both as_integer and as_integer_or_inf.", call = call0)
    }
    if (as_integer_or_inf && finite) {
        stop_bad_arg("finite", "must be FALSE when as_integer_or_inf is TRUE.", call = call0)
    }

    if (is.null(x)) {
        if (allow_null) return(NULL)
        stop_bad_arg(arg, "must not be NULL.", call = call)
    }

    if (!is.numeric(x) || length(x) != 1L) {
        stop_bad_arg(arg, "must be a numeric scalar.", call = call)
    }

    if (is.na(x)) {
        if (allow_na) return(x)
        stop_bad_arg(arg, "must not be NA.", call = call)
    }

    if ((finite || as_integer) && !is.finite(x)) {
        stop_bad_arg(arg, "must be finite.", call = call)
    }

    if ((whole_num || as_integer || as_integer_or_inf) && is.finite(x) && x != trunc(x)) {
        stop_bad_arg(arg, "must be a whole number.", call = call)
    }

    le <- check_numeric_scalar(le, allow_null = TRUE, finite = finite, call = call0)
    lt <- check_numeric_scalar(lt, allow_null = TRUE, finite = finite, call = call0)
    ge <- check_numeric_scalar(ge, allow_null = TRUE, finite = finite, call = call0)
    gt <- check_numeric_scalar(gt, allow_null = TRUE, finite = finite, call = call0)

    if (!is.null(ge) && !is.null(le) && ge > le) {
        stop_bad_arg(arg, "has inconsistent bounds: ge > le.", call = call0)
    }
    if (!is.null(ge) && !is.null(lt) && ge >= lt) {
        stop_bad_arg(arg, "has inconsistent bounds: ge >= lt.", call = call0)
    }
    if (!is.null(gt) && !is.null(le) && gt >= le) {
        stop_bad_arg(arg, "has inconsistent bounds: gt >= le.", call = call0)
    }
    if (!is.null(gt) && !is.null(lt) && gt >= lt) {
        stop_bad_arg(arg, "has inconsistent bounds: gt >= lt.", call = call0)
    }

    if (!is.null(le) && !(x <= le)) {
        stop_bad_arg(arg, sprintf("must be <= %s.", format(le)), call = call)
    }
    if (!is.null(lt) && !(x <  lt)) {
        stop_bad_arg(arg, sprintf("must be < %s.",  format(lt)), call = call)
    }
    if (!is.null(ge) && !(x >= ge)) {
        stop_bad_arg(arg, sprintf("must be >= %s.", format(ge)), call = call)
    }
    if (!is.null(gt) && !(x >  gt)) {
        stop_bad_arg(arg, sprintf("must be > %s.",  format(gt)), call = call)
    }

    if (as_integer_or_inf && !is.finite(x)) {
        return(x)
    }

    if (as_integer || as_integer_or_inf) {
        lo <- -(.Machine$integer.max + 1)
        hi <- .Machine$integer.max
        if (x < lo || x > hi) {
            stop_bad_arg(arg, sprintf("must be in integer range [%d, %d].", lo, hi), call = call)
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
                         pattern = NULL,
                         call = rlang::caller_env(n = 2)) {
    call0 <- rlang::caller_env()
    allow_null  <- check_logical(allow_null, call = call0)
    allow_na    <- check_logical(allow_na, call = call0)
    allow_empty <- check_logical(allow_empty, call = call0)
    trim        <- check_logical(trim, call = call0)
    if (missing(n) && (!is.null(min_n) || !is.null(max_n))) {
        n <- NULL
    }
    n <- check_numeric_scalar(n, allow_null = TRUE, ge = 0, as_integer = TRUE, call = call0)
    min_n <- check_numeric_scalar(min_n, allow_null = TRUE, ge = 0, as_integer = TRUE, call = call0)
    max_n <- check_numeric_scalar(max_n, allow_null = TRUE, ge = 0, as_integer = TRUE, call = call0)
    n_chars <- check_numeric_scalar(n_chars,
                                    allow_null = TRUE,
                                    ge = 0,
                                    as_integer = TRUE,
                                    call = call0)
    if (!is.null(n) && (!is.null(min_n) || !is.null(max_n))) {
        stop_bad_arg("n", "cannot be used together with min_n/max_n.", call = call0)
    }
    if (!is.null(min_n) && !is.null(max_n) && min_n > max_n) {
        stop_bad_arg("min_n", "must be <= max_n.", call = call0)
    }
    if (!is.null(n_chars) && (!is.null(min_chars) || !identical(max_chars, Inf))) {
        stop_bad_arg("n_chars", "cannot be used together with min_chars/max_chars.", call = call0)
    }
    if (is.null(x)) {
        if (allow_null) return(NULL)
        stop_bad_arg(arg, "must not be NULL.", call = call)
    }

    if (!is.character(x)) {
        stop_bad_arg(arg, "must be a character vector.", call = call)
    }

    len <- length(x)
    if (!is.null(n) && len != n) {
        stop_bad_arg(arg, sprintf("must be a character vector of length %d.", n), call = call)
    }
    if (!is.null(min_n) && len < min_n) {
        stop_bad_arg(arg, sprintf("must have length >= %d.", min_n), call = call)
    }
    if (!is.null(max_n) && len > max_n) {
        stop_bad_arg(arg, sprintf("must have length <= %d.", max_n), call = call)
    }

    if (!allow_na && anyNA(x)) {
        stop_bad_arg(arg, "must not contain NA.", call = call)
    }

    if (trim) {
        x <- trimws(x)
    }

    if (!allow_empty) {
        bad_empty <- !is.na(x) & !nzchar(x)
        if (any(bad_empty)) {
            stop_bad_arg(arg, "must not be empty.", call = call)
        }
    }

    if (is.null(n_chars)) {
        min_chars <- check_numeric_scalar(min_chars,
                                          allow_null = TRUE,
                                          ge = 0, as_integer = TRUE,
                                          call = call0)
        min_chars <- min_chars %||% (if (allow_empty) 0L else 1L)
        max_chars <- check_numeric_scalar(
            max_chars,
            finite = FALSE,
            ge = 0,
            whole_num = TRUE,
            as_integer_or_inf = TRUE,
            call = call0
        )
    } else {
        min_chars <- n_chars
        max_chars <- n_chars
    }

    nch <- nchar(x, type = "chars", allowNA = TRUE)
    if (min_chars > 0L && any(nch < min_chars, na.rm = TRUE)) {
        stop_bad_arg(arg, sprintf("must have at least %d character(s).", min_chars), call = call)
    }
    if (is.finite(max_chars) && any(nch > max_chars, na.rm = TRUE)) {
        stop_bad_arg(arg, sprintf("must have at most %d character(s).", max_chars), call = call)
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
            pattern = NULL,
            call = call
        )

        ok <- grepl(pattern, x)
        if (allow_na) ok[is.na(x)] <- TRUE
        if (!all(ok)) {
            stop_bad_arg(arg, sprintf("must match pattern %s.", sQuote(pattern)), call = call)
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
                          scalar_values = FALSE,
                          call = rlang::caller_env(n = 2)) {
    call0 <- rlang::caller_env()
    type <- match.arg(type)
    name_case <- match.arg(name_case)
    allow_null <- check_logical(allow_null, call = call0)
    allow_empty <- check_logical(allow_empty, call = call0)
    unique_case_insensitive <- check_logical(unique_case_insensitive, call = call0)
    allow_na_values <- check_logical(allow_na_values, call = call0)
    allow_empty_values <- check_logical(allow_empty_values, call = call0)
    scalar_values <- check_logical(scalar_values, call = call0)

    if (is.null(x)) {
        if (allow_null) return(NULL)
        stop_bad_arg(arg, "must not be NULL.", call = call)
    }

    ok_type <- switch(
        type,
        character = is.character(x),
        list      = is.list(x),
        either    = is.character(x) || is.list(x)
    )
    if (!ok_type) {
        stop_bad_arg(
            arg,
            switch(
                type,
                character = "must be a named character vector (or empty).",
                list      = "must be a named list (or empty).",
                either    = "must be a named character vector or named list (or empty)."
            ),
            call = call
        )
    }

    if (!allow_empty && length(x) == 0L) {
        stop_bad_arg(arg, "must not be empty.", call = call)
    }

    if (length(x) == 0L) {
        return(x)
    }

    nms <- names(x)
    if (is.null(nms)) stop_bad_arg(arg, "must be named.", call = call)
    if (anyNA(nms) || any(!nzchar(nms))) {
        stop_bad_arg(arg, "must have non-NA, non-empty names.", call = call)
    }

    if (name_case == "upper") nms <- toupper(nms)
    if (name_case == "lower") nms <- tolower(nms)
    names(x) <- nms

    if (unique_case_insensitive || name_case != "asis") {
        if (anyDuplicated(tolower(nms))) {
            stop_bad_arg(arg, "must not contain duplicate names (case-insensitive).", call = call)
        }
    } else {
        if (anyDuplicated(nms)) {
            stop_bad_arg(arg, "must not contain duplicate names.", call = call)
        }
    }

    if (is.character(x)) {
        check_string(
            x,
            arg = arg,
            n = NULL,
            allow_na = allow_na_values,
            allow_empty = allow_empty_values,
            call = call
        )
        return(x)
    }

    # list: values must be character vectors (optionally scalar)
    for (i in seq_along(x)) {
        nm <- nms[i]
        arg_i <- sprintf("%s[[%s]]", arg, shQuote(nm))

        xi <- x[[i]]
        if (is.null(xi)) stop_bad_arg(arg_i, "must not be NULL.", call = call)
        if (!is.character(xi)) stop_bad_arg(arg_i, "must be a character vector.", call = call)

        check_string(
            xi,
            arg = arg_i,
            n = if (scalar_values) 1L else NULL,
            allow_na = allow_na_values,
            allow_empty = allow_empty_values,
            call = call
        )
    }

    x
}
