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

fundsr_verbosity <- function() {
    v <- fundsr_get_option("verbosity")
    v <- suppressWarnings(as.integer(v))
    if (length(v) != 1L || is.na(v) || v < 0L) 1L else v
}

fundsr_abort <- function(msg,
                         class,
                         call = rlang::caller_env(n = 2),
                         ...) {
    msg <- collapse_msg(msg)

    cls <- as.character(class)
    cls <- cls[!is.na(cls) & nzchar(cls)]
    if (!length(cls)) cls <- "fundsr_error"
    cls <- unique(c(cls, "fundsr_error"))

    if (!is.environment(call)) {
        call0 <- rlang::caller_env()
        rlang::abort(
            message = "`call` must be an environment (a caller frame).",
            class   = c("fundsr_internal_error", "fundsr_error"),
            call    = call0,
            .frame  = call0
        )
    }

    rlang::abort(
        message = msg,
        class   = cls,
        call    = call,
        .frame  = call,
        ...
    )
}

fundsr_warn <- function(msg,
                        class = NULL,
                        ...) {
    msg <- collapse_msg(msg)

    cls <- as.character(class)
    cls <- cls[!is.na(cls) & nzchar(cls)]
    if (!length(cls)) cls <- "fundsr_warning"
    cls <- unique(c(cls, "fundsr_warning"))

    rlang::warn(
        message = msg,
        class   = cls,
        ...
    )
}

fundsr_msg <- function(msg,
                       ...,
                       level = 1L,
                       class = NULL) {
    level <- suppressWarnings(as.integer(level))
    if (length(level) != 1L || is.na(level) || level < 0L) level <- 1L
    if (level != 0L && fundsr_verbosity() < level) {
        return(invisible(NULL))
    }

    msg <- collapse_msg(msg)

    cls <- as.character(class)
    cls <- cls[!is.na(cls) & nzchar(cls)]
    if (!length(cls)) cls <- "fundsr_message"
    cls <- unique(c(cls, "fundsr_message"))

    rlang::inform(
        message = msg,
        class = cls,
        ...
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
