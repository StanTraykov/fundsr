.norm_msg <- function(msg) {
    nms <- names(msg)
    msg <- as.character(msg)
    if (!length(msg)) return("<no message>")

    if (!is.null(nms)) {
        length(nms) <- length(msg)
        nms[is.na(nms)] <- ""
        names(msg) <- nms
    }

    msg[is.na(msg)] <- "<NA>"
    msg[!nzchar(trimws(msg))] <- "<blank>"
    msg
}

.norm_class <- function(class, append) {
    norm_chr <- function(x) {
        x <- as.character(x)
        x[!is.na(x) & nzchar(x)]
    }
    class <- norm_chr(class)
    append <- norm_chr(append)
    if (!length(class)) append else unique(c(class, append))
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
    msg <- .norm_msg(msg)
    class <- .norm_class(class, "fundsr_error")

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
        class   = class,
        call    = call,
        .frame  = call,
        ...
    )
}

fundsr_warn <- function(msg,
                        class = NULL,
                        ...) {
    msg <- .norm_msg(msg)
    class <- .norm_class(class, "fundsr_warning")
    rlang::warn(
        message = msg,
        class   = class,
        ...
    )
}

fundsr_msg <- function(msg,
                       level = 1L,
                       class = NULL,
                       ...) {
    level <- suppressWarnings(as.integer(level))
    if (length(level) != 1L || is.na(level) || level < 0L) level <- 1L
    if (level != 0L && fundsr_verbosity() < level) {
        return(invisible(NULL))
    }
    msg <- .norm_msg(msg)
    class <- .norm_class(class, "fundsr_message")

    if (is.null(names(msg))) {
        names(msg) <- c("i", rep.int("*", length(msg) - 1L))
    }

    rlang::inform(
        message = msg,
        class   = class,
        ...
    )
}

stop_bad_arg <- function(arg, msg, call = rlang::caller_env(n = 2)) {
    arg <- as.character(arg)
    if (length(arg) != 1L || is.na(arg) || !nzchar(arg)) {
        call0 <- rlang::caller_env()
        fundsr_abort(
            msg     = "`arg` must be a single non-empty string.",
            class   = "fundsr_internal_error",
            call    = call0
        )
    }
    msg <- .norm_msg(msg)
    msg[[1]] <- paste(sQuote(arg), msg[[1]])

    fundsr_abort(
        msg     = msg,
        class   = "fundsr_bad_arg",
        call    = call,
        arg     = arg
    )
}
