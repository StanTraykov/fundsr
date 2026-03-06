splice_series <- function(
        x,
        y,
        x_col,
        y_col,
        out_col,
        splice_date,
        date_col = "date"
) {
    x_col <- check_string(x_col)
    y_col <- check_string(y_col)
    out_col <- check_string(out_col)
    date_col <- check_string(date_col)
    splice_date <- lubridate::as_date(splice_date)

    if (is.na(splice_date)) {
        stop_bad_arg("splice_date", "must be coercible to Date.")
    }

    # Basic column checks
    miss_x <- setdiff(c(date_col, x_col), names(x))
    if (length(miss_x)) {
        stop_bad_arg(
            "x",
            paste0(
                "is missing required column(s): ",
                paste(miss_x, collapse = ", "),
                "."
            )
        )
    }

    miss_y <- setdiff(c(date_col, y_col), names(y))
    if (length(miss_y)) {
        stop_bad_arg(
            "y",
            paste0(
                "is missing required column(s): ",
                paste(miss_y, collapse = ", "),
                "."
            )
        )
    }

    # Date coercion + duplicate checks
    x_dates <- lubridate::as_date(x[[date_col]])
    y_dates <- lubridate::as_date(y[[date_col]])

    if (anyNA(x_dates)) {
        stop_bad_arg("x", paste0("has NA/non-date values in `", date_col, "`."))
    }
    if (anyNA(y_dates)) {
        stop_bad_arg("y", paste0("has NA/non-date values in `", date_col, "`."))
    }

    if (anyDuplicated(x_dates) > 0L) {
        fundsr_abort(
            msg = c(
                "Cannot splice series: duplicate dates found in first input.",
                i = paste0("date_col = ", sQuote(date_col), ".")
            ),
            class = "fundsr_bad_state"
        )
    }

    if (anyDuplicated(y_dates) > 0L) {
        fundsr_abort(
            msg = c(
                "Cannot splice series: duplicate dates found in second input.",
                i = paste0("date_col = ", sQuote(date_col), ".")
            ),
            class = "fundsr_bad_state"
        )
    }

    x2 <- x %>%
        mutate(`__date__` = x_dates) %>%
        transmute(
            date = .data$`__date__`,
            !!out_col := .data[[x_col]]
        ) %>%
        filter(.data$date < splice_date)

    y2 <- y %>%
        mutate(`__date__` = y_dates) %>%
        transmute(
            date = .data$`__date__`,
            !!out_col := .data[[y_col]]
        ) %>%
        filter(.data$date >= splice_date)

    out <- bind_rows(x2, y2) %>%
        arrange(.data$date)

    if (anyDuplicated(out$date) > 0L) {
        dup_dates <- unique(out$date[duplicated(out$date)])
        fundsr_abort(
            msg = c(
                "Spliced result contains duplicate dates.",
                i = paste0(
                    "Examples: ",
                    paste(format(utils::head(sort(dup_dates), 5L), "%Y-%m-%d"), collapse = ", "),
                    "."
                )
            ),
            class = "fundsr_bad_state"
        )
    }

    out
}

adjust_for_split <- function(data,
                             split_date,
                             split_ratio,
                             value_col,
                             date_col = "date") {
    call <- rlang::caller_env()

    if (!inherits(data, "data.frame")) {
        stop_bad_arg("data", "must be a data frame.", call = call)
    }

    value_col <- check_string(value_col, call = call)
    date_col <- check_string(date_col, call = call)
    split_ratio <- check_numeric_scalar(
        split_ratio,
        arg = "split_ratio",
        gt = 0,
        call = call
    )

    if (!date_col %in% names(data)) {
        stop_bad_arg(
            "date_col",
            c(
                "must name an existing column in `data`.",
                i = glue("Column {sQuote(date_col)} was not found.")
            ),
            call = call
        )
    }

    if (!value_col %in% names(data)) {
        stop_bad_arg(
            "value_col",
            c(
                "must name an existing column in `data`.",
                i = glue("Column {sQuote(value_col)} was not found.")
            ),
            call = call
        )
    }

    if (!is.numeric(data[[value_col]])) {
        stop_bad_arg(
            "value_col",
            c(
                "must refer to a numeric column.",
                i = glue("Column {sQuote(value_col)} has class: {paste(class(data[[value_col]]), collapse = ', ')}.")
            ),
            call = call
        )
    }

    split_date <- suppressWarnings(lubridate::as_date(split_date))
    if (length(split_date) != 1L || is.na(split_date)) {
        stop_bad_arg("split_date", "must be a single valid date.", call = call)
    }

    date_vec <- suppressWarnings(lubridate::as_date(data[[date_col]]))
    bad_dates <- !is.na(data[[date_col]]) & is.na(date_vec)

    if (any(bad_dates)) {
        stop_bad_arg(
            "data",
            c(
                "contains unparseable dates in the date column.",
                i = glue("Date column: {sQuote(date_col)}."),
                i = glue("Rows failing date parsing: {sum(bad_dates)}.")
            ),
            call = call
        )
    }

    out <- data
    out[[value_col]] <- as.numeric(out[[value_col]])

    idx <- !is.na(date_vec) & date_vec < split_date
    out[[value_col]][idx] <- out[[value_col]][idx] / split_ratio

    out
}
