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
    x_col <- rlang::as_string(ensym(x_col))
    y_col <- rlang::as_string(ensym(y_col))
    out_col <- rlang::as_string(ensym(out_col))
    date_col <- rlang::as_string(ensym(date_col))
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
    x_dates <- as_date(x[[date_col]])
    y_dates <- as_date(y[[date_col]])

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
