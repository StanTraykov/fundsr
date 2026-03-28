# Pivot selected fund columns to long format.
#
# Selects a date column and the specified fund columns from `df`, pivots the
# fund columns to long format, and standardizes the date column name to `date`
# in the output.
#
# @param df A data frame containing a date column and fund columns.
# @param funds Character vector of fund column names to pivot.
# @param date_col Name of the date column in `df`.
# @param values_to Name of the value column in the output.
# @param names_to Name of the fund-name column in the output.
# @param drop_na Logical; whether to drop rows with missing values after pivoting.
#
# @return A tibble with columns `date`, `names_to`, and `values_to`.
#
# @keywords internal
longer <- function(df,
                   funds,
                   date_col = "date",
                   values_to = "value",
                   names_to = "fund",
                   drop_na = TRUE) {
    if (!is.data.frame(df)) stop_bad_arg("df", "must be a data frame.")
    check_string(date_col)
    check_string(values_to)
    check_string(names_to)
    check_logical(drop_na)
    check_string(funds, n = NULL)

    if (!(date_col %in% names(df))) {
        stop_bad_arg("date_col", sprintf("column %s not found in `df`.", sQuote(date_col)))
    }

    df %>%
        select(any_of(c(date_col, funds))) %>%
        tidyr::pivot_longer(
            cols = -all_of(date_col),
            names_to = names_to,
            values_to = values_to,
            values_drop_na = drop_na
        ) %>%
        rename(date = !!date_col)
}

#' Compute rolling annualized tracking difference statistics
#'
#' For each fund–index pair in `fund_index_map`, computes rolling, annualized
#' tracking differences over a backward-looking window of `n_days` calendar
#' days. Both log-return and CAGR forms are returned.
#'
#' @param df Data frame containing a date column, fund columns, and
#'   benchmark/index columns referenced in `fund_index_map`.
#' @param n_days Rolling lookback window in calendar days.
#' @param fund_index_map Named character vector mapping fund column names
#'   to their corresponding benchmark/index base column names.
#' @param date_col Name of the date column in `df`.
#'   Must be of class `Date` and sorted (strictly increasing).
#' @param index_level Which index level to use, one of `"net"` or `"gross"`.
#'   If `"gross"`, `gross_suffix` is appended to the mapped index base name
#'   before lookup in `df`.
#' @param annual_days Number of days used for annualization.
#' @param messages Character vector controlling emitted messages. Any of
#'   `"roll"` (per-pair progress) and `"skip"` (skip reasons). Use
#'   `messages = "roll"` to show only progress, `messages = "skip"` to show
#'   only skip reasons, or `messages = character()` or `NULL` to silence all
#'   messages.
#' @param gross_suffix Suffix appended to the mapped index base name when
#'   `index_level = "gross"`.
#'
#' @return A named list with two data frames, `cagr` and `log`. Each data frame
#'   contains `date_col` followed by one column per fund (named as in
#'   `fund_index_map`), holding the rolling annualized tracking differences.
#'
#' @details
#' For each date \eqn{t}, a target anchor threshold \eqn{t - n\_days} is formed.
#' The anchor date \eqn{t_0} is chosen as the **last available observation at or
#' before** \eqn{t - n\_days} among rows where **both** fund and index values are
#' present. Let \eqn{\Delta = t - t_0} in calendar days (\eqn{\Delta} can be
#' greater than `n_days` when data are missing around the threshold).
#'
#' The annualized tracking differences are:
#' \itemize{
#'   \item Log-return difference:
#'     \deqn{
#'       \left[\ln\left(\frac{f_t}{f_{t_0}}\right) - \ln\left(\frac{i_t}{i_{t_0}}\right)\right]
#'       \times \frac{annual\_days}{\Delta}
#'     }
#'   \item CAGR difference:
#'     \deqn{
#'       \left(\frac{f_t}{f_{t_0}}\right)^{annual\_days/\Delta}
#'       - \left(\frac{i_t}{i_{t_0}}\right)^{annual\_days/\Delta}
#'     }
#' }
#'
#' Values are `NA` when an anchor cannot be found, current-date inputs are missing,
#' or inputs are invalid for the chosen formula (e.g. any non-positive level for
#' log returns, or non-finite / non-positive ratios for CAGR).
#'
#' Funds are skipped (optionally with a message) when the fund column is missing,
#' the mapped index column is missing (after applying `index_level` /
#' `gross_suffix`), or when `fund == index` (self-tracking).
#'
#' Emitted messages will be visible at verbosity level >= 1 (option `fundsr.verbosity`).
#' Verbosity level >= 4 forces both message types regardless of the `messages` argument.
#'
#' @family rolling difference functions
#' @export
roll_diffs <- function(df,
                       n_days,
                       fund_index_map,
                       date_col = "date",
                       index_level = c("net", "gross"),
                       annual_days = 365,
                       messages = c("roll", "skip"),
                       gross_suffix = "-GR") {
    index_level <- match.arg(index_level)
    if (is.null(messages) || length(messages) == 0L) {
        messages <- character()
    } else {
        messages <- match.arg(messages, choices = c("roll", "skip"), several.ok = TRUE)
    }
    if (!is.data.frame(df)) {
        stop_bad_arg("df", "must be a data frame.")
    }
    check_numeric_scalar(n_days, whole_num = TRUE, gt = 0)
    check_string(date_col)
    check_numeric_scalar(annual_days, whole_num = TRUE, gt = 0)
    check_string(gross_suffix)
    check_mapping(fund_index_map, scalar_values = TRUE)

    if (!(date_col %in% names(df))) {
        stop_bad_arg("date_col", sprintf("column %s not found in `df`.", sQuote(date_col)))
    }

    d <- df[[date_col]]
    if (!inherits(d, "Date")) {
        stop_bad_arg("date_col", "must refer to a Date column in `df`.")
    }
    if (anyNA(d)) {
        stop_bad_arg("date_col", "must not contain NA.")
    }
    if (is.unsorted(d, strictly = TRUE)) {
        stop_bad_arg("date_col", "must be strictly increasing.")
    }

    verbosity_override <- fundsr_verbosity() >= 4
    msg_roll <- "roll" %in% messages || verbosity_override
    msg_skip <- "skip" %in% messages || verbosity_override

    date_num <- as.numeric(df[[date_col]])
    n <- nrow(df)
    fund_names <- names(fund_index_map)
    out_log  <- set_names(vector("list", length(fund_names)), fund_names)
    out_cagr <- set_names(vector("list", length(fund_names)), fund_names)

    for (fund in fund_names) {
        out_log[[fund]]  <- rep(NA_real_, n)
        out_cagr[[fund]] <- rep(NA_real_, n)
    }

    for (fund in fund_names) {
        index <- fund_index_map[[fund]]
        if (index_level == "gross") index <- paste0(index, gross_suffix)

        if (!(fund %in% names(df))) {
            if (msg_skip) fundsr_msg(sprintf("Skipping %s: not in df", fund))
            next
        }
        if (!(index %in% names(df))) {
            if (msg_skip) fundsr_msg(sprintf("Skipping %s: tracked index %s not in df",
                                             fund,
                                             index))
            next
        }
        if (fund == index) {
            if (msg_skip) fundsr_msg(sprintf("Skipping %s: self-tracking", fund))
            next
        }
        if (msg_roll) fundsr_msg(sprintf("Roll diffs %s -> %s", fund, index))


        fund_vals  <- df[[fund]]
        index_vals <- df[[index]]

        anchor_rows <- which(!is.na(fund_vals) & !is.na(index_vals))
        if (length(anchor_rows) == 0) next
        anchor_dates <- date_num[anchor_rows]

        anchor_pos <- findInterval(date_num - n_days, anchor_dates)
        has_anchor <- anchor_pos > 0

        f_start <- rep(NA_real_, n)
        i_start <- rep(NA_real_, n)
        anchor_date_for_row <- rep(NA_real_, n)

        if (any(has_anchor)) {
            pos <- anchor_pos[has_anchor]
            ar  <- anchor_rows[pos]
            f_start[has_anchor] <- fund_vals[ar]
            i_start[has_anchor] <- index_vals[ar]
            anchor_date_for_row[has_anchor] <- anchor_dates[pos]
        }

        has_current <- !is.na(fund_vals) & !is.na(index_vals)
        elapsed_days <- date_num - anchor_date_for_row
        eligible <- has_current & has_anchor & !is.na(elapsed_days) & (elapsed_days > 0)
        if (!any(eligible)) next

        annual_factor <- annual_days / elapsed_days

        valid_log <- eligible &
            (fund_vals  > 0) & (index_vals > 0) &
            (f_start    > 0) & (i_start    > 0)

        if (any(valid_log)) {
            out_log[[fund]][valid_log] <-
                (log(fund_vals[valid_log] / f_start[valid_log]) -
                 log(index_vals[valid_log] / i_start[valid_log])) *
                annual_factor[valid_log]
        }

        ratio_f <- fund_vals / f_start
        ratio_i <- index_vals / i_start

        valid_cagr <- eligible &
            is.finite(ratio_f) & is.finite(ratio_i) &
            (ratio_f > 0) & (ratio_i > 0)

        if (any(valid_cagr)) {
            pow <- annual_factor[valid_cagr]
            out_cagr[[fund]][valid_cagr] <-
                ratio_f[valid_cagr]^pow - ratio_i[valid_cagr]^pow
        }
    }

    df_log  <- data.frame(df[[date_col]], out_log,  check.names = FALSE)
    df_cagr <- data.frame(df[[date_col]], out_cagr, check.names = FALSE)
    names(df_log)[1]  <- date_col
    names(df_cagr)[1] <- date_col

    list(cagr = df_cagr, log = df_log)
}
