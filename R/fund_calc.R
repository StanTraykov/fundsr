longer <- function(df, funds, sfx, values_to, names_to = "fund") {
    df %>%
        select(any_of(c("date", paste0(funds, sfx)))) %>%
        tidyr::pivot_longer(
            cols = -date,
            names_to = names_to,
            values_to = values_to,
            names_pattern = paste0("(.+)", sfx),
            values_drop_na = TRUE
        )
}

#' Compute rolling annualized tracking differences (CAGR or log-return)
#'
#' For each fund–index pair in `fund_index_map`, computes a rolling,
#' annualized tracking-difference series over a backward window of
#' `n_days`. The result is added as new columns named `<fund>_rd`.
#'
#' @param df Data frame containing the date column, fund columns, and
#'   benchmark/index columns referenced in `fund_index_map`.
#' @param n_days Rolling lookback window in calendar days.
#' @param fund_index_map Named character vector mapping fund column names
#'   to their corresponding benchmark/index base column names.
#' @param date_col Name of the date column in `df`. Defaults to `"date"`.
#' @param use_log Logical; if `TRUE`, computes **log-return tracking
#'   differences**. If `FALSE`, computes **CAGR tracking differences**.
#'   Defaults to `TRUE`.
#' @param index_level Character; which index level to use. One of
#'   `"net"` or `"gross"`. If `"gross"`, `gross_suffix` is appended to the
#'   mapped index base name before lookup in `df`. Defaults to `"net"`.
#' @param annual_days Number of days used for annualization. Defaults to
#'   `365`.
#' @param silent_skip Logical; whether to suppress messages when skipping
#'   fund–index pairs due to missing columns or self-tracking. Defaults to
#'   `FALSE`.
#' @param gross_suffix Character; suffix appended to the mapped index base
#'   name when `index_level = "gross"`. Defaults to `"-GR"`.
#'
#' @return A data frame like `df` with one additional column per processed
#'   fund, named `<fund>_rd`, containing the rolling tracking differences.
#'
#' @details
#' For each fund–index pair, the function locates an anchor observation at
#' least `n_days` before each date (using the last available non-missing
#' fund+index point at or before `date - n_days`) and computes an
#' annualized tracking difference using the elapsed calendar days
#' \eqn{t - t_0}.
#'
#' * **log-return difference**
#'   \deqn{ \left[\ln\left(\frac{f_t}{f_{t_0}}\right) -
#'          \ln\left(\frac{i_t}{i_{t_0}}\right)\right] \times
#'          \frac{annual\_days}{t - t_0} }
#'
#' * **CAGR difference**
#'   \deqn{ \left(\frac{f_t}{f_{t_0}}\right)^{annual\_days/(t - t_0)} -
#'          \left(\frac{i_t}{i_{t_0}}\right)^{annual\_days/(t - t_0)} }
#'
#' where \eqn{f} and \eqn{i} are fund and index values, respectively.
#'
#' Pairs are skipped (optionally with a message) if the fund column is not
#' present in `df`, the corresponding index column is not present (after
#' applying `index_level`/`gross_suffix`), or if the mapping implies
#' self-tracking (fund column equals index column).
#'
#' The tracking difference is `NA` whenever the anchor point is missing,
#' the required values are invalid (e.g., non-positive in log mode), or no
#' past observation lies far enough back to satisfy the lookback window.
#'
#' @export
roll_diffs <- function(df,
                       n_days,
                       fund_index_map,
                       date_col = "date",
                       use_log = TRUE,
                       index_level = c("net", "gross"),
                       annual_days = 365,
                       silent_skip = FALSE,
                       gross_suffix = "-GR") {
    index_level <- match.arg(index_level)
    df <- df %>%
        mutate(.date_num = as.numeric(.data[[date_col]]))
    for (fund in names(fund_index_map)) {
        index <- fund_index_map[[fund]]
        if (index_level == "gross")
            index <- paste0(index, gross_suffix)
        roll_type <- if (use_log) "log-ret" else "CAGR"
        if (!(fund %in% names(df))) {
            if (!isTRUE(silent_skip)) {
                message(glue("Skipping {fund}: not in df"))
            }
            next
        }
        if (!(index %in% names(df))) {
            if (!isTRUE(silent_skip)) {
                message(glue("Skipping {fund}: tracked index {index} not in df"))
            }
            next
        }
        if (fund == index) {
            if (!isTRUE(silent_skip)) {
                message(glue("Skipping {index}: self-tracking"))
            }
            next
        }
        message(glue("Roll {roll_type} for {fund} tracking {index}"))
        roll_diff_col = paste0(fund, "_rd")
        no_na <- df %>%
            select(".date_num", !!sym(fund), !!sym(index)) %>%
            filter(!is.na(!!sym(fund)), !is.na(!!sym(index)))
        roll_diff <- function(fnd, idx, date, n_days) {
            if (is.na(fnd) || is.na(idx)) {
                return(NA_real_)
            }
            ndate <- as.numeric(date)
            pday <- findInterval(ndate - n_days, no_na$.date_num)
            if (pday == 0) {
                return(NA_real_)
            }
            delta_d <- ndate - no_na$.date_num[pday]
            if (delta_d <= 0) return(NA_real_)

            f_start <- no_na[[fund]][pday]
            i_start <- no_na[[index]][pday]

            if (use_log) {
                if (f_start <= 0 || i_start <= 0 || fnd <= 0 || idx <= 0) return(NA_real_)
                # Annualized log tracking difference
                (log(fnd / f_start) - log(idx / i_start)) * (annual_days / delta_d)
            } else {
                ratio_f <- fnd / f_start
                ratio_i <- idx / i_start
                if (!is.finite(ratio_f) || !is.finite(ratio_i) || ratio_f <= 0 || ratio_i <= 0) return(NA_real_)
                # CAGR difference
                (ratio_f^(annual_days / delta_d)) - (ratio_i^(annual_days / delta_d))
            }
        }
        df <- df %>%
            rowwise() %>%
            mutate(
                !!roll_diff_col := roll_diff(!!sym(fund),
                                             !!sym(index),
                                             !!sym(date_col),
                                             n_days)
            ) %>%
            ungroup()
    }
    df %>% select(-.data$.date_num)
}
