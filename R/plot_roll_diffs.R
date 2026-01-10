#' Plot rolling return differences against benchmark
#'
#' Plots rolling annualized tracking differences (CAGR-style or log-return) for
#' selected funds against their benchmark series. The plot uses quantile-based
#' y-limits and formats the y-axis in basis points.
#'
#' @param data Input data frame containing a `date` column and one rolling-difference
#'   column per fund (specified by `funds`).
#' @param n_days Window length (in days) used to compute rolling differences (used
#'   for labelling).
#' @param funds Character vector of fund column names to include.
#' @param use_log Logical; if `TRUE`, labels the plot as log-return differences.
#'   If `FALSE`, labels the plot as CAGR differences.
#' @param gg_params Optional ggplot components to add to the plot.
#' @param title_add Optional title suffix. Can be a single string or a named
#'   character vector specifying the title in multiple languages (e.g. `c(en=..., bg=...)`).
#' @param date_brk Optional date-break specification for the x-axis (e.g. `"3 months"`).
#'   If `NULL`, it is chosen automatically based on the time span and available data.
#' @param qprob Two-element numeric vector giving lower and upper quantiles used
#'   to set the baseline y-axis limits. Defaults to `c(0.005, 0.995)`.
#' @param bmark_type Benchmark type used in the title: `"net"` or `"gross"`.
#'
#' @return A ggplot object.
#'
#' @details
#' The function reshapes `data` to long format and produces a scatter plot coloured
#' by fund. The y-axis limits are primarily determined from quantiles of the rolling
#' differences (as specified by `qprob`), always including 0.
#'
#' To avoid clipping recent extremes, the y-limits are expanded (if needed) to also
#' include the full range observed in the most recent 30 days of data, even when
#' those values fall outside the `qprob` quantiles.
#'
#' The x-axis breaks are chosen as follows when `date_brk` is `NULL`: for spans of
#' up to 3 years, breaks default to `"3 months"`. For longer spans, breaks are
#' anchored to calendar months (semiannual or quarterly depending on span) and
#' are included only when the data range extends beyond the midpoint to the
#' neighboring break.
#'
#' @family rolling difference functions
#' @export

plot_roll_diffs <- function(data,
                            n_days,
                            funds,
                            use_log = FALSE,
                            gg_params = NULL,
                            title_add = NULL,
                            date_brk = NULL,
                            qprob = c(0.005, 0.995),
                            bmark_type = c("net", "gross")) {
    bmark_type <- match.arg(bmark_type)
    bmark_type <- if (bmark_type == "net") gettext("net") else gettext("gross")
    title_add <- pick_user_trans(title_add)
    ttl <- if (is.null(title_add)) "" else paste(":", title_add)
    title_msg <- if (use_log) {
        gettext("{n_days}d rolling log-return differences vs {bmark_type} benchmark{ttl}")
    } else {
        gettext("{n_days}d rolling CAGR differences vs {bmark_type} benchmark{ttl}")
    }
    title <- glue(ttl = ttl, title_msg)
    fundsr_msg(paste("plot_roll_diffs:", title), level = 1L)

    vals <- select(data, all_of(funds))
    has_data <- rowSums(is.finite(data.matrix(vals))) > 0
    if (!any(has_data)) {
        # fallback: use full date span
        warning("plot_roll_diffs: no finite data for selected funds.",
                call. = FALSE)
        min_date <- min(data[["date"]], na.rm = TRUE)
        max_date <- max(data[["date"]], na.rm = TRUE)
    } else {
        min_date <- min(data[["date"]][has_data], na.rm = TRUE)
        max_date <- max(data[["date"]][has_data], na.rm = TRUE)
    }
    min_yr <- lubridate::year(min_date)
    max_yr <- lubridate::year(max_date)
    n_yrs <- max_yr - min_yr
    if (!is.null(date_brk)) {
        date_breaks <- date_brk
        date_break_dates <- NULL
    } else if (n_yrs <= 3) {
        date_breaks <- "3 months"
        date_break_dates <- NULL
    } else {
        yrs <- (min_yr - 1):(max_yr + 1)
        months <- if (n_yrs >= 8) c(1, 7) else c(1, 4, 7, 10)
        date_break_dates <- as.Date(sprintf(
            "%d-%02d-01",
            rep(yrs, each = length(months)),
            rep(months, times = length(yrs))
        ))
        date_break_dates <- keep_supported_breaks(date_break_dates, min_date, max_date)
        date_breaks <- NULL
    }

    cdata <- longer(data, funds, values_to = "roll_diff")
    qlims <- stats::quantile(cdata$roll_diff,
                             probs = qprob,
                             na.rm = TRUE)
    # force-include last 30 days (even if outside quantiles)
    cutoff <- max(cdata$date, na.rm = TRUE) - lubridate::days(30)
    recent_range <- cdata %>%
        filter(.data$date >= cutoff) %>%
        summarize(
            lo = suppressWarnings(min(.data$roll_diff, na.rm = TRUE)),
            hi = suppressWarnings(max(.data$roll_diff, na.rm = TRUE))
        )
    recent_lo <- recent_range$lo
    recent_hi <- recent_range$hi
    if (!is.finite(recent_lo) || !is.finite(recent_hi)) {
        recent_lo <- NA_real_
        recent_hi <- NA_real_
    }
    y_lims <- range(c(qlims, 0, recent_lo, recent_hi), na.rm = TRUE)

    p <- ggplot2::ggplot(cdata,
                         ggplot2::aes(x = .data$date, y = .data$roll_diff, color = .data$fund)) +
        ggplot2::geom_point(alpha = 0.5, size = 1.5) +
        ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(alpha = 1, size = 3.2))) +
        ggplot2::scale_x_date(
            breaks      = if (is.null(date_break_dates)) ggplot2::waiver() else date_break_dates,
            date_breaks = if (is.null(date_breaks)) ggplot2::waiver() else date_breaks,
            date_labels = "%Y-%m"
        ) +
        ggplot2::scale_y_continuous(
            labels = function(x) x * 10000
        ) +
        ggplot2::coord_cartesian(ylim = y_lims) +
        ggplot2::expand_limits(y = 0) +
        # scale_color_distinct() +
        ggplot2::theme_minimal() +
        ggplot2::theme(
            text = ggplot2::element_text(size = 12),
            panel.grid.minor.x = ggplot2::element_blank(),
            axis.text.x = ggplot2::element_text(angle = 30, hjust = 1)
        ) +
        ggplot2::geom_hline(yintercept = 0,
                            color = "black",
                            linewidth = 0.7,
                            linetype = "solid",
                            alpha = 0.5) +
        ggplot2::labs(
            title = title,
            color = gettext("fund"),
            x = NULL,
            y = if (use_log) {
                gettext("rolling annualized log-return difference (bps)")
            } else {
                gettext("rolling CAGR difference (bps)")
            }
        )
    add_gg_params(p, gg_params)
}
