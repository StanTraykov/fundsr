read_xlsx_hdr <- function(file_path,
                          sheet_name = 1,
                          header_rows,
                          skip_rows = NULL,
                          col_types = NULL) {
    data <- readxl::read_excel(file_path,
                               sheet = sheet_name,
                               col_types = col_types,
                               .name_repair = "unique_quiet")
    data <- tibble::as_tibble(data)
    headers <- data[header_rows, ] %>% as.data.frame()
    # Join the headers if more than one row is specified
    concatenated_headers <- apply(headers, 2, function(col) {
        paste(stats::na.omit(col), collapse = ".")
    })
    colnames(data) <- concatenated_headers
    # Remove header rows and anything above them from the data
    data <- data[(max(header_rows) + 1):nrow(data), ]
    if (!is.null(skip_rows)) {
        data <- data[-seq_len(skip_rows), ]
    }
    colnames(data) <- ifelse(colnames(data) == "",
                             paste0("col", seq_along(data)),
                             colnames(data))
    data
}

#' Read and combine XLM data from multiple Xetra XLSX files
#'
#' Scans a directory for Xetra ETF XLSX files, extracts the XLM column
#' and associated metadata, and returns a combined data frame with
#' standardized column names and parsed dates.
#'
#' @param directory Directory containing the XLSX files to read.
#' @param header_rows Integer vector indicating which rows contain
#'   header information for `read_xlsx_hdr()`.
#' @param col_types Optional column type specification passed to
#'   `read_xlsx_hdr()`.
#'
#' @return A data frame containing combined XLM records from all files
#'   in the directory.
#'
#' @details
#' Each XLSX file is processed with `read_xlsx_hdr()`, filtered to the
#' relevant XLM, ticker, and product-name fields (stored in
#' columns `xlm`, `ticker`, `name`, and `date`). The Month–Year embedded
#' in the XLM column name is parsed and used as the observation date.
#'
#' @family XLM functions
#' @export
read_xlm_directory <- function(directory,
                               header_rows = c(4, 5),
                               col_types = NULL) {
    # Get all .xlsx files in the directory
    file_list <- list.files(path = directory, pattern = "\\.xlsx$",
                            full.names = TRUE)
    # Read and process each file
    combined_data <- purrr::map_dfr(file_list, function(file) {
        # Read the file using read_xlsx_hdr
        data <- read_xlsx_hdr(file_path = file,
                              sheet_name = "Exchange Traded Funds",
                              header_rows = header_rows,
                              col_types = col_types) %>%
            select(matches("XLM|Ticker|Product Name"))
        # Find the column matching "Xetra Liquidity Measure (XLM)*.<Month Year>"
        xlm_col <- grep("^Xetra Liquidity Measure \\(XLM\\)\\*\\..*",
                        colnames(data), value = TRUE)
        if (length(xlm_col) != 1) {
            found <- if (!length(xlm_col)) "none" else paste(xlm_col, collapse = ", ")
            fundsr_abort(
                msg = c(
                    sprintf("Could not uniquely identify the XLM column in file %s.",
                            sQuote(file)),
                    i = sprintf("Expected exactly 1 matching column; found %d: %s.",
                                length(xlm_col), found)
                ),
                class = "fundsr_bad_data"
            )
        }
        # Extract the Month-Year string from the column name
        month_year <- sub("^Xetra Liquidity Measure \\(XLM\\)\\*\\.",
                          "",
                          xlm_col)
        fundsr_msg(glue("XLM read: {month_year}"), level = 2L)
        data <- data %>%
            rename_with(~ "xlm", matches("^Xetra Liquidity Measure")) %>%
            rename_with(~ "ticker", matches("Xetra Ticker")) %>%
            rename_with(~ "name", matches("Product Name")) %>%
            mutate(date = lubridate::as_date(
                lubridate::parse_date_time(month_year, orders = "my")
            )) %>%
            mutate(xlm = as.numeric(.data$xlm)) %>%
            mutate(ticker = tolower(.data$ticker))
        data
    })

    # FIX: XLM showing wrong ticker for this ETF
    combined_data$ticker[combined_data$name == "Xtrackers EURO STOXX 50 UCITS ETF 1C"] <- "xesc"
    combined_data
}

#' Plot Xetra Liquidity Measure (XLM) time series
#'
#' Generates a time-series plot of XLM values for one or more ETFs, with
#' optional back-translation of the displayed ticker labels via the
#' `fundsr.xetra_map` option.
#'
#' @param xlm_data A data frame containing XLM observations, including
#'   columns `date`, `xlm`, `ticker`, and `name`.
#' @param tickers Character vector of tickers to plot, or a pattern if
#'   `rgx = TRUE`.
#' @param rgx Logical; if `TRUE`, `tickers` is treated as a regular
#'   expression and matched against the `name` column; otherwise it is
#'   matched against the `ticker` column.
#' @param gg_params Optional list of additional ggplot components (such as
#'   themes, scales, or labels) to be added to the base plot.
#' @param back_trans Logical; if `TRUE`, replaces values in the `ticker`
#'   column *after filtering* using the inverse of the mapping stored in the
#'   `fundsr.xetra_map` option. This affects only what is displayed (legend
#'   labels and color grouping), not which rows are selected.
#'
#' @return A ggplot object showing the XLM time-series curves.
#'
#' @details
#' If `rgx = FALSE`, the data are filtered by matching `tickers` against the
#' `ticker` column. If `rgx = TRUE`, `tickers` is treated as a regular
#' expression and matched against the `name` column.
#'
#' When `back_trans = TRUE`, the function reads option `fundsr.xetra_map`,
#' expected to be a named character vector mapping internal identifiers to
#' Xetra tickers, for example:
#' `c(fwra = "fwia", acwi = "lyy0")`.
#' The mapping is inverted and applied to the already-filtered data so that
#' occurrences of Xetra tickers in the plotted data are replaced by their
#' internal identifiers in the plot's legend and grouping.
#'
#' @family XLM functions
#' @export
#'
#' @examples
#' \dontrun{
#' options(fundsr.xetra_map = c(fwra = "fwia", acwi = "lyy0", iwda = "eunl", cw8 = "amew"))
#'
#' # Filter by tickers as they appear in xlm_data$ticker, but display back-translated labels
#' plot_xlms(xlm_data, tickers = c("fwia", "eunl"), back_trans = TRUE)
#'
#' # Regex match against the `name` column
#' # (back-translation, if enabled, still happens after filtering)
#' plot_xlms(xlm_data, tickers = "MSCI World", rgx = TRUE, back_trans = TRUE)
#' }
plot_xlms <- function(xlm_data, tickers, rgx = FALSE, gg_params = NULL, back_trans = FALSE) {
    data <- if (rgx) {
        xlm_data %>% filter(stringr::str_detect(.data$name, tickers))
    } else {
        xlm_data %>% filter(.data$ticker %in% tickers)
    }
    if (back_trans) {
        xetra_map <- fundsr_get_option("xetra_map", NULL)
        if (!is.null(xetra_map)) {
            inv <- set_names(names(xetra_map), unname(xetra_map)) # xetra ==> internal
            data <- data %>%
                mutate(
                    ticker = if_else(
                        .data$ticker %in% names(inv),
                        unname(inv[.data$ticker]),
                        .data$ticker
                    )
                )
        }
    }

    fundsr_msg(paste("plot_xlms:", paste(tickers, collapse = ", ")), level = 1L)

    p <- ggplot2::ggplot(
        data,
        ggplot2::aes(x = .data$date, y = .data$xlm, color = .data$ticker)
    ) +
        ggplot2::geom_line(linewidth = 1) +
        ggplot2::geom_point(size = 2) +
        ggplot2::scale_x_date(
            date_breaks = "1 month",
            date_labels = "%Y-%m",
            expand = ggplot2::expansion(mult = 0, add = 5)
        ) +
        ggplot2::expand_limits(y = 0) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
            text = ggplot2::element_text(size = 12),
            legend.position = "bottom",
            panel.grid.minor = ggplot2::element_blank(),
            axis.text.x = ggplot2::element_text(angle = 30, hjust = 1)
        ) +
        ggplot2::labs(
            title = gettext("Xetra Liquidity Measure (\u20AC100K round-trip spread costs)"),
            y = gettext("monthly avg XLM (bps)"),
            x = NULL,
            color = gettext("fund")
        )

    add_gg_params(p, gg_params)
}
