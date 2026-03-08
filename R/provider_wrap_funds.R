#' Fund provider wrappers
#'
#' Vendor-specific import wrappers for NAV histories.
#'
#' Most fund importers are wrappers around [load_fund()] (which uses [store_timeseries()] and
#' [read_timeseries_excel()]). Some use [store_timeseries()] directly, coupled with a reader like
#' [read_timeseries()] or [read_timeseries_excel()]. Extra arguments (`...`) are passed to
#' [store_timeseries()]. For wrappers built on [load_fund()], `...` is first passed through
#' [load_fund()].
#'
#' @param ticker Fund identifier/ticker.
#' @param file Optional filename. The default is `toupper(ticker)` with an extension
#'   determined by the vendor format, such as `.xls[x]` or `.csv`.
#' @param benchmark Optional benchmark identifier (to be added to the fund-index map).
#' @param retrieve_benchmark Logical. Also import the vendor-supplied benchmark series. Only
#'   supported for NAV histories from Invesco, iShares, and Xtrackers ([inve()], [ishs()],
#'   [xtra()]).
#' @inheritDotParams store_timeseries -var_name -expr -fund_index_map
#'
#' @concept provider wrappers
#' @seealso [Index provider wrappers][index_provider_wrappers], [load_fund()], [store_timeseries()],
#'   [read_timeseries_excel()], [read_timeseries()]
#'
#' @name fund_provider_wrappers
NULL

#' @describeIn fund_provider_wrappers Import an iShares NAV history
#' @export
ishs <- function(ticker, file = NULL, benchmark = NULL, retrieve_benchmark = FALSE, ...) {
    load_fund(ticker = ticker,
              file = file,
              sheet = "Historical",
              date_col = "^As Of",
              benchmark = benchmark,
              benchmark_col = "^Benchmark Ret",
              retrieve_benchmark = retrieve_benchmark,
              ...)
}

#' @describeIn fund_provider_wrappers Import an SPDR NAV history
#' @export
spdr <- function(ticker, file = NULL, benchmark = NULL, ...) {
    load_fund(ticker = ticker,
              file = file,
              benchmark = benchmark,
              ...)
}

#' @describeIn fund_provider_wrappers Import an Xtrackers NAV history
#' @export
xtra <- function(ticker, file = NULL, benchmark = NULL, retrieve_benchmark = FALSE, ...) {
    load_fund(ticker = ticker,
              file = file,
              benchmark = benchmark,
              benchmark_col = "^Index Level",
              retrieve_benchmark = retrieve_benchmark,
              ...)
}

#' @describeIn fund_provider_wrappers Import an Amundi NAV history
#' @export
amun <- function(ticker, file = NULL, benchmark = NULL, ...) {
    load_fund(ticker = ticker,
              file = file,
              nav_col = "^Official NAV",
              benchmark = benchmark,
              ...)
}

#' @describeIn fund_provider_wrappers Import an Invesco NAV history
#' @export
inve <- function(ticker, file = NULL, benchmark = NULL, retrieve_benchmark = FALSE, ...) {
    load_fund(ticker = ticker,
              file = file,
              nav_col = "^NAV$", # need end marker ($) for Invesco do disambiguate
              benchmark = benchmark,
              benchmark_col = "^Index",
              retrieve_benchmark = retrieve_benchmark,
              ...)
}

#' @describeIn fund_provider_wrappers Import a Vanguard NAV history
#' @export
vang <- function(ticker, file = NULL, benchmark = NULL, ...) {
    load_fund(ticker = ticker,
              file = file,
              nav_col = "^NAV \\(USD\\)$",
              benchmark = benchmark,
              ...)
}

#' @describeIn fund_provider_wrappers Import a UBS NAV history
#' @export
ubs <- function(ticker, file = NULL, benchmark = NULL, ...) {
    load_fund(ticker = ticker,
              file = file,
              nav_col = "^Official NAV",
              benchmark = benchmark,
              ...)
}

#' @describeIn fund_provider_wrappers Import an HSBC NAV history
#' @export
hsbc <- function(ticker, file = NULL, benchmark = NULL, ...) {
    load_fund(ticker = ticker,
              file = file,
              benchmark = benchmark,
              date_order = "mdy",
              ...)
}

#' @describeIn fund_provider_wrappers Import an BNP Paribas NAV history
#' @export
bnpp <- function(ticker, file = NULL, benchmark = NULL, ...) {
    load_fund(ticker = ticker,
              file = file,
              benchmark = benchmark,
              nav_col = "^Value",
              ...)
}

#' @describeIn fund_provider_wrappers Import an Avantis NAV history
#' @param var_name Specify a custom variable name for the storage environment.
#' @export
avan <- function(ticker, file = NULL, benchmark = NULL, var_name = NULL, ...) {
    ticker_lower <- tolower(ticker)
    file <- file %||% paste0(toupper(ticker), ".csv")
    store_timeseries(
        var_name %||% ticker_lower,
        read_timeseries(
            file,
            date_col = "Date",
            orders = "ymd",
            line_filter = "^(Date,|[0-9]{4}/[0-9]{2}/[0-9]{2},)"
        ) %>%
            select("date", "NAV") %>%
            rename_with(~ ticker_lower, "NAV"),
        fund_index_map = if (is.null(benchmark)) NULL else set_names(benchmark, ticker_lower),
        ...
    )
}
