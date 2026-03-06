#' Import an iShares fund
#'
#' Wrapper around `load_fund()` for iShares files.
#'
#' @param ticker Fund ticker.
#' @param file Optional filename override.
#' @param benchmark Optional benchmark key.
#' @param retrieve_benchmark Logical; also import benchmark column.
#' @param ... Additional arguments passed to [load_fund()] and [store_timeseries()], such as
#'   `postprocess`, `session`.
#'
#' @seealso
#' [load_fund()]
#' @family provider wrappers
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

#' Import an SPDR fund
#'
#' Wrapper around `load_fund()` for SPDR files.
#'
#' @param ticker Fund ticker.
#' @param file Optional filename override.
#' @param benchmark Optional benchmark key.
#' @param ... Additional arguments passed to [load_fund()] and [store_timeseries()], such as
#'   `postprocess`, `session`.
#'
#' @seealso
#' [load_fund()]
#' @family provider wrappers
#' @export
spdr <- function(ticker, file = NULL, benchmark = NULL, ...) {
    load_fund(ticker = ticker,
              file = file,
              benchmark = benchmark,
              ...)
}

#' Import an Xtrackers fund
#'
#' Wrapper around `load_fund()` for Xtrackers files.
#'
#' @param ticker Fund ticker.
#' @param file Optional filename override.
#' @param benchmark Optional benchmark key.
#' @param retrieve_benchmark Logical; also import benchmark column.
#' @param ... Additional arguments passed to [load_fund()] and [store_timeseries()], such as
#'   `postprocess`, `session`.
#'
#' @seealso
#' [load_fund()]
#' @family provider wrappers
#' @export
xtra <- function(ticker, file = NULL, benchmark = NULL, retrieve_benchmark = FALSE, ...) {
    load_fund(ticker = ticker,
              file = file,
              benchmark = benchmark,
              benchmark_col = "^Index Level",
              retrieve_benchmark = retrieve_benchmark,
              ...)
}

#' Import an Amundi fund
#'
#' Wrapper around `load_fund()` for Amundi files.
#'
#' @param ticker Fund ticker.
#' @param file Optional filename override.
#' @param benchmark Optional benchmark key.
#' @param ... Additional arguments passed to [load_fund()] and [store_timeseries()], such as
#'   `postprocess`, `session`.
#'
#' @seealso
#' [load_fund()]
#' @family provider wrappers
#' @export
amun <- function(ticker, file = NULL, benchmark = NULL, ...) {
    load_fund(ticker = ticker,
              file = file,
              nav_col = "^Official NAV",
              benchmark = benchmark,
              ...)
}

#' Import an Invesco fund
#'
#' Wrapper around `load_fund()` for Invesco files.
#'
#' @param ticker Fund ticker.
#' @param file Optional filename override.
#' @param benchmark Optional benchmark key.
#' @param retrieve_benchmark Logical; also import benchmark column.
#' @param ... Additional arguments passed to [load_fund()] and [store_timeseries()], such as
#'   `postprocess`, `session`.
#'
#' @seealso
#' [load_fund()]
#' @family provider wrappers
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

#' Import a Vanguard fund
#'
#' Wrapper around `load_fund()` for Vanguard files.
#'
#' @param ticker Fund ticker.
#' @param file Optional filename override.
#' @param benchmark Optional benchmark key.
#' @param ... Additional arguments passed to [load_fund()] and [store_timeseries()], such as
#'   `postprocess`, `session`.
#'
#' @seealso
#' [load_fund()]
#' @family provider wrappers
#' @export
vang <- function(ticker, file = NULL, benchmark = NULL, ...) {
    load_fund(ticker = ticker,
              file = file,
              nav_col = "^NAV \\(USD\\)$",
              benchmark = benchmark,
              ...)
}

#' Import a UBS fund
#'
#' Wrapper around `load_fund()` for UBS files.
#'
#' @param ticker Fund ticker.
#' @param file Optional filename override.
#' @param benchmark Optional benchmark key.
#' @param ... Additional arguments passed to [load_fund()] and [store_timeseries()], such as
#'   `postprocess`, `session`.
#'
#' @seealso
#' [load_fund()]
#' @family provider wrappers
#' @export
ubs <- function(ticker, file = NULL, benchmark = NULL, ...) {
    load_fund(ticker = ticker,
              file = file,
              nav_col = "^Official NAV",
              benchmark = benchmark,
              ...)
}

#' Import an HSBC fund
#'
#' Wrapper around `load_fund()` for HSBC files.
#'
#' @param ticker Fund ticker.
#' @param file Optional filename override.
#' @param benchmark Optional benchmark key.
#' @param ... Additional arguments passed to [load_fund()] and [store_timeseries()], such as
#'   `postprocess`, `session`.
#'
#' @seealso
#' [load_fund()]
#' @family provider wrappers
#' @export
hsbc <- function(ticker, file = NULL, benchmark = NULL, ...) {
    load_fund(ticker = ticker,
              file = file,
              benchmark = benchmark,
              date_order = "mdy",
              ...)
}

#' Import an BNP Paribas fund
#'
#' Wrapper around `load_fund()` for HSBC files.
#'
#' @param ticker Fund ticker.
#' @param file Optional filename override.
#' @param benchmark Optional benchmark key.
#' @param ... Additional arguments passed to [load_fund()] and [store_timeseries()], such as
#'   `postprocess`, `session`.
#'
#' @seealso
#' [load_fund()]
#' @family provider wrappers
#' @export
bnpp <- function(ticker, file = NULL, benchmark = NULL, ...) {
    load_fund(ticker = ticker,
              file = file,
              benchmark = benchmark,
              nav_col = "^Value",
              ...)
}

#' Import an Avantis fund
#'
#' Wrapper around `read_timeseries()` for Avantis files.
#'
#' @param ticker Fund ticker.
#' @param file Filename.
#' @param benchmark Optional benchmark key.
#' @param ... Additional arguments passed to [store_timeseries()], such as
#'   `postprocess`, `session`.
#'
#' @seealso
#' [read_timeseries()]
#' @family provider wrappers
#' @export
avan <- function(ticker, file, benchmark = NULL, ...) {
    ticker_lower <- tolower(ticker)
    store_timeseries(
        ticker_lower,
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
