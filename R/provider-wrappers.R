#' Import an MSCI index sheet and register benchmark mappings
#'
#' Wrapper around `store_timeseries()` and `read_timeseries_excel()` for MSCI index files.
#'
#' @param var_name Storage key used in `.fundsr_storage`.
#' @param col_trans Named vector specifying column translations.
#' @param benchmarks Optional index mapping to record in
#'   the fund index map (used to map gross to net indices).
#' @param file Filename of the XLSX file to import.
#'
#' @return Invisibly returns `NULL`. Data are stored via `store_timeseries()`.
#'
#' @export
msci <- function(var_name, col_trans, benchmarks = NULL, file) {
    store_timeseries(
        var_name = var_name,
        expr = read_timeseries_excel(
            xl_file = file,
            data_sheet = 1,
            date_col = "^Date",
            col_trans = col_trans,
            date_order = "mdy",
            comma_rep = ""
        ),
        fund_index_map = benchmarks
    )
}

#' Import an iShares fund
#'
#' Wrapper around `load_fund()` for iShares files.
#'
#' @param ticker Fund ticker.
#' @param file Optional filename override.
#' @param benchmark Optional benchmark key.
#' @param retrieve_benchmark Logical; also import benchmark column.
#' @export
ishs <- function(ticker, file = NULL, benchmark = NULL, retrieve_benchmark = FALSE) {
    load_fund(ticker = ticker,
              file = file,
              data_sheet = "Historical",
              date_col = "^As Of",
              benchmark = benchmark,
              benchmark_col = "^Benchmark Ret",
              retrieve_benchmark = retrieve_benchmark)
}

#' Import an SPDR fund
#'
#' Wrapper around `load_fund()` for SPDR files.
#'
#' @param ticker Fund ticker.
#' @param file Optional filename override.
#' @param benchmark Optional benchmark key.
#' @export
spdr <- function(ticker, file = NULL, benchmark = NULL) {
    load_fund(ticker = ticker,
              file = file,
              benchmark = benchmark)
}

#' Import an Xtrackers fund
#'
#' Wrapper around `load_fund()` for Xtrackers files.
#'
#' @param ticker Fund ticker.
#' @param file Optional filename override.
#' @param benchmark Optional benchmark key.
#' @param retrieve_benchmark Logical; also import benchmark column.
#' @export
xtra <- function(ticker, file = NULL, benchmark = NULL, retrieve_benchmark = FALSE) {
    load_fund(ticker = ticker,
              file = file,
              benchmark = benchmark,
              benchmark_col = "^Index Level",
              retrieve_benchmark = retrieve_benchmark)
}

#' Import an Amundi fund
#'
#' Wrapper around `load_fund()` for Amundi files.
#'
#' @param ticker Fund ticker.
#' @param file Optional filename override.
#' @param benchmark Optional benchmark key.
#' @export
amun <- function(ticker, file = NULL, benchmark = NULL) {
    load_fund(ticker = ticker,
              file = file,
              nav_col = "^Official NAV",
              benchmark = benchmark)
}

#' Import an Invesco fund
#'
#' Wrapper around `load_fund()` for Invesco files.
#'
#' @param ticker Fund ticker.
#' @param file Optional filename override.
#' @param benchmark Optional benchmark key.
#' @param retrieve_benchmark Logical; also import benchmark column.
#' @export
inve <- function(ticker, file = NULL, benchmark = NULL, retrieve_benchmark = FALSE) {
    load_fund(ticker = ticker,
              file = file,
              nav_col = "^NAV$", # need end marker ($) for Invesco do disambiguate
              benchmark = benchmark,
              benchmark_col = "^Index",
              retrieve_benchmark = retrieve_benchmark)
}

#' Import a Vanguard fund
#'
#' Wrapper around `load_fund()` for Vanguard files.
#'
#' @param ticker Fund ticker.
#' @param file Optional filename override.
#' @param benchmark Optional benchmark key.
#' @export
vang <- function(ticker, file = NULL, benchmark = NULL) {
    load_fund(ticker = ticker,
              file = file,
              nav_col = "^NAV \\(USD\\)$",
              benchmark = benchmark)
}

#' Import a UBS fund
#'
#' Wrapper around `load_fund()` for UBS files.
#'
#' @param ticker Fund ticker.
#' @param file Optional filename override.
#' @param benchmark Optional benchmark key.
#' @export
ubs <- function(ticker, file = NULL, benchmark = NULL) {
    load_fund(ticker = ticker,
              file = file,
              nav_col = "^Official NAV",
              benchmark = benchmark)
}

#' Import an HSBC fund
#'
#' Wrapper around `load_fund()` for HSBC files.
#'
#' @param ticker Fund ticker.
#' @param file Optional filename override.
#' @param benchmark Optional benchmark key.
#' @export
hsbc <- function(ticker, file = NULL, benchmark = NULL) {
    load_fund(ticker = ticker,
              file = file,
              benchmark = benchmark,
              date_order = "mdy")
}
