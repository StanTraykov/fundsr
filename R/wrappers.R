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
#' @seealso
#' [store_timeseries()], [read_timeseries_excel()]
#' @family provider wrappers
#' @export
msci <- function(var_name, col_trans, benchmarks = NULL, file) {
    store_timeseries(
        var_name = var_name,
        expr = read_timeseries_excel(
            file = file,
            sheet = 1,
            date_col = "^Date",
            col_trans = col_trans,
            date_order = "mdy",
            comma_rep = ""
        ),
        fund_index_map = benchmarks
    )
}

#' Load a fund's NAV data and optionally register its benchmark mapping
#'
#' Imports a fund's NAV time series from an Excel file and stores it in the
#' storage environment via `store_timeseries()`. Optionally, a benchmark column
#' can also be imported, and a fund/index mapping is recorded in
#' `.fundsr$fund_index_map`.
#'
#' If `file` is `NULL`, the function searches `getOption("fundsr.data_dir")` for
#' exactly one of `paste0(toupper(ticker), ".xlsx")` or
#' `paste0(toupper(ticker), ".xls")`.
#'
#' @param ticker Fund ticker symbol. Used (in lower case) as the storage key and
#'   (in upper case) to derive the default filename.
#' @param file Optional filename. If `NULL` (the default), it is inferred from
#'   `ticker` as described above.
#' @param sheet Sheet index or name containing the NAV data. Defaults to `1`.
#' @param date_col Regular expression identifying the date column. Defaults to `"^Date"`.
#' @param nav_col Regular expression identifying the fund's NAV column. Defaults to `"^NAV"`.
#' @param benchmark Optional benchmark key that this fund should be associated
#'   with in the fund/index map. When `retrieve_benchmark = TRUE`, the same value
#'   is also used as the name under which the benchmark series is imported.
#' @param benchmark_col Regular expression identifying the benchmark column in
#'   the Excel sheet. Only used when `retrieve_benchmark = TRUE`.
#' @param retrieve_benchmark Logical; if `TRUE`, both `benchmark` and
#'   `benchmark_col` must be supplied and the benchmark column is imported
#'   alongside the fund.
#' @param date_order Date parsing order passed to the importer. Defaults to `"dmy"`.
#' @param var_name Specify a custom variable name for the storage environment.
#' @param data_sheet Deprecated; use `sheet`.
#'
#' @return Invisibly returns `NULL`. The imported data are stored in
#'   `.fundsr_storage` under `tolower(ticker)`. A fund/index mapping is recorded
#'   in `.fundsr$fund_index_map` when `benchmark` is supplied.
#'
#' @details
#' The function builds a column-translation mapping from the fund NAV column and,
#' if requested, a benchmark column. It then calls `read_timeseries_excel()` to read the
#' Excel file and `store_timeseries()` to cache the imported object under
#' `tolower(ticker)`. When `benchmark` is provided, a corresponding entry is
#' added to `.fundsr$fund_index_map` to link the fund to its benchmark key.
#'
#' @seealso
#' Provide wrappers: [amun()], [hsbc()], [inve()], [ishs()], [spdr()], [ubs()], [vang()], [xtra()]
#' @family fund/index workflow functions
#' @export
load_fund <- function(ticker,
                      file = NULL,
                      sheet = 1,
                      date_col = "^Date",
                      nav_col = "^NAV",
                      benchmark = NULL,
                      benchmark_col = NULL,
                      retrieve_benchmark = FALSE,
                      date_order = "dmy",
                      var_name = NULL,
                      data_sheet = NULL) {
    # Deprecated param handling
    if (!missing(data_sheet) && !is.null(data_sheet)) {
        lifecycle::deprecate_warn(
            when = "0.2.1",
            what = "load_fund(data_sheet)",
            with = "load_fund(sheet)"
        )
        if (!missing(sheet)) {
            stop("Use only one of `sheet` or deprecated `data_sheet`.",
                 call. = FALSE)
        }
        sheet <- data_sheet
    }
    # / Deprecated param handling

    ticker_lower <- tolower(ticker)

    # Use provided file or derive default filename based on ticker
    fund_data_dir <- getOption("fundsr.data_dir")
    if (is.null(file)) {
        candidates <- paste0(toupper(ticker), c(".xlsx", ".xls"))
        paths <- file.path(fund_data_dir, candidates)
        exists <- file.exists(paths)
        if (!any(exists)) {
            stop(glue("No .xls[x] file found for {ticker}."), call. = FALSE)
        }
        if (sum(exists) > 1L) {
            stop(glue("Multiple .xls[x] files found for {ticker}."), call. = FALSE)
        }
        file <- candidates[exists]
    }

    # Build column translations
    ct <- c(
        set_names(nav_col, ticker_lower)
    )
    # If a benchmark is provided, add it to col_trans
    if (retrieve_benchmark) {
        if (is.null(benchmark) || is.null(benchmark_col))
            stop("Need benchmark and benchmark_col to retrieve benchmark.")
        ct <- c(ct, set_names(benchmark_col, benchmark))
    }
    store_timeseries(
        var_name %||% ticker_lower,
        read_timeseries_excel(
            file = file,
            sheet = sheet,
            date_col = date_col,
            col_trans = ct,
            date_order = date_order
        ),
        fund_index_map = if (is.null(benchmark)) NULL else set_names(benchmark, ticker_lower)
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
#'
#' @seealso
#' [load_fund()]
#' @family provider wrappers
#' @export
ishs <- function(ticker, file = NULL, benchmark = NULL, retrieve_benchmark = FALSE) {
    load_fund(ticker = ticker,
              file = file,
              sheet = "Historical",
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
#'
#' @seealso
#' [load_fund()]
#' @family provider wrappers
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
#'
#' @seealso
#' [load_fund()]
#' @family provider wrappers
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
#'
#' @seealso
#' [load_fund()]
#' @family provider wrappers
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
#'
#' @seealso
#' [load_fund()]
#' @family provider wrappers
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
#'
#' @seealso
#' [load_fund()]
#' @family provider wrappers
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
#'
#' @seealso
#' [load_fund()]
#' @family provider wrappers
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
#'
#' @seealso
#' [load_fund()]
#' @family provider wrappers
#' @export
hsbc <- function(ticker, file = NULL, benchmark = NULL) {
    load_fund(ticker = ticker,
              file = file,
              benchmark = benchmark,
              date_order = "mdy")
}
