#' Import an MSCI index sheet and register benchmark mappings
#'
#' Wrapper around `store_timeseries()` and `read_timeseries_excel()` for MSCI index files.
#'
#' @param var_name Storage key used in `session$storage`.
#' @param col_trans Named vector specifying column translations.
#' @param benchmarks Optional index mapping to record in
#'   the fund index map (used to map gross to net indices).
#' @param file Filename of the XLSX file to import.
#' @param session Optional `fundsr_session` object. Defaults to the package
#'   default session when `NULL`.
#'
#' @return Invisibly returns `NULL`. Data are stored via `store_timeseries()`.
#' @seealso
#' [store_timeseries()], [read_timeseries_excel()]
#' @family provider wrappers
#' @export
msci <- function(var_name, file, col_trans, benchmarks = NULL, session = NULL) {
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
        fund_index_map = benchmarks,
        session = session
    )
}

#' Load a fund's NAV data and optionally register its benchmark mapping
#'
#' Imports a fund's NAV time series from an Excel file and stores it in the
#' storage environment via `store_timeseries()`. Optionally, a benchmark column
#' can also be imported, and a fund/index mapping is recorded in
#' `session$state$fund_index_map`.
#'
#' If `file` is `NULL`, the function searches `fundsr.data_dir` for
#' exactly one of `paste0(toupper(ticker), ".xlsx")` or
#' `paste0(toupper(ticker), ".xls")`.
#'
#' @param ticker Fund ticker symbol. Used (in lower case) as the storage key and
#'   (in upper case) to derive the default filename.
#' @param file Optional filename. If `NULL` (the default), it is inferred from
#'   `ticker` as described above.
#' @param sheet Sheet index or name containing the NAV data.
#' @param date_col Regular expression identifying the date column.
#' @param nav_col Regular expression identifying the fund's NAV column.
#' @param benchmark Optional benchmark key that this fund should be associated
#'   with in the fund/index map. When `retrieve_benchmark = TRUE`, the same value
#'   is also used as the name under which the benchmark series is imported.
#' @param benchmark_col Regular expression identifying the benchmark column in
#'   the Excel sheet. Only used when `retrieve_benchmark = TRUE`.
#' @param retrieve_benchmark Logical; if `TRUE`, both `benchmark` and
#'   `benchmark_col` must be supplied and the benchmark column is imported
#'   alongside the fund.
#' @param date_order Date parsing order passed to the importer.
#' @param var_name Specify a custom variable name for the storage environment.
#' @param data_sheet Deprecated; use `sheet`.
#' @param ... Additional arguments passed to [store_timeseries()], such as
#'   `overwrite` or `postprocess`.
#'
#' @return Invisibly returns `NULL`. The imported data are stored in
#'   `session$storage` under `tolower(ticker)`. A fund/index mapping is recorded
#'   in `session$state$fund_index_map` when `benchmark` is supplied.
#'
#' @details
#' The function builds a column-translation mapping from the fund NAV column and,
#' if requested, a benchmark column. It then calls `read_timeseries_excel()` to read the
#' Excel file and `store_timeseries()` to cache the imported object under
#' `var_name`, if supplied, otherwise `tolower(ticker)`. When `benchmark` is provided, a
#' corresponding entry is added to `session$state$fund_index_map` to link the fund to its benchmark key.
#'
#' @seealso
#' Provider wrappers: [amun()], [hsbc()], [inve()], [ishs()], [spdr()], [ubs()], [vang()], [xtra()]
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
                      data_sheet = NULL,
                      ...) {
    check_string(ticker)
    check_string(file, allow_null = TRUE)
    check_string(date_col)
    check_string(nav_col)
    check_string(benchmark, allow_null = TRUE)
    check_string(benchmark_col, allow_null = TRUE)
    check_logical(retrieve_benchmark)
    check_string(date_order)
    check_string(var_name, allow_null = TRUE)
    check_string(data_sheet, allow_null = TRUE)
    # Deprecated param handling
    if (!missing(data_sheet) && !is.null(data_sheet)) {
        lifecycle::deprecate_warn(
            when = "0.2.1",
            what = "load_fund(data_sheet)",
            with = "load_fund(sheet)"
        )
        if (!missing(sheet)) {
            stop_bad_arg(
                "data_sheet",
                "(deprecated) cannot be used together with `sheet` (use only the latter)."
            )
        }
        sheet <- data_sheet
    }
    # / Deprecated param handling

    ticker_lower <- tolower(ticker)

    # Use provided file or derive default filename based on ticker
    fund_data_dir <- fundsr_get_option("data_dir")
    if (is.null(file)) {
        candidates <- paste0(toupper(ticker), c(".xlsx", ".xls"))
        paths <- file.path(fund_data_dir, candidates)
        exists <- file.exists(paths)
        if (!any(exists)) {
            fundsr_abort(
                msg = c(
                    glue("No .xls[x] file found for {ticker}."),
                    glue("Looked for: {paste(sQuote(paths), collapse = ', ')}.")
                ),
                class = "fundsr_io_error"
            )
        }
        if (sum(exists) > 1L) {
            fundsr_abort(
                msg = c(
                    glue("Multiple .xls[x] files found for {ticker}."),
                    glue("Matches: {paste(sQuote(paths[exists]), collapse = ', ')}.")
                ),
                class = "fundsr_io_error"
            )
        }
        file <- candidates[exists]
    }

    # Build column translations
    ct <- c(
        set_names(nav_col, ticker_lower)
    )
    # If a benchmark is provided, add it to col_trans
    if (retrieve_benchmark) {
        if (is.null(benchmark) || is.null(benchmark_col)) {
            stop_bad_arg(
                "retrieve_benchmark",
                "requires both `benchmark` and `benchmark_col`."
            )
        }
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
        fund_index_map = if (is.null(benchmark)) NULL else set_names(benchmark, ticker_lower),
        ...
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
#' @param ... Additional arguments passed to [load_fund()] and [store_timeseries()], such as
#'   `postprocess`.
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
#'   `postprocess`.
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
#'   `postprocess`.
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
#'   `postprocess`.
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
#'   `postprocess`.
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
#'   `postprocess`.
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
#'   `postprocess`.
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
#'   `postprocess`.
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
#'   `postprocess`.
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
#'   `postprocess`.
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
