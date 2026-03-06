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

#' Import an S&P Dow Jones index sheet and register benchmark mappings
#'
#' Wrapper around `store_timeseries()` and `read_timeseries_excel()` for S&P Dow Jones index files.
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
spdj <- function(var_name, file, col_trans, benchmarks = NULL, session = NULL) {
    store_timeseries(
        var_name = var_name,
        expr = read_timeseries_excel(
            file = file,
            sheet = 1,
            date_col = "^Effective date",
            col_trans = col_trans,
            date_order = "ymd",
            comma_rep = ""
        ),
        fund_index_map = benchmarks,
        session = session
    )
}
