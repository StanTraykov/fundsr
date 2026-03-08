#' Index provider wrappers
#'
#' Vendor-specific import wrappers for index total return levels (net or gross).
#'
#' Wrappers around [store_timeseries()] and [read_timeseries_excel()] for index files.
#'
#' @param var_name Storage key used in `session$storage`. If `NULL`, `tolower(file)` is
#'   used.
#' @param col_trans Named vector specifying column translations.
#' @param benchmarks Optional benchmark mapping to record in the fund-index map
#'   (for example, to map gross to net indices).
#' @param file Filename of the Excel file to import.
#' @param index_id Index identifier
#' @inheritDotParams store_timeseries -var_name -expr -fund_index_map
#'
#' @return Invisibly returns `NULL`. Data are stored via [store_timeseries()].
#'
#' @seealso [Fund provider wrappers][fund_provider_wrappers], [store_timeseries()],
#'   [read_timeseries_excel()]
#' @concept provider wrappers
#' @name index_provider_wrappers
NULL

#' @describeIn index_provider_wrappers Import an MSCI index sheet and register benchmark mappings
#' @export
msci <- function(file, col_trans, benchmarks = NULL, var_name = NULL, ...) {
    store_timeseries(
        var_name = var_name %||% tolower(file),
        expr = read_timeseries_excel(
            file = file,
            sheet = 1,
            date_col = "^Date",
            col_trans = col_trans,
            date_order = "mdy",
            comma_rep = ""
        ),
        fund_index_map = benchmarks,
        ...
    )
}

#' @describeIn index_provider_wrappers Import an S&P Dow Jones index sheet and register benchmark
#'   mappings
#' @export
spdj <- function(file, col_trans, benchmarks = NULL, var_name = NULL, ...) {
    store_timeseries(
        var_name = var_name %||% tolower(file),
        expr = read_timeseries_excel(
            file = file,
            sheet = 1,
            date_col = "^Effective date",
            col_trans = col_trans,
            date_order = "ymd",
            comma_rep = ""
        ),
        fund_index_map = benchmarks,
        ...
    )
}

#' @describeIn index_provider_wrappers Import an MSCI two-column TSV file for a single index
#' @export
msci_tsv <- function(file, index_id, benchmarks = NULL, var_name = NULL, ...) {
    check_string(index_id)
    store_timeseries(
        var_name = var_name %||% tolower(file),
        expr = read_timeseries(
            file = file,
            date_col = "Date",
            ext_override = "tsv",
            line_filter = "^[0-9]|^Date\\t",
            orders = "mdy"
        ) %>% set_names(c("date", index_id)),
        fund_index_map = benchmarks,
        ...
    )

}
