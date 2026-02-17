#' Read a time series file (CSV/TSV) with a date + one or more value columns
#'
#' Loads a delimited file from the directory specified by `fundsr.data_dir`,
#' parses the date column into a proper `Date`, and coerces all other columns
#' to numeric.
#'
#' The reader is chosen by file extension: `.csv` uses [readr::read_csv()] and
#' `.tsv`/`.tab`/`.txt` uses [readr::read_tsv()]. Gzipped variants such as
#' `.csv.gz` and `.tsv.gz` are also supported.
#'
#' @param file Filename to read (relative to `fundsr.data_dir` option).
#' @param date_col Name of the date column in the file.
#' @param time_unit Character scalar giving the unit of a *numeric* date column
#'   (Unix epoch). One of `"ms"` (default), `"s"`, `"us"`, `"ns"`.
#' @param orders Character vector of lubridate parsing orders for a *text* date
#'   column (passed to [lubridate::parse_date_time()]). If `NULL`, a default set
#'   of common dmy-order formats is used.
#' @param force_text_date Logical scalar. If `TRUE`, the date column is always
#'   parsed as text using `orders` (no Unix-epoch numeric interpretation is
#'   attempted).
#' @param line_filter Optional regular expression used to pre-filter the raw
#'   file by lines before parsing. If supplied, the file is read with
#'   [readr::read_lines()] and only lines matching `line_filter` are kept. The
#'   regex must match both data lines and the header line, e.g.
#'   `"^[0-9]|^Date,"`
#'
#' @return A tibble with parsed date column and numeric value columns.
#'
#' @details
#' The function assumes a date column exists (default: `date`). By default, if
#' the date column looks numeric (i.e., coercion to numeric yields at least one
#' non-`NA`), it is interpreted as a Unix timestamp (scaled by `time_unit`).
#' Otherwise it is parsed as text using `orders`. If `force_text_date = TRUE`,
#' it is always parsed as text using `orders`.
#'
#' All non-date columns are coerced with `as.numeric()` (non-parsable values
#' become `NA`).
#'
#' @family fund/index file readers
#' @export
read_timeseries <- function(
    file,
    date_col = "date",
    time_unit = c("ms", "s", "us", "ns"),
    orders = NULL,
    force_text_date = FALSE,
    line_filter = NULL
) {
    time_unit <- match.arg(time_unit)
    check_string(file)
    check_string(date_col)
    check_string(orders, allow_null = TRUE, min_n = 1)
    check_logical(force_text_date)
    check_string(line_filter, allow_null = TRUE)

    fund_data_dir <- fundsr_get_option("data_dir")
    path <- file.path(fund_data_dir, file)

    if (!file.exists(path)) {
        stop_bad_arg(
            "file",
            c(
                "must refer to an existing file; file does not exist.",
                sprintf("path = %s.", sQuote(path))
            )
        )
    }

    fundsr_msg(paste("Reading Text: ", sQuote(path)), level = 1L)

    if (!is.null(line_filter)) {
        data_lines <- grep(line_filter, readr::read_lines(path), value = TRUE)
        if (length(data_lines) == 0L) {
            stop_bad_arg(
                "line_filter",
                c(
                    "matched no lines.",
                    sprintf("path = %s.", sQuote(path))
                )
            )
        }
        read_obj <- I(data_lines)
    } else {
        read_obj <- path
    }

    ext <- tolower(tools::file_ext(path))
    if (identical(ext, "gz")) {
        ext <- tolower(tools::file_ext(sub("\\.gz$", "", path, ignore.case = TRUE)))
    }
    reader <- switch(
        ext,
        "csv" = readr::read_csv,
        "tsv" = readr::read_tsv,
        "tab" = readr::read_tsv,
        "txt" = readr::read_tsv,
        stop_bad_arg(
            "file",
            c("must have extension .csv or .tsv/.txt/.tab (optionally .gz).",
              sprintf("ext  = %s.", sQuote(ext)),
              sprintf("path = %s.", sQuote(path)))
        )
    )
    div <- switch(
        time_unit,
        "s"  = 1,
        "ms" = 1000,
        "us" = 1e6,
        "ns" = 1e9
    )
    if (is.null(orders)) {
        orders <- c(
            "d/m/Y", "d.m.Y", "d-m-Y",
            "d/m/y", "d.m.y", "d-m-y",
            "d/b/Y", "d.b.Y", "d-b-Y", "d b Y",
            "d/b/y", "d.b.y", "d-b-y", "d b y"
        )
    }

    df <- reader(read_obj, show_col_types = FALSE, name_repair = "unique_quiet")
    if (!(date_col %in% names(df))) {
        stop_bad_arg(
            "date_col",
            c(
                "must name a column present in the input.",
                sprintf("date_col = %s.", sQuote(date_col)),
                sprintf("path     = %s.", sQuote(path))
            )
        )
    }

    out <- mutate(
        df,
        "{date_col}" := {
            d <- .data[[date_col]]

            if (inherits(d, "Date")) {
                d
            } else if (inherits(d, "POSIXt")) {
                lubridate::as_date(d)
            } else if (isTRUE(force_text_date)) {
                parsed <- lubridate::parse_date_time(
                    as.character(d),
                    orders = orders,
                    tz = "UTC",
                    quiet = TRUE
                )
                lubridate::as_date(parsed)
            } else {
                d_num <- suppressWarnings(as.numeric(d))
                if (any(!is.na(d_num))) {
                    lubridate::as_date(lubridate::as_datetime(d_num / div, tz = "UTC"))
                } else {
                    parsed <- lubridate::parse_date_time(
                        as.character(d),
                        orders = orders,
                        tz = "UTC",
                        quiet = TRUE
                    )
                    lubridate::as_date(parsed)
                }
            }
        },
        across(!all_of(date_col), ~ suppressWarnings(as.numeric(.x)))
    )

    if (!identical(date_col, "date")) {
        # FIXME should probably overwrite / rename the original `date` column instead
        if ("date" %in% names(out)) {
            stop_bad_arg(
                "date_col",
                c(
                    "cannot be specified because renaming it to `date` would overwrite an",
                    "existing `date` column in the input file.",
                    sprintf("path = %s.", sQuote(path))
                )
            )
        }
        out <- rename(out, date = all_of(date_col))
    }

    # Dates must be unique
    dup_pos <- anyDuplicated(out$date)
    if (dup_pos > 0L) {
        dup_dates <- unique(out$date[duplicated(out$date)])
        ex <- format(utils::head(sort(dup_dates), 5L), "%Y-%m-%d")

        fundsr_abort(
            msg = c(
                "Parsed dates are not unique.",
                sprintf("n_unique = %d.", length(unique(out$date))),
                sprintf("n_rows   = %d.", nrow(out)),
                sprintf("examples = %s.", paste(ex, collapse = ", ")),
                sprintf("path     = %s.", sQuote(path))
            ),
            class = c("fundsr_duplicate_dates", "fundsr_bad_data", "fundsr_io_error")
        )
    }

    out
}

#' Read an MSCI two-column TSV file
#'
#' Extracts the data portion of an MSCI TSV file—skipping header noise—and reads
#' it as a two-column table containing a date and a numeric value.
#'
#' @param file Filename of the TSV to read (relative to `fundsr.data_dir` option).
#'
#' @return A tibble with a `Date` column and one numeric column.
#'
#' @details
#' The function filters lines beginning with a digit (date rows) or the literal
#' `"Date"`, then parses them using a fixed `%m/%d/%Y` date format and a numeric
#' second field.
#'
#' @family fund/index file readers
#' @export
read_msci_tsv <- function(file) {
    fund_data_dir <- fundsr_get_option("data_dir")
    path <- file.path(fund_data_dir, file)
    if (!file.exists(path)) {
        stop_bad_arg(
            "file",
            c(
                "must refer to an existing file; file does not exist.",
                sprintf("path = %s.", sQuote(path))
            )
        )
    }
    lines <- readr::read_lines(path)
    data_lines <- grep("^[0-9]|^Date\\b", lines, value = TRUE)
    if (!length(data_lines)) {
        fundsr_abort(
            msg = c(
                "MSCI TSV parse failed: no data lines found.",
                sprintf("path = %s.", sQuote(path))
            ),
            class = "fundsr_bad_data"
        )
    }
    df <- readr::read_tsv(I(data_lines), col_types = readr::cols(
        readr::col_date(format = "%m/%d/%Y"),
        readr::col_double()
    ))
    df
}
