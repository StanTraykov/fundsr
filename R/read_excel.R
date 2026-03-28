read_excel_or_xml <- function(file_path, sheet = NULL) {
    check_string(file_path)
    sheet_idx <- NULL
    sheet_name <- NULL
    if (!is.null(sheet)) {
        if (is.numeric(sheet)) {
            sheet_idx <- check_numeric_scalar(
                sheet,
                arg = "sheet [if numeric]",
                as_integer = TRUE,
                ge = 1L
            )
        } else if (is.character(sheet)) {
            sheet_name <- check_string(sheet, arg = "sheet [if string]")
        } else {
            stop_bad_arg(
                "sheet",
                "must be NULL, a sheet name (string), or a 1-based sheet index (numeric)."
            )
        }
    }

    # First, try reading as a standard Excel file with readxl
    fundsr_msg(paste("Reading Excel:", sQuote(file_path)), level = 1L)
    df <- tryCatch({
        # Try readxl in a tryCatch
        readxl::read_excel(path = file_path,
                           sheet = sheet,
                           col_names = FALSE,
                           .name_repair = "unique_quiet")
    }, error = function(e) e)  # if error, we store the error object

    if (!inherits(df, "error")) {
        # If readxl succeeded, return df as a tibble
        fundsr_msg("readxl succeeded. Returning data.", level = 2L)
        return(tibble::as_tibble(df, .name_repair = "minimal"))
    }

    fundsr_msg("readxl failed. Attempting parse as Excel 2003 XML...", level = 2L)

    # If readxl fails, we assume it might be a 2003 XML (SpreadsheetML) file.
    # We'll do BOM removal and parse with xml2.
    file_size <- file.info(file_path)$size
    if (is.na(file_size) || file_size == 0) {
        stop_bad_arg(
            "file_path",
            c("must refer to an existing non-empty file.",
              i = sprintf("file_path = %s.", sQuote(file_path)))
        )
    }
    raw_data <- readBin(file_path, what = "raw", n = file_size)

    # Helper to remove BOM if present (UTF-8 BOM 0xEF 0xBB 0xBF)
    raw_to_text_no_bom <- function(rdat) {
        if (length(rdat) >= 3 &&
                rdat[1] == as.raw(0xEF) &&
                rdat[2] == as.raw(0xBB) &&
                rdat[3] == as.raw(0xBF)) {
            rdat <- rdat[-(1:3)]
        }
        rawToChar(rdat, multiple = FALSE)
    }

    xml_text <- raw_to_text_no_bom(raw_data)
    xml_text <- gsub("S&P", "S&amp;P", xml_text, fixed = TRUE)

    parse_ok <- TRUE
    doc <- NULL

    tryCatch({
        doc <- xml2::read_xml(xml_text)
    }, error = function(e) {
        parse_ok <<- FALSE
    })

    if (!parse_ok || is.null(doc)) {
        readxl_msg <- conditionMessage(df)
        if (is.null(readxl_msg) || !nzchar(readxl_msg)) {
            readxl_msg <- "<unknown>"
        }
        fundsr_abort(
            msg = c(
                "Could not parse the file as Excel or as valid SpreadsheetML (Excel 2003 XML).",
                i = sprintf("file_path = %s.", sQuote(file_path)),
                "Original readxl error:",
                x = readxl_msg
            ),
            class = c("fundsr_bad_data", "fundsr_excel_import_failed"),
            parent = df
        )
    }

    # If doc parsed OK as XML, attempt to find <ss:Worksheet> etc.
    ws_nodes <- xml2::xml_find_all(
        doc,
        ".//ss:Worksheet",
        ns = c(ss = "urn:schemas-microsoft-com:office:spreadsheet")
    )
    if (length(ws_nodes) == 0) {
        fundsr_abort(
            msg = c(
                "No <ss:Worksheet> found in the XML.",
                i = sprintf("file_path = %s.", sQuote(file_path))
            ),
            class = c("fundsr_bad_data", "fundsr_excel_import_failed")
        )
    }

    # If user gave a sheet name/number, try to match it
    if (is.null(sheet)) {
        matched_ws <- ws_nodes[[1]]
    } else if (!is.null(sheet_idx)) {
        if (sheet_idx > length(ws_nodes)) {
            stop_bad_arg(
                "sheet [if numeric]",
                c(
                    "must be between 1 and the number of worksheets in the XML file.",
                    i = sprintf("sheet = %d.", sheet_idx),
                    i = sprintf("n_sheets = %d.", length(ws_nodes)),
                    i = sprintf("file_path = %s.", sQuote(file_path))
                )
            )
        }
        matched_ws <- ws_nodes[[sheet_idx]]
    } else {
        # sheet_name is non-NULL here
        matched_ws <- NULL
        sheet_trim <- trimws(sheet_name)

        for (ws in ws_nodes) {
            nm_orig <- xml2::xml_attr(ws, "Name", ns = "ss")
            if (is.na(nm_orig)) nm_orig <- xml2::xml_attr(ws, "Name")
            if (is.na(nm_orig)) next

            if (trimws(nm_orig) == sheet_trim) {
                matched_ws <- ws
                break
            }
        }

        if (is.null(matched_ws)) {
            fundsr_warn(paste0("Could not find a Worksheet named ",
                               sQuote(sheet_name),
                               ". Selecting the first sheet."))
            matched_ws <- ws_nodes[[1]]
        }
    }

    tbl_node <- xml2::xml_find_first(
        matched_ws,
        ".//ss:Table",
        ns = c(ss = "urn:schemas-microsoft-com:office:spreadsheet")
    )
    if (inherits(tbl_node, "xml_missing") || is.null(tbl_node)) {
        fundsr_abort(
            msg = c(
                "No <ss:Table> found in the selected Worksheet.",
                i = sprintf("file_path = %s.", sQuote(file_path))
            ),
            class = c("fundsr_bad_data", "fundsr_excel_import_failed")
        )
    }

    row_nodes <- xml2::xml_find_all(
        tbl_node,
        ".//ss:Row",
        ns = c(ss = "urn:schemas-microsoft-com:office:spreadsheet")
    )

    # Build rows
    rows_list <- lapply(row_nodes, function(rnode) {
        cell_nodes <- xml2::xml_find_all(
            rnode,
            ".//ss:Cell",
            ns = c(ss = "urn:schemas-microsoft-com:office:spreadsheet")
        )
        row_cells <- sapply(cell_nodes, function(cnode) {
            data_node <- xml2::xml_find_first(
                cnode,
                ".//ss:Data",
                ns = c(ss = "urn:schemas-microsoft-com:office:spreadsheet")
            )
            if (inherits(data_node, "xml_missing") || is.null(data_node)) {
                return(NA_character_)
            }
            xml2::xml_text(data_node)
        })
        unname(row_cells)
    })

    max_cols <- max(sapply(rows_list, length))
    row_arrays <- lapply(rows_list, function(rr) {
        length(rr) <- max_cols  # pad with NAs
        rr
    })
    mat <- do.call(rbind, row_arrays)
    df2 <- as.data.frame(mat, stringsAsFactors = FALSE)
    df2 <- tibble::as_tibble(df2, .name_repair = "minimal")
    df2
}

#' Read a time series from an Excel workbook
#'
#' Reads an Excel sheet, detects the header row by searching for a date header,
#' parses the date column, selects/renames value columns by regex, and optionally
#' coerces value columns to numeric.
#'
#' The sheet is read using `read_excel_or_xml()` (tries `readxl` first, then an
#' XML fallback). Completely empty columns are dropped. The first row containing
#' `date_col` (any cell match) is treated as the header row; data starts
#' below it.
#'
#' Date parsing:
#' - If the detected date column is numeric (or looks numeric), it is interpreted
#'   as an Excel serial date with origin `"1899-12-30"`.
#' - Otherwise the date strings are cleaned (truncated to 24 chars, `"Sept"` →
#'   `"Sep"`, trailing `" 12:00:00 AM"` removed) and parsed with `as.Date()` using
#'   formats from `make_date_fmts(date_order)`.
#' After parsing, the function drops all rows from the first unparseable date
#' onward (i.e., it truncates at the first `NA` date), then filters remaining
#' `NA` dates.
#'
#' Column selection/renaming:
#' `col_trans` maps desired output names to regex patterns matched against the
#' detected header names. If a pattern matches multiple columns, they are kept
#' and suffixed (`name`, `name2`, `name3`, ...).
#'
#' Numeric coercion:
#' For non-date columns, character values have `"$"` / `"USD "` stripped, commas
#' replaced by `comma_rep`, then are converted with `as.numeric()`. If
#' `force_numeric = TRUE`, the converted numeric column is kept even if some
#' values fail to parse; otherwise the column is only replaced when all non-`NA`
#' values parse successfully.
#'
#' @param file Path to the Excel workbook, relative to `fundsr.data_dir`.
#' @param sheet Sheet identifier to read from (sheet name or 1-based index).
#' @param date_col String used to detect the header row and identify the
#'   date column (matched via regex against cell contents for header-row
#'   detection, and against column names after headers are assigned).
#' @param col_trans Named character vector (or list) mapping output column names
#'   to regex patterns used to select columns from the sheet. Names are returned
#'   column names; values are patterns matched against header names.
#' @param date_order Character scalar indicating day/month/year order used to
#'   generate candidate date formats for parsing text dates (passed to
#'   `make_date_fmts()`). Default is `"dmy"`.
#' @param force_numeric Logical. If `TRUE` (default), always replace matched
#'   value columns with their numeric coercions (non-parsable values become
#'   `NA`). If `FALSE`, only replace when coercion succeeds for all non-`NA`
#'   values.
#' @param comma_rep Character scalar used when converting character numerics:
#'   commas are replaced by this string before conversion. Default `"."` (treat
#'   comma as decimal separator).
#'
#' @return A tibble with a `date` column (class `Date`) and the selected value
#'   columns (possibly numeric), with names determined by `col_trans`.
#'
#' @seealso [read_timeseries()] for CSV/TSV time series import.
#'
#' @family fund/index file readers
#' @export
#'
#' @examples
#' \dontrun{
#' x <- read_timeseries_excel(
#'   file = "example.xlsx",
#'   sheet = 1,
#'   date_col = "^Date$",
#'   col_trans = c(nav = "NAV", tr = "TR"),
#'   date_order = "dmy"
#' )
#' }
read_timeseries_excel <- function(file,
                                  sheet,
                                  date_col,
                                  col_trans,
                                  date_order = "dmy",
                                  force_numeric = TRUE,
                                  comma_rep = ".") {
    check_string(file)
    check_string(date_col)
    check_mapping(col_trans, allow_empty = FALSE, scalar_values = TRUE, type = "either")
    check_string(date_order, n_chars = 3L, pattern = "^[dmyM]{3}$")
    check_logical(force_numeric)
    check_string(comma_rep, allow_empty = TRUE)

    fund_data_dir <- fundsr_get_option("data_dir")
    xl_file <- file.path(fund_data_dir, file)

    # Mostly AI-written, except step (6) which is optimized (tidier alternative
    # based on lubridate::parse_date_time() is too slow when importing many funds).
    # Other steps could be cleaned up in the future.

    # 1) Read raw data (now tries readxl first, then XML fallback)
    raw_data <- read_excel_or_xml(file_path = xl_file, sheet = sheet)

    # 2) Remove columns entirely NA
    raw_data <- select(raw_data, where(~ any(!is.na(.x))))

    # 3) Find row containing date_col
    row_header_idx <- which(apply(raw_data, 1, function(x) {
        any(stringr::str_detect(as.character(x), date_col), na.rm = TRUE)
    }))
    if (length(row_header_idx) == 0) {
        stop_bad_arg(
            "date_col",
            c(
                "did not match any cell; could not detect a header row.",
                i = sprintf("file = %s.", sQuote(xl_file)),
                i = sprintf("sheet = %s.", sQuote(sheet))
            )
        )
    }
    row_header_idx <- row_header_idx[1]

    if (row_header_idx == nrow(raw_data)) {
        fundsr_abort(
            msg = c(
                sprintf(
                    "The detected header row (%d) is the last row; no data below it.",
                    row_header_idx
                ),
                i = sprintf("file = %s.", sQuote(xl_file)),
                i = sprintf("sheet = %s.", sQuote(sheet))
            ),
            class = c("fundsr_bad_data", "fundsr_excel_import_failed")
        )
    }

    # 4) Extract that row as header names, slice data below
    header_values <- raw_data[row_header_idx, , drop = TRUE] %>%
        as.character() %>%
        stringr::str_trim() %>%
        tidyr::replace_na("...") %>%
        (function(x) ifelse(x == "", "...", x))() %>%
        make.unique(sep = "_")

    data_raw <- slice(raw_data, (row_header_idx + 1):nrow(raw_data))

    if (length(header_values) != ncol(data_raw)) {
        if (length(header_values) > ncol(data_raw)) {
            header_values <- header_values[seq_len(ncol(data_raw))]
        }
    }
    colnames(data_raw) <- header_values

    # 5) Identify date column, rename => "date"
    date_col_idx <- which(stringr::str_detect(names(data_raw), date_col))
    if (length(date_col_idx) == 0) {
        stop_bad_arg(
            "date_col",
            c(
                "did not match any column after assigning header names.",
                i = sprintf("file = %s.", sQuote(xl_file)),
                i = sprintf("sheet = %s.", sQuote(sheet))
            )
        )
    }
    date_col_idx <- date_col_idx[1]
    old_date_name <- names(data_raw)[date_col_idx]
    data_raw <- rename(data_raw, date = !!old_date_name)

    # 6) Parse date, drop trailing junk
    if (is.numeric(data_raw$date)) {
        safe_dates <- as.Date(data_raw$date, origin = "1899-12-30")
    } else if (grepl("^[0-9]+(\\.[0-9]+)?$", data_raw$date[1])) {
        safe_dates <- as.Date(suppressWarnings(as.numeric(data_raw$date)), origin = "1899-12-30")
    } else {
        dts <- data_raw$date
        dts[!is.na(dts) & nchar(dts) > 24] <-
            substr(dts[!is.na(dts) & nchar(dts) > 24], 1, 24)
        dts <- gsub("Sept", "Sep", dts)
        dts <- gsub(" 12:00:00 AM$", "", dts)
        fmts <- make_date_fmts(date_order)
        safe_dates <- as.Date(dts, tryFormats = fmts)
    }

    first_junk <- which(is.na(safe_dates))[1]
    if (!is.na(first_junk)) {
        data_raw <- data_raw[seq_len(first_junk - 1), , drop = FALSE]
        safe_dates <- safe_dates[seq_len(first_junk - 1)]
    }
    data_raw$parsed_date <- safe_dates[seq_len(nrow(data_raw))]
    data_raw <- filter(data_raw, !is.na(.data$parsed_date))
    data_raw$date <- data_raw$parsed_date
    data_raw$parsed_date <- NULL

    # 6b) Dates must be unique after parsing
    dup_pos <- anyDuplicated(data_raw$date)
    if (dup_pos > 0L) {
        dup_dates <- unique(data_raw$date[duplicated(data_raw$date)])
        ex <- format(utils::head(sort(dup_dates), 5L), "%Y-%m-%d")

        fundsr_abort(
            msg = c(
                "Parsed dates are not unique.",
                i = sprintf("n_unique = %d.", length(unique(data_raw$date))),
                i = sprintf("n_rows   = %d.", nrow(data_raw)),
                i = sprintf("examples = %s.", paste(ex, collapse = ", ")),
                i = sprintf("file     = %s.", sQuote(xl_file)),
                i = sprintf("sheet    = %s.", sQuote(sheet))
            ),
            class = c("fundsr_duplicate_dates", "fundsr_bad_data", "fundsr_excel_import_failed")
        )
    }

    # 7) Match columns by regex, rename them
    matched_cols <- purrr::map2(
        .x = names(col_trans),
        .y = unname(col_trans),
        ~ {
            idx <- which(stringr::str_detect(names(data_raw), .y))
            if (!length(idx)) return(NULL)
            if (length(idx) == 1) {
                newn <- .x
            } else {
                newn <- c(.x, paste0(.x, seq(2, length(idx))))
            }
            tibble(old_name = names(data_raw)[idx], new_name = newn)
        }
    ) %>%
        purrr::compact() %>%
        bind_rows()

    keep_cols <- c("date", matched_cols$old_name)
    keep_cols <- unique(keep_cols)
    data_subset <- data_raw[, keep_cols, drop = FALSE]

    if (nrow(matched_cols) > 0) {
        nm <- stats::setNames(matched_cols$old_name, matched_cols$new_name)
        data_subset <- rename(data_subset, !!!nm)
    }

    # 8) Convert `date` to Date, attempt numeric for others
    data_subset$date <- as.Date(data_subset$date)
    for (col_name in setdiff(names(data_subset), "date")) {
        if (is.character(data_subset[[col_name]])) {
            old_vals <- data_subset[[col_name]]
            new_vals <- gsub("\\$|USD ?", "", data_subset[[col_name]])
            new_vals <- gsub(",", comma_rep, new_vals)
            new_vals <- suppressWarnings(as.numeric(new_vals))
            parse_failed <- which(!is.na(old_vals) & is.na(new_vals))
            if (force_numeric || length(parse_failed) == 0) {
                data_subset[[col_name]] <- new_vals
            }
        }
    }

    fundsr_msg(c(v = glue(
        "{nrow(data_subset)} rows x {ncol(data_subset)} cols ",
        "(sheet='{sheet}', date col ='{date_col}')."
    )), level = 2L)

    data_subset
}
