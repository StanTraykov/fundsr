# © 2023–2025 Stanislav Traykov <st@gmuf.com>.
# Personal use and non-commercial result sharing permitted; commercial use requires permission.

read_excel_or_xml <- function(file_path, sheet = NULL) {
    # First, try reading as a standard Excel file with readxl
    message("Attempting readxl on '", file_path, "'...")
    df <- tryCatch({
        # Try readxl in a tryCatch
        readxl::read_excel(path = file_path,
                           sheet = sheet,
                           col_names = FALSE,
                           .name_repair = "unique_quiet")
    }, error = function(e) e)  # if error, we store the error object

    if (!inherits(df, "error")) {
        # If readxl succeeded, return df as a tibble
        message("readxl succeeded. Returning data.")
        return(tibble::as_tibble(df, .name_repair = "minimal"))
    }

    message("readxl failed. Attempting parse as Excel 2003 XML...")

    # ------------------------------------------------------------------
    # If readxl fails, we assume it might be a 2003 XML (SpreadsheetML) file.
    # We'll do BOM removal and parse with xml2.
    # ------------------------------------------------------------------
    file_size <- file.info(file_path)$size
    if (is.na(file_size) || file_size == 0) {
        stop("File '", file_path, "' not found or is empty.")
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
        stop("Could not parse '", file_path, "' as Excel or as valid 2003 XML.\n",
             "Original readxl error was: ", df$message)
    }

    # If doc parsed OK as XML, attempt to find <ss:Worksheet> etc.
    ws_nodes <- xml2::xml_find_all(
        doc,
        ".//ss:Worksheet",
        ns = c(ss = "urn:schemas-microsoft-com:office:spreadsheet")
    )
    if (length(ws_nodes) == 0) {
        stop("No <ss:Worksheet> found in the XML for '", file_path, "'.")
    }

    # If user gave a sheet name, try to match it
    if (!is.null(sheet)) {
        matched_ws <- NULL
        sheet_trim <- trimws(sheet)
        for (ws in ws_nodes) {
            nm_orig <- xml2::xml_attr(ws, "Name", ns = "ss")
            if (is.na(nm_orig)) {
                nm_orig <- xml2::xml_attr(ws, "Name")
            }
            if (is.na(nm_orig)) next
            nm_trim <- trimws(nm_orig)
            #message("DEBUG: found Worksheet '", nm_trim, "'; comparing to '", sheet_trim, "'")
            if (nm_trim == sheet_trim) {
                matched_ws <- ws
                break
            }
        }
        if (is.null(matched_ws)) {
            warning("Could not find a Worksheet named '", sheet, "'; using the first.")
            matched_ws <- ws_nodes[[1]]
        }
    } else {
        matched_ws <- ws_nodes[[1]]
    }

    tbl_node <- xml2::xml_find_first(
        matched_ws,
        ".//ss:Table",
        ns = c(ss = "urn:schemas-microsoft-com:office:spreadsheet")
    )
    if (inherits(tbl_node, "xml_missing") || is.null(tbl_node)) {
        stop("No <ss:Table> found in the selected Worksheet for '", file_path, "'.")
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
    return(df2)
}

import_xl_data <- function(xl_file,
                           data_sheet,
                           date_field_name,
                           col_trans,
                           date_order = "dmy",
                           force_numeric = TRUE,
                           comma_rep = ".") {

    # 1) Read raw data (now tries readxl first, then XML fallback)
    raw_data <- read_excel_or_xml(file_path = xl_file, sheet = data_sheet)

    # 2) Remove columns entirely NA
    raw_data <- dplyr::select(raw_data, dplyr::where(~ any(!is.na(.x))))

    # 3) Find row containing date_field_name
    row_header_idx <- which(apply(raw_data, 1, function(x) {
        any(stringr::str_detect(as.character(x), date_field_name), na.rm = TRUE)
    }))
    if (length(row_header_idx) == 0) {
        stop(glue::glue(
            "Could NOT find the date column header matching '{date_field_name}' ",
            "in '{xl_file}', sheet '{data_sheet}'."
        ))
    }
    row_header_idx <- row_header_idx[1]

    if (row_header_idx == nrow(raw_data)) {
        stop(glue::glue(
            "The detected header row ({row_header_idx}) is the last row. ",
            "No data below it in '{xl_file}', sheet '{data_sheet}'."
        ))
    }

    # 4) Extract that row as header names, slice data below
    header_values <- raw_data[row_header_idx, , drop = TRUE] %>%
        as.character() %>%
        stringr::str_trim() %>%
        tidyr::replace_na("...") %>%
        (\(x) ifelse(x == "", "...", x))() %>%
        make.unique(sep = "_")

    data_raw <- dplyr::slice(raw_data, (row_header_idx + 1):nrow(raw_data))

    if (length(header_values) != ncol(data_raw)) {
        if (length(header_values) > ncol(data_raw)) {
            header_values <- header_values[seq_len(ncol(data_raw))]
        }
    }
    colnames(data_raw) <- header_values

    # 5) Identify date column, rename => "date"
    date_col_idx <- which(stringr::str_detect(names(data_raw), date_field_name))
    if (length(date_col_idx) == 0) {
        stop(glue::glue(
            "Could NOT find a column matching '{date_field_name}' ",
            "after assigning header names in '{xl_file}', sheet '{data_sheet}'."
        ))
    }
    date_col_idx <- date_col_idx[1]
    old_date_name <- names(data_raw)[date_col_idx]
    data_raw <- dplyr::rename(data_raw, date = !!old_date_name)

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
        if (date_order == "dmy") {
            fmts <- c("%d/%m/%Y", "%d.%m.%Y", "%d %b %Y", "%d/%b/%Y", "%d-%b-%Y")
        } else { #mdy
            fmts <- c("%m/%d/%Y", "%b %d, %Y")
        }
        safe_dates <- as.Date(dts, tryFormats = fmts)
    }

    first_junk <- which(is.na(safe_dates))[1]
    if (!is.na(first_junk)) {
        data_raw <- data_raw[seq_len(first_junk - 1), , drop = FALSE]
        safe_dates <- safe_dates[seq_len(first_junk - 1)]
    }
    data_raw$parsed_date <- safe_dates[seq_len(nrow(data_raw))]
    data_raw <- dplyr::filter(data_raw, !is.na(.data$parsed_date))
    data_raw$date <- data_raw$parsed_date
    data_raw$parsed_date <- NULL

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
            dplyr::tibble(old_name = names(data_raw)[idx], new_name = newn)
        }
    ) %>%
        purrr::compact() %>%
        dplyr::bind_rows()

    keep_cols <- c("date", matched_cols$old_name)
    keep_cols <- unique(keep_cols)
    data_subset <- data_raw[, keep_cols, drop = FALSE]

    if (nrow(matched_cols) > 0) {
        nm <- stats::setNames(matched_cols$old_name, matched_cols$new_name)
        data_subset <- dplyr::rename(data_subset, !!!nm)
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

    message(glue::glue(
        "Returning {nrow(data_subset)} rows x {ncol(data_subset)} columns ",
        "from '{xl_file}' (sheet='{data_sheet}', date_field='{date_field_name}')."
    ))

    data_subset
}
