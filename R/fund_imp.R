#' Register an import function
#'
#' Appends `fun` to the internal import-function registry (`.fundsr$import_funs`).
#' Registered functions are intended to be run sequentially in registration order.
#'
#' @param fun A function to register. Must take no arguments.
#'
#' @return Invisibly returns the updated `.fundsr$import_funs` list.
#' @export
#'
#' @examples
#' add_import_fun(function() NULL)

add_import_fun <- function(fun) {
    if (!is.function(fun)) {
        stop("`fun` must be a function.", call. = FALSE)
    }
    if (length(formals(fun)) != 0L) {
        stop("`fun` must take no arguments.", call. = FALSE)
    }
    if (is.null(.fundsr$import_funs)) {
        .fundsr$import_funs <- list()
    }
    if (!is.list(.fundsr$import_funs)) {
        stop("Internal registry `.fundsr$import_funs` must be a list.", call. = FALSE)
    }
    .fundsr$import_funs <- c(.fundsr$import_funs, list(fun))
    invisible(.fundsr$import_funs)
}

#' Clear registered import functions
#'
#' Clears the internal import-function registry (`.fundsr$import_funs`), removing
#' all previously registered import functions.
#'
#' @return Invisibly returns `NULL`. Called for side effects.
#' @export
#'
#' @examples
#' clear_import_funs()
clear_import_funs <- function() {
    .fundsr$import_funs <- list()
    invisible(NULL)
}

#' Load or refresh the fund storage environment
#'
#' Runs the import function registry (`.fundsr$import_funs`) to populate (or refresh)
#' the package's storage environment (`.fundsr_storage`).
#'
#' The function temporarily sets the `fundsr.redo_import` option so that import
#' functions can decide whether to recompute cached objects.
#'
#' @param redo Logical scalar. If `TRUE`, forces a full re-import by setting
#'   `options(fundsr.redo_import = TRUE)` for the duration of this call.
#'
#' @return Invisibly returns `.fundsr_storage` after running the import functions.
#'
#' @details
#' The previous value of `getOption("fundsr.redo_import")` is restored on exit,
#' even if an import function errors.
#'
#' Import functions are taken from `.fundsr$import_funs` and are called
#' sequentially in registration order. Each registered function must take
#' no arguments.
#'
#' @export
import_funds <- function(redo = FALSE) {
    if (!is.logical(redo) || length(redo) != 1L || is.na(redo)) {
        stop("`redo` must be TRUE or FALSE.", call. = FALSE)
    }
    if (!exists(".fundsr_storage", inherits = TRUE) || !is.environment(.fundsr_storage)) {
        stop("Internal storage `.fundsr_storage` is not initialised.", call. = FALSE)
    }
    fns <- .fundsr$import_funs
    if (is.null(fns)) fns <- list()
    if (!is.list(fns)) {
        stop("Internal registry `.fundsr$import_funs` must be a list of functions.", call. = FALSE)
    }
    for (i in seq_along(fns)) {
        fn <- fns[[i]]
        if (!is.function(fn)) {
            stop(sprintf("`.fundsr$import_funs[[%d]]` is not a function.", i), call. = FALSE)
        }
        if (length(formals(fn)) != 0L) {
            stop(sprintf("`.fundsr$import_funs[[%d]]` must take no arguments.", i), call. = FALSE)
        }
    }
    old <- getOption("fundsr.redo_import", FALSE)
    options(fundsr.redo_import = redo)
    on.exit(options(fundsr.redo_import = old), add = TRUE)

    for (fn in fns) fn()

    invisible(.fundsr_storage)
}

#' Coalesce suffixed join columns into unsuffixed base columns
#'
#' Helper for post-processing join results that use suffixes such as
#' `.x` / `.y`. For each base name that appears with both suffixes
#' (e.g. `FTAW.x` and `FTAW.y`), this function creates a new column
#' `FTAW` as `dplyr::coalesce(FTAW.x, FTAW.y)` and drops the suffixed
#' columns. The order of `suffix` controls which column is preferred.
#'
#' @param df A data frame or tibble produced by joins.
#' @param suffix Character vector of length 2 giving the suffixes to
#'   coalesce, in priority order. For example, `c(".x", ".y")` uses
#'   `.x` as the primary source and falls back to `.y` when `.x` is `NA`.
#'
#' @return A tibble with the same rows as `df`, where any pairs of
#'   suffixed columns (e.g. `name.x` / `name.y`) are replaced by a single
#'   unsuffixed column (e.g. `name`) containing the coalesced values.
#'
#' @keywords internal
coalesce_join_suffixes <- function(df, suffix = c(".x", ".y")) {
    df  <- tibble::as_tibble(df)
    nms <- names(df)
    sx <- suffix[1]
    sy <- suffix[2]

    # find base names that have both .x and .y variants
    base_x <- sub(paste0("\\Q", sx, "\\E$"), "", nms[endsWith(nms, sx)])
    base_y <- sub(paste0("\\Q", sy, "\\E$"), "", nms[endsWith(nms, sy)])
    base   <- intersect(base_x, base_y)

    if (length(base) == 0L) return(df)

    for (b in base) {
        cx <- paste0(b, sx)
        cy <- paste0(b, sy)

        df <- df %>%
            mutate(
                !!b := dplyr::coalesce(.data[[cx]], .data[[cy]]),
                .keep = "unused"
            )
    }

    df
}

#' Join all tibbles in an environment with optional late left-joins
#'
#' Performs a `dplyr::full_join()` across all objects in an environment,
#' except those listed in `late`. Objects listed in `late` are instead
#' joined afterwards using `dplyr::left_join()` in the order given.
#' Optionally, any columns created with join suffixes (such as `.x` /
#' `.y`) can be automatically coalesced into single unsuffixed columns.
#'
#' @param env Environment containing the tibbles to join.
#' @param by Character vector of join keys (passed to `dplyr::full_join()`).
#' @param late Character vector of object names in `env` that should be
#'   *excluded* from the initial full join and instead left-joined
#'   afterwards. Defaults to `NULL`.
#' @param coalesce_suffixed Optional character vector of length 2 giving
#'   join suffixes to coalesce (for example, `c(".x", ".y")` or
#'   `c(".y", ".x")`). When non-`NULL`, any pairs of columns whose names
#'   end in these suffixes (and share the same base name) are replaced by
#'   a single unsuffixed column containing the coalesced values. If
#'   `NULL` (the default), no automatic coalescing is performed.
#'
#' @return A tibble: the full join of all non-late objects, followed by
#'   sequential left-joins of the late objects. If `coalesce_suffixes`
#'   is supplied, suffixed join columns are coalesced into unsuffixed
#'   base columns as described above.
#'
#' @details
#' This helper is designed for workflows where the majority of tables
#' should be fully joined, while certain sparse or auxiliary tables
#' (e.g. calendars, metadata) should only be attached via `left_join()`.
#'
#' When `coalesce_suffixed` is provided, the function uses an internal
#' helper to replace pairs like `name.x` / `name.y` with a single
#' `name` column whose values are taken from the first suffix in
#' `coalesce_suffixed` and then, where missing, from the second.
#'
#' If a name in `late` does not exist in `env`, it is ignored with a
#' warning.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   e <- new.env()
#'   e$a <- tibble::tibble(date = 1:3, x = 1:3)
#'   e$b <- tibble::tibble(date = 1:3, x = c(NA, 20, 30))
#'
#'   # Simple full join of a and b
#'   join_env(e, by = "date")
#'
#'   # Full join with automatic coalescing of x.x / x.y into x
#'   join_env(e, by = "date", coalesce_suffixes = c(".x", ".y"))
#' }
join_env <- function(env, by, late = NULL, coalesce_suffixed = NULL) {
    obj_names <- ls(envir = env, sorted = FALSE)

    message(glue("Joining: {glue::glue_collapse(obj_names, sep = ', ')}"))

    raw_late <- late %||% character(0)
    missing_late <- setdiff(raw_late, obj_names)
    if (length(missing_late)) {
        warning("Objects not found in `env` and ignored in `late`: ",
                paste(missing_late, collapse = ", "))
    }
    late <- intersect(raw_late, obj_names)

    main_names <- setdiff(obj_names, late)

    if (length(main_names) == 0L) {
        stop("No objects left to full_join after excluding `late`.")
    }

    main_list <- mget(main_names, envir = env)
    j <- purrr::reduce(main_list, dplyr::full_join, by = by)

    if (length(late) > 0L) {
        late_list <- mget(late, envir = env)
        j <- purrr::reduce(late_list, dplyr::left_join, .init = j, by = by)
    }

    if (!is.null(coalesce_suffixed))
        j <- coalesce_join_suffixes(j, coalesce_suffixed)

    j
}

#' Get the internal fund index map
#'
#' Returns the package's fund index lookup table stored in
#' `.fundsr$fund_index`.
#'
#' @return A character vector or named list representing the internal
#'   fund index mapping.
#'
#' @export
get_fund_index <- function() {
    .fundsr$fund_index
}

#' Get the internal fund storage
#'
#' Returns the fundsr's fund storage environment
#' (`.fundsr_storage`).
#'
#' @return The storage environment.
#'
#' @export
get_storage <- function() {
    .fundsr_storage
}

save_as <- function(named_urls, path = getOption("fundsr.data_dir"), redownload = FALSE) {
    if (!dir.exists(path)) dir.create(path, recursive = TRUE)
    for (file in names(named_urls)) {
        url <- named_urls[[file]]
        # hack .xls extension for iShares downloads (not critical but allows Excel to open
        # these files with a warning [outdated XML])
        ext <- if (grepl("ishares", url, ignore.case = TRUE)) {
            ".xls"
        } else {
            ".xlsx"
        }
        full_file <- file.path(path, paste0(file, ext))
        if (redownload || !file.exists(full_file)) {
            message(glue("Downloading '{file}'"))
            Sys.sleep(stats::runif(1, 0.5, 1.0))
            utils::download.file(url, full_file, mode = "wb")
        } else {
            message(glue("Skipping '{file}': file already exists."))
        }
    }
}

#' Download fund data according to the configured download list
#'
#' Retrieves all fund data files listed in the `fundsr.dl_list` option
#' and saves them into the directory specified by `fundsr.data_dir`.
#'
#' @param redownload Logical; if `TRUE`, existing files are overwritten.
#'   If `FALSE`, only missing files are downloaded.
#'
#' @return Invisibly returns `NULL`. Files are written as a side effect.
#'
#' @details
#' The function delegates the download and save process to `save_as()`,
#' using the download specifications stored in the package option
#' `fundsr.dl_list`.
#'
#' @export
dl_funds <- function(redownload = FALSE) {
    dl_list <- getOption("fundsr.dl_list")
    fund_data_dir <- getOption("fundsr.data_dir")
    save_as(dl_list, path = fund_data_dir, redownload = redownload)
}

#' Add entries to the fund download list
#'
#' Adds one or more named download specifications to the `fundsr.dl_list`
#' option. Existing entries are preserved; entries in `x` replace any
#' existing entries with the same name.
#'
#' @param x A named character vector mapping download identifiers to URLs.
#'
#' @return A list with the previous value of `fundsr.dl_list`.
#' @export
add_to_dl_list <- function(x) {
    stopifnot(is.character(x), !is.null(names(x)), all(nzchar(names(x))))
    cur <- getOption("fundsr.dl_list", character())
    new <- c(cur, x)
    new <- new[!duplicated(names(new), fromLast = TRUE)]
    options(fundsr.dl_list = new)
}


#' Read a CSV with a date + one or more value (fund NAV/index level) columns.
#'
#' Loads a CSV from the directory specified by `fundsr.data_dir`,
#' converts the `date` column to a proper Date, and coerces all other
#' columns to numeric.
#'
#' @param file Filename of the CSV to read (relative to `getOption("fundsr.data_dir")`).
#' @param date_div Divider applied to the raw date field before conversion.
#'   Defaults to `1000` (ms precision).
#'
#' @return A tibble with parsed `date` and numeric value columns.
#'
#' @details
#' The function assumes a column named `date` exists and represents a Unix
#' timestamp in seconds or milliseconds. All non-`date` columns are coerced
#' with `as.numeric()` (non-parsable values become `NA`).
#'
#' @export
get_csv <- function(file, date_div = 1000) {
    fund_data_dir <- getOption("fundsr.data_dir")
    df <- readr::read_csv(file.path(fund_data_dir, file), show_col_types = FALSE)
    df %>%
        mutate(
            date = lubridate::as_date(lubridate::as_datetime(as.numeric(date) / date_div)),
            across(-date, ~ suppressWarnings(as.numeric(.x)))
        )
}


#' Read an MSCI two-column TSV file
#'
#' Extracts the data portion of an MSCI TSV file—skipping header noise—
#' and reads it as a two-column table containing a date and a numeric
#' value.
#'
#' @param file Filename of the TSV to read.
#'
#' @return A tibble with a `Date` column and one numeric column.
#'
#' @details
#' The function filters lines beginning with a date or the literal
#' `"Date"`, then parses them using a fixed `%m/%d/%Y` date format and a
#' numeric second field.
#'
#' @export
get_msci_tsv <- function(file) {
    fund_data_dir <- getOption("fundsr.data_dir")
    lines <- readr::read_lines(file.path(fund_data_dir, file))
    data_lines <- grep("^[0-9]|Date", lines, value = TRUE)
    df <- readr::read_tsv(I(data_lines), col_types = readr::cols(
        readr::col_date(format = "%m/%d/%Y"),
        readr::col_double()
    ))
    df
}

#' Set or update a cached object in the package storage environment
#'
#' Evaluates an expression and assigns its result into the internal
#' `.fundsr_storage` environment under the given variable name.
#' The value is recomputed only if it is missing or if the global option
#' `fundsr.redo_import` is set to `TRUE`. Optionally updates the global
#' fund-index mapping.
#'
#' @param var_name Name of the variable to store in `.fundsr_storage`.
#' @param expr An expression to evaluate when (re)computing the value.
#' @param add_fi_pairs Optional named vector or list of fund–index
#'   pairs to append to `.fundsr$fund_index`.
#'
#' @return Invisibly returns `NULL`. Called for its side effects.
#'
#' @details
#' The expression `expr` is evaluated in the caller's environment and
#' then stored in `.fundsr_storage`. This function provides memoisation-
#' like behaviour for costly imports or transformations that should
#' persist across function calls.
#'
#' @export
setg <- function(var_name, expr, add_fi_pairs = NULL) {
    message(paste("*** Importing:", var_name))
    # Access the parent's environment (where setg was called)
    parent_env <- parent.frame()
    # Get global redo flag
    redo <- getOption("fundsr.redo_import", FALSE)
    # Check if assignment is needed and evaluate expr in parent_env
    if (redo || !exists(var_name, envir = .fundsr_storage)) {
        assign(var_name,
               eval(substitute(expr), envir = parent_env),
               envir = .fundsr_storage)
        # Also add fund index pairs to global map (if supplied)
        if (!is.null(add_fi_pairs)) {
            .fundsr$fund_index[names(add_fi_pairs)] <- add_fi_pairs
        }
    }
}

#' Import an MSCI index sheet and register benchmark mappings
#'
#' Wrapper around `setg()` and `import_xl_data()` for MSCI index files.
#'
#' @param var_name Storage key used in `.fundsr_storage`.
#' @param col_trans Named vector specifying column translations.
#' @param benchmarks Optional index mapping to record in
#'   the fund index map (used to map gross to net indices).
#' @param file Filename of the XLSX file to import.
#'
#' @return Invisibly returns `NULL`. Data are stored via `setg()`.
#'
#' @export
msci <- function(var_name, col_trans, benchmarks = NULL, file) {
    fund_data_dir <- getOption("fundsr.data_dir")
    setg(
        var_name = var_name,
        expr = import_xl_data(
            xl_file = file.path(fund_data_dir, file),
            data_sheet = 1,
            date_field_name = "^Date",
            col_trans = col_trans,
            date_order = "mdy",
            comma_rep = ""
        ),
        add_fi_pairs = benchmarks
    )
}

#' Import a fund's NAV data and register its benchmark mapping
#'
#' Imports a fund's NAV time series from an Excel file and stores it in
#' the internal storage via `setg()`. Optionally, a benchmark column can
#' also be imported, and a fund–index mapping is recorded in
#' `.fundsr$fund_index`.
#'
#' @param ticker Fund ticker symbol. Used (in lower case) as the storage
#'   key and (in upper case) to derive the default filename.
#' @param file Optional filename. If `NULL` (the default), it is
#'   constructed as `paste0(toupper(ticker), default_ext)`.
#' @param data_sheet Sheet index or name containing the NAV data.
#'   Defaults to `1`.
#' @param date_col Regular expression identifying the date column.
#'   Defaults to `"^Date"`.
#' @param nav_col Regular expression identifying the fund's NAV column.
#'   Defaults to `"^NAV"`.
#' @param benchmark Optional benchmark that this fund should
#'   be associated with in the fund index map. When
#'   `retrieve_benchmark = TRUE`, the same value is also used as the name
#'   under which the benchmark series is imported.
#' @param benchmark_col Regular expression identifying the benchmark
#'   column in the Excel sheet. Only used when `retrieve_benchmark = TRUE`.
#' @param retrieve_benchmark Logical; if `TRUE`, both `benchmark` and
#'   `benchmark_col` must be supplied and the benchmark column is imported
#'   alongside the fund.
#' @param date_order Date parsing order passed to the importer.
#'   Defaults to `"dmy"`.
#' @param default_ext Default filename extension used when `file` is not
#'   provided. Defaults to `".xlsx"`.
#'
#' @return Invisibly returns `NULL`. The imported data are stored in
#'   `.fundsr_storage` under `tolower(ticker)`, and a fund–index mapping
#'   is added to `.fundsr$fund_index` if `benchmark` is supplied.
#'
#' @details
#' The function builds a column-translation mapping from the fund NAV
#' column and, if requested, a benchmark column. It then calls
#' `import_xl_data()` to read the Excel file and `setg()` to cache the
#' imported object under `tolower(ticker)`. When `benchmark` is provided,
#' a corresponding entry is added to `.fundsr$fund_index` to link the
#' fund to its benchmark key.
#'
#' @export
fund_imp <- function(ticker,
                     file = NULL,
                     data_sheet = 1,
                     date_col = "^Date",
                     nav_col = "^NAV",
                     benchmark = NULL,
                     benchmark_col = NULL,
                     retrieve_benchmark = FALSE,
                     date_order = "dmy",
                     default_ext = ".xlsx") {
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
    setg(
        ticker_lower,
        import_xl_data(
            xl_file = file.path(fund_data_dir, file),
            data_sheet = data_sheet,
            date_field_name = date_col,
            col_trans = ct,
            date_order = date_order,
        ),
        add_fi_pairs = set_names(benchmark, ticker_lower)
    )

}

####### Manager-specific wrappers for fund_imp #######

#' Import an iShares fund
#'
#' Wrapper around `fund_imp()` for iShares files.
#'
#' @param ticker Fund ticker.
#' @param file Optional filename override.
#' @param benchmark Optional benchmark key.
#' @param retrieve_benchmark Logical; also import benchmark column.
#' @export
ishs <- function(ticker, file = NULL, benchmark = NULL, retrieve_benchmark = FALSE) {
    fund_imp(ticker = ticker,
             file = file,
             data_sheet = "Historical",
             date_col = "^As Of",
             benchmark = benchmark,
             benchmark_col = "^Benchmark Ret",
             retrieve_benchmark = retrieve_benchmark)
}

#' Import an SPDR fund
#'
#' Wrapper around `fund_imp()` for SPDR files.
#'
#' @param ticker Fund ticker.
#' @param file Optional filename override.
#' @param benchmark Optional benchmark key.
#' @export
spdr <- function(ticker, file = NULL, benchmark = NULL) {
    fund_imp(ticker = ticker,
             file = file,
             benchmark = benchmark)
}

#' Import an Xtrackers fund
#'
#' Wrapper around `fund_imp()` for Xtrackers files.
#'
#' @param ticker Fund ticker.
#' @param file Optional filename override.
#' @param benchmark Optional benchmark key.
#' @param retrieve_benchmark Logical; also import benchmark column.
#' @export
xtra <- function(ticker, file = NULL, benchmark = NULL, retrieve_benchmark = FALSE) {
    fund_imp(ticker = ticker,
             file = file,
             benchmark = benchmark,
             benchmark_col = "^Index Level",
             retrieve_benchmark = retrieve_benchmark)
}

#' Import an Amundi fund
#'
#' Wrapper around `fund_imp()` for Amundi files.
#'
#' @param ticker Fund ticker.
#' @param file Optional filename override.
#' @param benchmark Optional benchmark key.
#' @export
amun <- function(ticker, file = NULL, benchmark = NULL) {
    fund_imp(ticker = ticker,
             file = file,
             nav_col = "^Official NAV",
             benchmark = benchmark)
}

#' Import an Invesco fund
#'
#' Wrapper around `fund_imp()` for Invesco files.
#'
#' @param ticker Fund ticker.
#' @param file Optional filename override.
#' @param benchmark Optional benchmark key.
#' @param retrieve_benchmark Logical; also import benchmark column.
#' @export
inve <- function(ticker, file = NULL, benchmark = NULL, retrieve_benchmark = FALSE) {
    fund_imp(ticker = ticker,
             file = file,
             nav_col = "^NAV$", # need end marker ($) for Invesco do disambiguate
             benchmark = benchmark,
             benchmark_col = "^Index",
             retrieve_benchmark = retrieve_benchmark)
}

#' Import a Vanguard fund
#'
#' Wrapper around `fund_imp()` for Vanguard files.
#'
#' @param ticker Fund ticker.
#' @param file Optional filename override.
#' @param benchmark Optional benchmark key.
#' @export
vang <- function(ticker, file = NULL, benchmark = NULL) {
    fund_imp(ticker = ticker,
             file = file,
             nav_col = "^NAV \\(USD\\)$",
             benchmark = benchmark)
}

#' Import a UBS fund
#'
#' Wrapper around `fund_imp()` for UBS files.
#'
#' @param ticker Fund ticker.
#' @param file Optional filename override.
#' @param benchmark Optional benchmark key.
#' @export
ubs <- function(ticker, file = NULL, benchmark = NULL) {
    fund_imp(ticker = ticker,
             file = file,
             nav_col = "^Official NAV",
             benchmark = benchmark)
}

#' Import an HSBC fund
#'
#' Wrapper around `fund_imp()` for HSBC files.
#'
#' @param ticker Fund ticker.
#' @param file Optional filename override.
#' @param benchmark Optional benchmark key.
#' @export
hsbc <- function(ticker, file = NULL, benchmark = NULL) {
    fund_imp(ticker = ticker,
             file = file,
             benchmark = benchmark,
             date_order = "mdy")
}
