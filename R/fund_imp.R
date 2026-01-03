#' Register a data loader
#'
#' Appends `fun` to the internal data-loader registry (`.fundsr$data_loaders`).
#' Registered functions are intended to be run sequentially in registration
#' order.
#'
#' If a loader with the same function body is already registered, `fun` is not
#' added again.
#'
#' @param fun A function to register. Must take no arguments.
#'
#' @return Invisibly returns the updated `.fundsr$data_loaders` list.
#' @export
#'
#' @examples
#' add_data_loader(function() NULL)
add_data_loader <- function(fun) {
    if (!is.function(fun)) {
        stop("`fun` must be a function.", call. = FALSE)
    }
    if (length(formals(fun)) != 0L) {
        stop("`fun` must take no arguments.", call. = FALSE)
    }
    if (is.null(.fundsr$data_loaders)) {
        .fundsr$data_loaders <- list()
    }
    if (!is.list(.fundsr$data_loaders)) {
        stop("Internal registry `.fundsr$data_loaders` must be a list.", call. = FALSE)
    }

    sig <- function(f) paste(deparse(body(f)), collapse = "\n")

    fun_sig <- sig(fun)
    already <- any(vapply(.fundsr$data_loaders, function(g) {
        is.function(g) && identical(sig(g), fun_sig)
    }, logical(1)))

    if (!already) {
        .fundsr$data_loaders <- c(.fundsr$data_loaders, list(fun))
    }

    invisible(.fundsr$data_loaders)
}

#' Clear registered data loaders
#'
#' Clears the internal data-loader registry (`.fundsr$data_loaders`), removing
#' all previously registered data loader functions.
#'
#' @return Invisibly returns `NULL`. Called for side effects.
#' @export
#'
#' @examples
#' clear_data_loaders()
clear_data_loaders <- function() {
    .fundsr$data_loaders <- list()
    invisible(NULL)
}

#' Clear storage
#'
#' Removes all objects from the package's storage environment
#' (`.fundsr_storage`). Optionally also clears the fund/index map
#' (`.fundsr$fund_index_map`).
#'
#' @param clear_fund_index_map Logical scalar; if `TRUE`, also clears
#'   `.fundsr$fund_index_map`.
#'
#' @return Invisibly returns `NULL`. Called for side effects.
#' @export
#'
#' @examples
#' clear_storage()
#' clear_storage(clear_fund_index_map = TRUE)
clear_storage <- function(clear_fund_index_map = FALSE) {
    if (!is.logical(clear_fund_index_map) || length(clear_fund_index_map) != 1L || is.na(clear_fund_index_map)) {
        stop("`clear_fund_index_map` must be TRUE or FALSE.", call. = FALSE)
    }
    if (!exists(".fundsr_storage", inherits = TRUE) || !is.environment(.fundsr_storage)) {
        stop("Fundsr storage environment is not initialised.", call. = FALSE)
    }
    rm(list = ls(envir = .fundsr_storage, all.names = TRUE), envir = .fundsr_storage)
    if (isTRUE(clear_fund_index_map)) {
        .fundsr$fund_index_map <- character()
    }
    invisible(NULL)
}

#' Run registered data loaders
#'
#' Runs the data loader registry (`.fundsr$data_loaders`) to populate (or refresh)
#' the package's storage environment (`.fundsr_storage`).
#'
#' The function temporarily sets the `fundsr.reload` option so that data loaders
#' can decide whether to recompute cached objects.
#'
#' @param reload Logical scalar. If `TRUE`, forces a full reload by setting
#'   `options(fundsr.reload = TRUE)` for the duration of this call.
#'
#' @return Invisibly returns `.fundsr_storage` after running the data loaders.
#'
#' @details
#' The previous value of `getOption("fundsr.reload")` is restored on exit,
#' even if a data loader errors.
#'
#' Data loaders are taken from `.fundsr$data_loaders` and are called
#' sequentially in registration order. Each registered function must take
#' no arguments.
#'
#' @export
run_data_loaders <- function(reload = FALSE) {
    if (!is.logical(reload) || length(reload) != 1L || is.na(reload)) {
        stop("`reload` must be TRUE or FALSE.", call. = FALSE)
    }
    if (!exists(".fundsr_storage", inherits = TRUE) || !is.environment(.fundsr_storage)) {
        stop("Fundsr storage is not initialised.", call. = FALSE)
    }
    fns <- .fundsr$data_loaders
    if (is.null(fns)) fns <- list()
    if (!is.list(fns)) {
        stop("Internal registry `.fundsr$data_loaders` must be a list of functions.", call. = FALSE)
    }
    for (i in seq_along(fns)) {
        fn <- fns[[i]]
        if (!is.function(fn)) {
            stop(sprintf("`.fundsr$data_loaders[[%d]]` is not a function.", i), call. = FALSE)
        }
        if (length(formals(fn)) != 0L) {
            stop(sprintf("`.fundsr$data_loaders[[%d]]` must take no arguments.", i), call. = FALSE)
        }
    }
    old <- getOption("fundsr.reload", FALSE)
    options(fundsr.reload = reload)
    on.exit(options(fundsr.reload = old), add = TRUE)

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
                !!b := coalesce(.data[[cx]], .data[[cy]]),
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
#'   sequential left-joins of the late objects. If `coalesce_suffixed`
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
#'   join_env(e, by = "date", coalesce_suffixed = c(".x", ".y"))
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
    j <- purrr::reduce(main_list, full_join, by = by)

    if (length(late) > 0L) {
        late_list <- mget(late, envir = env)
        j <- purrr::reduce(late_list, left_join, .init = j, by = by)
    }

    if (!is.null(coalesce_suffixed))
        j <- coalesce_join_suffixes(j, coalesce_suffixed)

    j
}

#' Load and join all series from registered data loaders
#'
#' Runs [run_data_loaders()], joins the resulting environment into a single data
#' frame via [join_env()], and sorts the result by `by`.
#'
#' This function is a convenience wrapper for the common workflow used in
#' examples and vignettes.
#'
#' @param by Column name to join by and to sort by. Defaults to `"date"`.
#' @param ... Additional arguments forwarded to [join_env()] (e.g. `late =`,
#'   `coalesce_suffixed =`, etc.).
#'
#' @return A tibble containing all joined series, sorted by `by`.
#'
#' @export
#' @examples
#' \dontrun{
#'
#' s1 <- load_all_series()
#' download_fund_data(redownload = TRUE)
#' s2 <- load_all_series(by = "date", late = "ftaw", coalesce_suffixed = c(".y", ".x")) %>%
#'   filter(date >= as_date("2013-01-01"))
#' }
load_all_series <- function(by = "date", ...) {
    stopifnot(is.character(by), length(by) == 1L, nzchar(by))

    run_data_loaders() %>%
        join_env(by = by, ...) %>%
        arrange(.data[[by]])
}

#' Get the internal fund index map
#'
#' Returns the package's fund index lookup table stored in
#' `.fundsr$fund_index_map`.
#'
#' @return A character vector or named list representing the internal
#'   fund index mapping.
#'
#' @export
get_fund_index_map <- function() {
    .fundsr$fund_index_map
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

download_as <- function(named_urls, path = getOption("fundsr.data_dir"), redownload = FALSE) {
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
            message(glue("Skipping download '{file}': already exists."))
        }
    }
}

#' Download fund data according to the configured download list
#'
#' Retrieves all fund data files listed in the `fundsr.fund_urls` option and saves
#' them into the directory specified by `fundsr.data_dir`.
#'
#' @param redownload Logical; if `TRUE`, existing files are overwritten. If
#'   `FALSE`, only missing files are downloaded.
#'
#' @return Invisibly returns `NULL`. Files are written as a side effect.
#'
#' @export
download_fund_data <- function(redownload = FALSE) {
    fund_urls <- getOption("fundsr.fund_urls")
    fund_data_dir <- getOption("fundsr.data_dir")
    download_as(fund_urls, path = fund_data_dir, redownload = redownload)
    invisible(NULL)
}

#' Read a time series file (CSV/TSV) with a date + one or more value columns
#'
#' Loads a delimited file from the directory specified by `fundsr.data_dir`,
#' converts the `date` column to a proper Date, and coerces all other columns to
#' numeric.
#'
#' The reader is chosen by file extension: `.csv` uses [readr::read_csv()] and
#' `.tsv`/`.tab`/`.txt` uses [readr::read_tsv()]. Gzipped variants such as
#' `.csv.gz` and `.tsv.gz` are also supported.
#'
#' @param file Filename to read (relative to `getOption("fundsr.data_dir")`).
#' @param time_unit Character scalar giving the unit of the numeric `date` column
#'   (Unix epoch). One of `"ms"` (default), `"s"`, `"us"`, `"ns"`.
#'
#' @return A tibble with parsed `date` and numeric value columns.
#'
#' @details
#' The function assumes a column named `date` exists and represents a Unix
#' timestamp. All non-`date` columns are coerced with `as.numeric()` (non-parsable
#' values become `NA`).
#'
#' @export
read_timeseries <- function(file, time_unit = c("ms", "s", "us", "ns")) {
    fund_data_dir <- getOption("fundsr.data_dir")
    path <- file.path(fund_data_dir, file)
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
        stop("Unsupported file extension for `file`: expected .csv or .tsv (optionally .gz).", call. = FALSE)
    )
    time_unit <- match.arg(time_unit)
    div <- switch(
        time_unit,
        "s"  = 1,
        "ms" = 1000,
        "us" = 1e6,
        "ns" = 1e9
    )
    df <- reader(path, show_col_types = FALSE)
    df %>%
        mutate(
            date = lubridate::as_date(lubridate::as_datetime(as.numeric(date) / div)),
            across(-date, ~ suppressWarnings(as.numeric(.x)))
        )
}

#' Read an MSCI two-column TSV file
#'
#' Extracts the data portion of an MSCI TSV file—skipping header noise—and reads
#' it as a two-column table containing a date and a numeric value.
#'
#' @param file Filename of the TSV to read (relative to `getOption("fundsr.data_dir")`).
#'
#' @return A tibble with a `Date` column and one numeric column.
#'
#' @details
#' The function filters lines beginning with a digit (date rows) or the literal
#' `"Date"`, then parses them using a fixed `%m/%d/%Y` date format and a numeric
#' second field.
#'
#' @export
read_msci_tsv <- function(file) {
    fund_data_dir <- getOption("fundsr.data_dir")
    lines <- readr::read_lines(file.path(fund_data_dir, file))
    data_lines <- grep("^[0-9]|Date", lines, value = TRUE)
    df <- readr::read_tsv(I(data_lines), col_types = readr::cols(
        readr::col_date(format = "%m/%d/%Y"),
        readr::col_double()
    ))
    df
}

#' Store a cached object in the package storage environment
#'
#' Evaluates an expression and assigns its result into the package storage
#' environment (`.fundsr_storage`) under the given variable name. The value is
#' recomputed only if it is missing or if the global option `fundsr.reload` is
#' set to `TRUE`. Optionally updates the fund/index map.
#'
#' @param var_name Name of the variable to store in `.fundsr_storage`.
#' @param expr An expression to evaluate when (re)computing the value.
#' @param fund_index_map Optional named vector or list of fund/index pairs to add
#'   to `.fundsr$fund_index_map`.
#'
#' @return Invisibly returns `NULL`. Called for side effects.
#'
#' @details
#' The expression `expr` is evaluated in the caller's environment and then stored
#' in `.fundsr_storage`. The `fundsr.reload` option can be used to force
#' recomputation.
#'
#' @export
store_timeseries <- function(var_name, expr, fund_index_map = NULL) {
    # Access the parent's environment (where store_timeseries was called)
    parent_env <- parent.frame()
    # Get global reload flag
    reload <- getOption("fundsr.reload", FALSE)
    # Check if assignment is needed and evaluate expr in parent_env
    if (reload || !exists(var_name, envir = .fundsr_storage)) {
        message(paste("*** Loading:", var_name))
        assign(var_name,
               eval(substitute(expr), envir = parent_env),
               envir = .fundsr_storage)
        # Also add fund index pairs to global map (if supplied)
        if (!is.null(fund_index_map)) {
            .fundsr$fund_index_map[names(fund_index_map)] <- fund_index_map
        }
    }
    invisible(NULL)
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
#' @param data_sheet Sheet index or name containing the NAV data. Defaults to `1`.
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
#'
#' @return Invisibly returns `NULL`. The imported data are stored in
#'   `.fundsr_storage` under `tolower(ticker)`. A fund/index mapping is recorded
#'   in `.fundsr$fund_index_map` when `benchmark` is supplied.
#'
#' @details
#' The function builds a column-translation mapping from the fund NAV column and,
#' if requested, a benchmark column. It then calls `import_xl_data()` to read the
#' Excel file and `store_timeseries()` to cache the imported object under
#' `tolower(ticker)`. When `benchmark` is provided, a corresponding entry is
#' added to `.fundsr$fund_index_map` to link the fund to its benchmark key.
#'
#' @export
load_fund <- function(ticker,
                      file = NULL,
                      data_sheet = 1,
                      date_col = "^Date",
                      nav_col = "^NAV",
                      benchmark = NULL,
                      benchmark_col = NULL,
                      retrieve_benchmark = FALSE,
                      date_order = "dmy") {
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
        ticker_lower,
        import_xl_data(
            xl_file = file.path(fund_data_dir, file),
            data_sheet = data_sheet,
            date_field_name = date_col,
            col_trans = ct,
            date_order = date_order
        ),
        fund_index_map = if (is.null(benchmark)) NULL else set_names(benchmark, ticker_lower)
    )

}

####### Provider-specific wrappers #######

#' Import an MSCI index sheet and register benchmark mappings
#'
#' Wrapper around `store_timeseries()` and `import_xl_data()` for MSCI index files.
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
    fund_data_dir <- getOption("fundsr.data_dir")
    store_timeseries(
        var_name = var_name,
        expr = import_xl_data(
            xl_file = file.path(fund_data_dir, file),
            data_sheet = 1,
            date_field_name = "^Date",
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
