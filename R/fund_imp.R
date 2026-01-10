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
#'
#' @family fund/index workflow functions
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
#'
#' @family fund/index workflow functions
#' @export
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
#'
#' @family fund/index workflow functions
#' @export
#' @examples
#' clear_storage()
#' clear_storage(clear_fund_index_map = TRUE)
clear_storage <- function(clear_fund_index_map = FALSE) {
    if (!is.logical(clear_fund_index_map) ||
            length(clear_fund_index_map) != 1L ||
            is.na(clear_fund_index_map)) {
        stop("`clear_fund_index_map` must be TRUE or FALSE.", call. = FALSE)
    }
    if (!exists(".fundsr_storage", inherits = TRUE) || !is.environment(.fundsr_storage)) {
        stop("Fundsr storage environment is not initialised.", call. = FALSE)
    }
    rm(list = ls(envir = .fundsr_storage, all.names = TRUE), envir = .fundsr_storage)
    if (isTRUE(clear_fund_index_map)) {
        clear_fund_index_map()
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
#' The previous value of option "fundsr.reload" is restored on exit,
#' even if a data loader errors.
#'
#' Data loaders are taken from `.fundsr$data_loaders` and are called
#' sequentially in registration order. Each registered function must take
#' no arguments.
#'
#' @family fund/index workflow functions
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
    old <- fundsr_get_option("reload")
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
#' @param suffixes Character vector of length 2 giving the suffixes to
#'   coalesce, in priority order. For example, `c(".x", ".y")` uses
#'   `.x` as the primary source and falls back to `.y` when `.x` is `NA`.
#'
#' @return A tibble with the same rows as `df`, where any pairs of
#'   suffixed columns (e.g. `name.x` / `name.y`) are replaced by a single
#'   unsuffixed column (e.g. `name`) containing the coalesced values.
#'
#' @keywords internal
coalesce_join_suffixes <- function(df, suffixes = c(".x", ".y")) {
    df  <- tibble::as_tibble(df)
    nms <- names(df)
    sx <- suffixes[1]
    sy <- suffixes[2]

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
#' Performs a `dplyr::full_join()` across all objects in an environment, except those listed in
#' `late`. Objects listed in `late` are instead joined afterwards using `dplyr::left_join()` (or
#' another function specified via `late_join`) in the order given. Column clashes during the full
#' join are resolved via join suffixes `c(".x", ".y")` while the late joins use
#' `c(".early", ".late")`. Optionally, columns with join suffixes can be automatically coalesced
#' into single unsuffixed columns via precedence specification (`join_precedence`).
#'
#' @param env Environment containing the tibbles to join.
#' @param by Character vector of join keys (passed to `dplyr::full_join()`).
#' @param late Character vector of object names in `env` that should be
#'   *excluded* from the initial full join and instead left-joined
#'   afterwards. Defaults to `NULL`.
#' @param join_precedence Optional character vector of length 2 giving join suffixes to coalesce
#'   (for example, `c(".early", ".late")` or `c(".late", ".early")`). When non-`NULL`, any pairs of
#'   columns whose names end in these suffixes (and share the same base name) are replaced by a
#'   single unsuffixed column containing the coalesced values, preferring the left suffix when both
#'   values are available. If `NULL` (the default), no automatic coalescing is performed.
#' @param coalesce_suffixed Deprecated; use `join_precedence`.
#' @param late_join Function to use for joining late objects.
#'
#' @return A tibble: the full join of all non-late objects, followed by sequential left-joins of the
#'   late objects. If `join_precedence` is supplied, suffixed join columns are coalesced into
#'   unsuffixed base columns as described above.
#'
#' @family fund/index workflow functions
#' @export
#'
#' @examples
#'   e <- new.env()
#'   e$members <- dplyr::band_members
#'   e$instruments <- dplyr::band_instruments
#'   e$other_instr <- dplyr::band_instruments |>
#'       dplyr::mutate(plays = c("banjo", "mellotron", "harpsichord"))
#'
#'   full <- join_env(e, by = "name")
#'   late <- join_env(e, by = "name", late = "other_instr")
#'   late_coalesced <- join_env(e,
#'                              by = "name",
#'                              late = "other_instr",
#'                              join_precedence = c(".late", ".early"))
#'   print(list(full = full, late = late, late_coalesced = late_coalesced))
join_env <- function(env,
                     by = "date",
                     late = NULL,
                     join_precedence = NULL,
                     coalesce_suffixed = NULL, # Deprecated; equivalent to above
                     late_join = dplyr::left_join) {
    # Deprecated param handling
    if (!missing(coalesce_suffixed) && !is.null(coalesce_suffixed)) {
        lifecycle::deprecate_warn(
            when = "0.2.1",
            what = "join_env(coalesce_suffixed)",
            with = "join_env(join_precedence)"
        )
        if (!missing(join_precedence)) {
            stop("Use only one of `join_precedence` or deprecated `coalesce_suffixed`.",
                 call. = FALSE)
        }
        join_precedence <- coalesce_suffixed
    }
    # / Deprecated param handling

    stopifnot(is.environment(env))
    by <- as.character(by)
    stopifnot(length(by) >= 1L, all(nzchar(by)))

    obj_names <- ls(envir = env, sorted = FALSE)
    raw_late <- late %||% character(0)
    fundsr_msg(glue("Joining: {glue::glue_collapse(obj_names, sep = ', ')}"), level = 2L)

    objs <- mget(obj_names, envir = env)
    not_df <- purrr::map_lgl(objs, ~ !is.data.frame(.x))
    if (any(not_df)) {
        offenders <- names(objs)[not_df]
        stop(
            "Non-joinable object(s) in `env` (expected data.frame/tibble): ",
            paste(offenders, collapse = ", "),
            call. = FALSE
        )
    }
    missing_by <- purrr::imap_lgl(objs, ~ !all(by %in% names(.x)))
    if (any(missing_by)) {
        offenders <- names(objs)[missing_by]
        stop(
            "Join key(s) missing from: ",
            paste(offenders, collapse = ", "),
            ". Required key(s): ",
            paste(by, collapse = ", "),
            call. = FALSE
        )
    }
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

    j <- purrr::reduce(main_list, full_join, by = by, suffix = c(".x", ".y"))
    if (length(late) > 0L) {
        late_list <- mget(late, envir = env)
        j <- purrr::reduce(late_list, late_join, .init = j, by = by, suffix = c(".early", ".late"))
    }
    if (!is.null(join_precedence))
        j <- coalesce_join_suffixes(j, join_precedence)

    j
}

#' Run all registered data loaders and join all loaded series into a big tibble.
#'
#' Runs [run_data_loaders()], joins the resulting environment into a single data
#' frame via [join_env()], and sorts the result by `by`.
#'
#' This function is a convenience wrapper for the most common workflow.
#'
#' @param reload Logical; if `TRUE`, forces a full reload by temporarily setting
#'   `options(fundsr.reload = TRUE)`.
#' @param by Column name to join by and to sort by.
#' @param ... Additional arguments forwarded to [join_env()] (e.g. `late =`,
#'   `join_precedence =`, etc.).
#'
#' @return A tibble containing all joined series, sorted by `by`.
#'
#' @family fund/index workflow functions
#' @export
#' @examples
#' \dontrun{
#'
#' s1 <- build_all_series()
#' download_fund_data(redownload = TRUE)
#' s2 <- build_all_series(by = "date", late = "ftaw", join_precedence = c(".y", ".x")) %>%
#'   filter(date >= as_date("2013-01-01"))
#' }
build_all_series <- function(reload = FALSE, by = "date", ...) {
    stopifnot(is.character(by), length(by) == 1L, nzchar(by))

    run_data_loaders(reload = reload) %>%
        join_env(by = by, ...) %>%
        arrange(.data[[by]])
}

#' Get the internal fund storage
#'
#' Returns the fundsr's fund storage environment
#' (`.fundsr_storage`).
#'
#' @return The storage environment.
#'
#' @family fund/index workflow functions
#' @export
get_storage <- function() {
    .fundsr_storage
}

#' Store a cached object in the package storage environment
#'
#' Evaluate an expression and cache its result in the package storage
#' environment (`.fundsr_storage`) under a given name. The expression is only
#' re-evaluated when the cached value is missing, when `overwrite = TRUE`, or
#' when the global option `fundsr.reload` is `TRUE`. Optionally merges additional
#' fund/index mappings into `.fundsr$fund_index_map`.
#'
#' @param var_name Character scalar. Name of the variable to store in
#'   `.fundsr_storage`.
#' @param expr An expression. Evaluated in the caller's environment when
#'   (re)computing the cached value.
#' @param fund_index_map Optional named vector or list. Fund/index pairs to merge
#'   into `.fundsr$fund_index_map`. Names are used as keys.
#' @param overwrite Logical scalar. If `TRUE`, recompute and replace any existing
#'   cached value, regardless of `fundsr.reload`.
#'
#' @return Invisibly returns `NULL` (called for its side effects).
#'
#' @details
#' `expr` is evaluated in the environment where `store_timeseries()` is called
#' (i.e. the caller's environment), then assigned into `.fundsr_storage` under
#' `var_name`.
#'
#' Caching behavior is controlled by:
#' \itemize{
#'   \item `overwrite = TRUE` (always recompute),
#'   \item `options(fundsr.reload = TRUE)` (force recomputation globally), or
#'   \item absence of `var_name` in `.fundsr_storage` (compute once).
#' }
#'
#' If `fund_index_map` is supplied, it is merged into `.fundsr$fund_index_map`
#' via name-based assignment: existing entries with the same names are replaced.
#'
#' @family fund/index workflow functions
#' @export
store_timeseries <- function(var_name, expr, fund_index_map = NULL, overwrite = FALSE) {
    # Access the parent's environment (where store_timeseries was called)
    parent_env <- parent.frame()
    # Get global reload flag
    reload <- fundsr_get_option("reload")
    # Check if assignment is needed and evaluate expr in parent_env
    if (overwrite || reload || !exists(var_name, envir = .fundsr_storage)) {
        fundsr_msg(paste("*** Loading:", var_name), level = 2L)
        assign(var_name,
               eval(substitute(expr), envir = parent_env),
               envir = .fundsr_storage)
        # Also add fund index pairs to global map (if supplied)
        if (!is.null(fund_index_map)) {
            add_fund_index_map(fund_index_map)
        }
    }
    invisible(NULL)
}
