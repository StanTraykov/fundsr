#' Get the internal fund storage
#'
#' Returns the fundsr's fund storage environment
#' (`session$storage`).
#'
#' @param session Optional `fundsr_session` object. Defaults to the package
#'   default session when `NULL`.
#'
#' @return The storage environment.
#'
#' @family fund/index workflow functions
#' @export
get_storage <- function(session = NULL) {
    fundsr_require_state(storage = TRUE, session = session)$storage
}

#' Clear storage
#'
#' Removes all objects from the package's storage environment
#' (`session$storage`). Optionally also clears the fund-index map
#' (`session$state$fund_index_map`).
#'
#' @param clear_map Logical scalar; if `TRUE`, also clears
#'   `session$state$fund_index_map`.
#' @param session Optional `fundsr_session` object. Defaults to the package
#'   default session when `NULL`.
#'
#' @return Invisibly returns `NULL`. Called for side effects.
#'
#' @family fund/index workflow functions
#' @export
#' @examples
#' clear_storage()
#' clear_storage(clear_map = TRUE)
clear_storage <- function(clear_map = FALSE, session = NULL) {
    check_logical(clear_map)
    session <- fundsr_get_session(session)

    if (clear_map) {
        clear_fund_index_map(session = session)
    }

    storage <- session$storage
    if (!is.environment(storage)) {
        return(invisible(NULL))
    }

    objs <- ls(envir = storage, all.names = TRUE)
    if (length(objs)) {
        tryCatch(
            rm(list = objs, envir = storage),
            error = function(e) {
                fundsr_abort(
                    msg    = "Failed to clear fundsr storage.",
                    class  = "fundsr_bad_state",
                    parent = e
                )
            }
        )
    }
    invisible(NULL)
}

#' Store a cached object in the package storage environment
#'
#' Evaluate an expression and cache its result in the package storage
#' environment (`session$storage`) under a given name. The expression is only
#' re-evaluated when the cached value is missing, when `overwrite = TRUE`, or
#' when the global option `fundsr.reload` is `TRUE`. Optionally merges additional
#' fund/index mappings into `session$state$fund_index_map`.
#'
#' @param var_name Character scalar. Name of the variable to store in
#'   `session$storage`.
#' @param expr An expression. Evaluated in the caller's environment when
#'   (re)computing the cached value.
#' @param fund_index_map Optional named vector of fund/index pairs to merge
#'   into `session$state$fund_index_map`. Names are used as keys, values should be indices.
#' @param overwrite Logical scalar. If `TRUE`, recompute and replace any existing
#'   cached value, regardless of `fundsr.reload`.
#' @param postprocess Function applied to the computed value before caching.
#'   Only used when the value is (re)computed (i.e. not applied when a cached
#'   value is reused). Defaults to [base::identity()].
#' @param session Optional `fundsr_session` object. Defaults to the package
#'   default session when `NULL`.
#'
#' @return Invisibly returns `NULL` (called for its side effects).
#'
#' @details
#' `expr` is evaluated in the environment where `store_timeseries()` is called
#' (i.e. the caller's environment), then assigned into `session$storage` under
#' `var_name`.
#'
#' Caching behavior is controlled by:
#' \itemize{
#'   \item `overwrite = TRUE` (always recompute),
#'   \item `options(fundsr.reload = TRUE)` (force recomputation globally), or
#'   \item absence of `var_name` in `session$storage` (compute once).
#' }
#'
#' If `fund_index_map` is supplied, it is merged into `session$state$fund_index_map`
#' via name-based assignment: existing entries with the same names are replaced.
#'
#' @family fund/index workflow functions
#' @export
store_timeseries <- function(var_name,
                             expr,
                             fund_index_map = NULL,
                             overwrite = FALSE,
                             postprocess = identity,
                             session = NULL) {
    check_string(var_name)
    check_logical(overwrite)
    if (!is.function(postprocess)) {
        stop_bad_arg("postprocess", "must be a function.")
    }
    # Access the parent's environment (where store_timeseries was called)
    parent_env <- parent.frame()
    reload <- isTRUE(fundsr_get_option("reload"))
    storage <- fundsr_require_state(storage = TRUE, session = session)$storage

    needs_eval <- overwrite || reload || !exists(var_name, envir = storage)
    # Check if assignment is needed and evaluate expr in parent_env
    if (needs_eval) {
        fundsr_msg(paste("*** Loading:", var_name), level = 2L)
        value <- eval(substitute(expr), envir = parent_env)
        value <- postprocess(value)
        assign(var_name, value, envir = storage)
    }
    # Also add fund index pairs to global map (if supplied)
    if (!is.null(fund_index_map)) {
        add_fund_index_map(fund_index_map, session = session)
    }
    invisible(NULL)
}

#' Coalesce suffixed join columns into unsuffixed base columns
#'
#' Helper for post-processing join results that use suffixes such as
#' `.x` / `.y`. For each base name that appears with both suffixes
#' (e.g. `FTAW.x` and `FTAW.y`), this function creates a new column
#' `FTAW` as `dplyr::coalesce(FTAW.x, FTAW.y)` and drops the suffixed
#' columns. The order of `suffixes` controls which column is preferred.
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
    check_string(suffixes, n = 2)
    if (identical(suffixes[1], suffixes[2])) {
        stop_bad_arg("suffixes", "must be two different suffixes.")
    }
    df  <- tibble::as_tibble(df)
    nms <- names(df)
    sx <- suffixes[1]
    sy <- suffixes[2]

    # find base names that have both .x and .y variants
    base_x <- sub(paste0("\\Q", sx, "\\E$"), "", nms[endsWith(nms, sx)])
    base_y <- sub(paste0("\\Q", sy, "\\E$"), "", nms[endsWith(nms, sy)])
    base   <- intersect(base_x, base_y)

    if (length(base) == 0L) return(df)
    conflicts <- intersect(base, nms)
    if (length(conflicts) > 0L) {
        ex <- paste(conflicts, collapse = ", ")
        fundsr_abort(
            msg   = glue::glue("Cannot coalesce: would overwrite existing column(s): {ex}."),
            class = "fundsr_join_conflict"
        )

    }

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

#' Join all data frames in an environment with optional late joins
#'
#' Performs a `dplyr::full_join()` across all objects in `env` (in alphabetical order), excluding
#' any listed in `late`. Late objects are then joined sequentially (via `dplyr::left_join()` by
#' default) in the order given. Full-join clashes use suffixes `c(".x", ".y")`; late joins use
#' `c(".early", ".late")`.
#'
#' Optionally, column pairs with specified suffixes can be coalesced into unsuffixed base columns
#' via `join_precedence`.
#'
#' @param env Environment containing *only* data frames (incl. tibbles) to join.
#' @param by Character vector of join keys (passed to `dplyr::full_join()`).
#' @param late Character vector of object names in `env` that should be
#'   *excluded* from the initial full join and instead left-joined
#'   afterwards. Defaults to `NULL`.
#' @param join_precedence Optional character vector of length 2 giving join suffixes to coalesce
#'   (for example, `c(".early", ".late")` or `c(".late", ".early")`). When non-`NULL`, any pairs of
#'   columns whose names end in these suffixes (and share the same base name) are replaced by a
#'   single unsuffixed column containing the coalesced values, preferring the left suffix when both
#'   values are available. If `NULL` (the default), no automatic coalescing is performed.
#' @param coalesce_suffixed Deprecated; use `join_precedence` (same meaning).
#' @param late_join Function to use for joining late objects, e.g. `dplyr::left_join` (the default).
#'   Must accept dplyr-style `suffix` and `by` arguments.
#'
#' @return A tibble: the full join of all non-late objects, followed by sequential left-joins (or
#'   other joins specified by `late_join`) of the late objects. If `join_precedence` is supplied,
#'   suffixed join columns are coalesced into unsuffixed base columns as described above.
#'
#' @family fund/index workflow functions
#' @export
#'
#' @examples
#'   e <- new.env()
#'   e$members <- dplyr::band_members
#'   e$instruments <- dplyr::band_instruments
#'   e$other_instr <- dplyr::band_instruments |>
#'       dplyr::mutate(plays = dplyr::case_match(name,
#'                                               "John" ~ "banjo",
#'                                               "Paul" ~ "mellotron",
#'                                               "Keith" ~ "harpsichord")) |>
#'       dplyr::add_row(name = "Mick", plays = "harmonica") |>
#'       dplyr::add_row(name = "Stu", plays = "piano")
#'
#'   full <- join_env(e, by = "name")
#'   late <- join_env(e, by = "name", late = "other_instr")
#'   late_coalesced <- join_env(e,
#'                              by = "name",
#'                              late = "other_instr",
#'                              join_precedence = c(".early", ".late"))
#'   print(list(full = full, late = late, late_coalesced = late_coalesced))
join_env <- function(env,
                     by = "date",
                     late = NULL,
                     join_precedence = NULL,
                     coalesce_suffixed = deprecated(), # equivalent to join_precedence
                     late_join = dplyr::left_join) {
    # Deprecated param handling
    if (lifecycle::is_present(coalesce_suffixed)) {
        lifecycle::deprecate_warn(
            when = "0.2.1",
            what = "join_env(coalesce_suffixed)",
            with = "join_env(join_precedence)"
        )
        if (!missing(join_precedence)) {
            stop_bad_arg(
                "coalesce_suffixed",
                paste("(deprecated) cannot be used together with `join_precedence`",
                      "(use only the latter).")
            )
        }
        join_precedence <- coalesce_suffixed
    }
    # / Deprecated param handling
    if (!is.environment(env)) {
        stop_bad_arg("env", "must be an environment.")
    }
    check_string(by, min_n = 1)
    check_string(late, allow_null = TRUE, min_n = 0)
    check_string(join_precedence, allow_null = TRUE, n = 2)
    if (!is.function(late_join)) {
        stop_bad_arg("late_join", "must be a function.")
    }

    obj_names <- ls(envir = env, sorted = TRUE)
    raw_late <- late %||% character(0)
    objs <- mget(obj_names, envir = env)
    not_df <- purrr::map_lgl(objs, ~ !is.data.frame(.x))
    if (any(not_df)) {
        offenders <- names(objs)[not_df]
        stop_bad_arg(
            "env",
            paste0(
                "must contain only data frames (incl. tibbles). Non-joinable object(s): ",
                paste(offenders, collapse = ", "),
                "."
            )
        )
    }
    missing_by <- purrr::imap_lgl(objs, ~ !all(by %in% names(.x)))
    if (any(missing_by)) {
        offenders <- names(objs)[missing_by]
        stop_bad_arg(
            "env",
            paste0(
                "contains data frame(s) missing join key(s). Offenders: ",
                paste(offenders, collapse = ", "),
                ". Required key(s): ",
                paste(by, collapse = ", "),
                "."
            )
        )
    }
    missing_late <- setdiff(raw_late, obj_names)
    if (length(missing_late)) {
        fundsr_warn(paste("Objects not found in `env` and ignored in `late`:",
                          paste(missing_late, collapse = ", ")))
    }
    late <- intersect(raw_late, obj_names)
    main_names <- setdiff(obj_names, late)
    if (length(main_names) == 0L) {
        stop_bad_arg(
            "late",
            "excludes all objects in `env`; no objects left to full_join."
        )
    }
    main_list <- mget(main_names, envir = env)
    main_chr <- glue::glue_collapse(main_names, sep = ", ")
    late_chr <- ""
    if (length(late) > 0L) {
        late_chr <- glue(" (late = {glue::glue_collapse(late, sep = ', ')})")
    }
    fundsr_msg(glue("Joining: {main_chr}{late_chr}."), level = 2L)

    j <- purrr::reduce(main_list, full_join, by = by, suffix = c(".x", ".y"))
    if (length(late) > 0L) {
        late_list <- mget(late, envir = env)
        j <- purrr::reduce(late_list, late_join, .init = j, by = by, suffix = c(".early", ".late"))
    }
    if (!is.null(join_precedence)) {
        j <- coalesce_join_suffixes(j, join_precedence)
    }
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
#' @param by Character vector of column names to join by and sort by.
#' @param ... Additional arguments forwarded to [join_env()] (e.g. `late`,
#'   `join_precedence`, etc.).
#' @param session Optional `fundsr_session` object. Defaults to the package
#'   default session when `NULL`.
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
build_all_series <- function(reload = FALSE, by = "date", session = NULL, ...) {
    check_string(by, min_n = 1)
    run_data_loaders(reload = reload, session = session) %>%
        join_env(by = by, ...) %>%
        arrange(across(all_of(by)))
}
