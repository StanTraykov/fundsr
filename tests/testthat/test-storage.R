test_that("get_storage returns the session storage environment", {
    session <- fundsr_session()

    storage <- get_storage(session = session)

    expect_true(is.environment(storage))
    expect_identical(storage, session$storage)
})

test_that("clear_storage removes storage objects and optionally clears map", {
    session <- fundsr_session()
    session$storage$a <- 1
    session$storage$b <- 2
    add_fund_index_map(c(fund_a = "IDX_A"), session = session)

    expect_invisible(clear_storage(session = session))
    expect_equal(ls(session$storage, all.names = TRUE), character())
    expect_equal(get_fund_index_map(session = session), c(fund_a = "IDX_A"))

    add_fund_index_map(c(fund_b = "IDX_B"), session = session)
    expect_invisible(clear_storage(clear_map = TRUE, session = session))
    expect_equal(get_fund_index_map(session = session), character())
})

test_that("clear_storage is robust to malformed session storage and locked bindings", {
    bad_session <- structure(
        list(state = new.env(parent = emptyenv()), storage = NULL),
        class = "fundsr_session"
    )
    expect_invisible(clear_storage(session = bad_session))

    session <- fundsr_session()
    assign("locked", 1, envir = session$storage)
    lockBinding("locked", session$storage)

    expect_invisible(clear_storage(session = session))
    expect_equal(ls(session$storage, all.names = TRUE), character())
})

test_that("store_timeseries caches, overwrites, and evaluates in caller environment", {
    session <- fundsr_session()
    counter <- 0L

    compute_value <- function(multiplier = 1L) {
        counter <<- counter + 1L
        10L * multiplier
    }

    store_timeseries("value", compute_value(), session = session)
    expect_equal(get("value", envir = session$storage), 10L)
    expect_equal(counter, 1L)

    store_timeseries("value", compute_value(2L), session = session)
    expect_equal(get("value", envir = session$storage), 10L)
    expect_equal(counter, 1L)

    store_timeseries("value", compute_value(3L), overwrite = TRUE, session = session)
    expect_equal(get("value", envir = session$storage), 30L)
    expect_equal(counter, 2L)

    withr::local_options(list(fundsr.reload = TRUE))
    store_timeseries("value", compute_value(4L), session = session)
    expect_equal(get("value", envir = session$storage), 40L)
    expect_equal(counter, 3L)
})

test_that("store_timeseries applies postprocess only on recompute and updates fund-index map", {
    session <- fundsr_session()
    post_counter <- 0L

    post <- function(x) {
        post_counter <<- post_counter + 1L
        x + 1L
    }

    store_timeseries(
        "series",
        5L,
        postprocess = post,
        fund_index_map = c(series = "SXXP"),
        session = session
    )
    expect_equal(get("series", envir = session$storage), 6L)
    expect_equal(post_counter, 1L)
    expect_equal(get_fund_index_map(session = session), c(series = "SXXP"))

    store_timeseries("series", 99L, postprocess = post, session = session)
    expect_equal(get("series", envir = session$storage), 6L)
    expect_equal(post_counter, 1L)

    expect_error(
        store_timeseries("series", 1L, postprocess = 1, session = session),
        class = "fundsr_bad_arg"
    )
})

test_that("coalesce_join_suffixes coalesces pairs, validates suffixes, and prevents conflicts", {
    df <- tibble::tibble(
        id = 1:3,
        value.x = c(1, NA, 3),
        value.y = c(10, 20, NA),
        keep = letters[1:3]
    )

    out <- fundsr:::coalesce_join_suffixes(df, suffixes = c(".x", ".y"))
    expect_named(out, c("id", "keep", "value"))
    expect_equal(out$value, c(1, 20, 3))

    reversed <- fundsr:::coalesce_join_suffixes(df, suffixes = c(".y", ".x"))
    expect_equal(reversed$value, c(10, 20, 3))

    unchanged <- fundsr:::coalesce_join_suffixes(tibble::tibble(a = 1, b = 2))
    expect_identical(unchanged, tibble::tibble(a = 1, b = 2))

    expect_error(
        fundsr:::coalesce_join_suffixes(df, suffixes = c(".x", ".x")),
        class = "fundsr_bad_arg"
    )

    conflict_df <- tibble::tibble(
        value = 99,
        value.x = 1,
        value.y = 2
    )
    expect_error(
        fundsr:::coalesce_join_suffixes(conflict_df),
        class = "fundsr_join_conflict"
    )
})

test_that("join_env joins frames with late stage behavior and coalescing", {
    env <- new.env(parent = emptyenv())
    env$b <- tibble::tibble(
        date = as.Date(c("2024-01-01", "2024-01-02")),
        x = c(1, 2)
    )
    env$a <- tibble::tibble(
        date = as.Date(c("2024-01-02", "2024-01-03")),
        y = c("a", "b")
    )
    env$late <- tibble::tibble(
        date = as.Date(c("2024-01-01", "2024-01-03", "2024-01-04")),
        x = c(NA, 3, 4)
    )

    out <- join_env(env, by = "date", late = "late", join_precedence = c(".early", ".late"))

    expect_s3_class(out, "tbl_df")
    expect_named(out, c("date", "y", "x"))
    expect_equal(out$date, as.Date(c("2024-01-02", "2024-01-03", "2024-01-01")))
    expect_equal(out$x, c(2, 3, 1))

    expect_warning(
        join_env(env, by = "date", late = c("late", "missing")),
        "Objects not found"
    )
})

test_that("join_env validates inputs and deprecated argument behavior", {
    env <- new.env(parent = emptyenv())
    env$a <- tibble::tibble(date = as.Date("2024-01-01"), x = 1)
    env$b <- tibble::tibble(date = as.Date("2024-01-01"), x = NA_real_)

    expect_error(join_env(list(), by = "date"), class = "fundsr_bad_arg")

    env_bad <- new.env(parent = emptyenv())
    env_bad$a <- tibble::tibble(date = as.Date("2024-01-01"), x = 1)
    env_bad$b <- 1
    expect_error(join_env(env_bad, by = "date"), class = "fundsr_bad_arg")

    env_missing <- new.env(parent = emptyenv())
    env_missing$a <- tibble::tibble(date = as.Date("2024-01-01"), x = 1)
    env_missing$b <- tibble::tibble(other = 1)
    expect_error(join_env(env_missing, by = "date"), class = "fundsr_bad_arg")

    expect_error(join_env(env, by = "date", late = c("a", "b")), class = "fundsr_bad_arg")
    expect_error(join_env(env, by = "date", late_join = 1), class = "fundsr_bad_arg")

    expect_warning(
        deprecated <- join_env(env, by = "date", coalesce_suffixed = c(".x", ".y")),
        class = "lifecycle_warning_deprecated"
    )
    expect_equal(deprecated$x, 1)

    expect_error(
        suppressWarnings(
            join_env(
                env,
                by = "date",
                join_precedence = c(".x", ".y"),
                coalesce_suffixed = c(".x", ".y")
            )
        ),
        class = "fundsr_bad_arg"
    )
})

test_that("build_all_series runs loaders and returns sorted joined output", {
    session <- fundsr_session()

    add_data_loader(function() {
        store_timeseries(
            "z",
            tibble::tibble(
                date = as.Date(c("2024-01-03", "2024-01-01")),
                z = c(3, 1)
            ),
            session = session
        )
    }, session = session)

    add_data_loader(function() {
        store_timeseries(
            "a",
            tibble::tibble(
                date = as.Date(c("2024-01-01", "2024-01-02")),
                a = c(10, 20)
            ),
            session = session
        )
    }, session = session)

    out <- build_all_series(
        by = "date",
        late = "z",
        join_precedence = c(".early", ".late"),
        session = session
    )

    expect_equal(out$date, sort(out$date))
    expect_named(out, c("date", "a", "z"))
    expect_equal(out$z, c(1, NA))
})
