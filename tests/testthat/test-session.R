test_that("sessions isolate state and can share storage", {
    state_a <- new.env(parent = emptyenv())
    storage_shared <- new.env(parent = emptyenv())

    session_a <- fundsr_session(state = state_a, storage = storage_shared)
    session_b <- fundsr_session(storage = storage_shared)

    add_data_loader(function() {
        store_timeseries("foo", tibble::tibble(date = as.Date("2024-01-01"), x = 1), session = session_a)
    }, session = session_a)

    run_data_loaders(session = session_a)

    expect_true(exists("foo", envir = get_storage(session = session_b), inherits = FALSE))
    expect_equal(get_fund_index_map(session = session_b), character())

    add_fund_index_map(c(foo = "IDX"), session = session_a)
    expect_equal(get_fund_index_map(session = session_a), c(foo = "IDX"))
    expect_equal(get_fund_index_map(session = session_b), character())
})

test_that("build_all_series uses the provided session", {
    session <- fundsr_session()

    add_data_loader(function() {
        store_timeseries(
            "a",
            tibble::tibble(date = as.Date("2024-01-01") + 0:1, a = c(1, 2)),
            session = session
        )
    }, session = session)

    add_data_loader(function() {
        store_timeseries(
            "b",
            tibble::tibble(date = as.Date("2024-01-01") + 0:1, b = c(10, 20)),
            session = session
        )
    }, session = session)

    out <- build_all_series(session = session)

    expect_named(out, c("date", "a", "b"))
    expect_equal(nrow(out), 2)
    expect_equal(out$a, c(1, 2))
    expect_equal(out$b, c(10, 20))
})


test_that("clear helpers are no-ops for uninitialised default session", {
    testthat::local_mocked_bindings(
        fundsr_default_session = function() {
            structure(
                list(state = NULL, storage = NULL),
                class = "fundsr_session"
            )
        }
    )

    expect_invisible(clear_data_loaders())
    expect_invisible(clear_fund_index_map())
    expect_invisible(clear_inkscape_queue())
    expect_invisible(clear_storage())
    expect_invisible(clear_storage(clear_map = TRUE))
    expect_invisible(reset_state())
})
