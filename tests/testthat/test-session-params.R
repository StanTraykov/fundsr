test_that("functions forwarding state-sensitive calls expose session when needed", {
    expect_true("session" %in% names(formals(save_plot)))
    expect_true("session" %in% names(formals(export_pngs)))
    expect_true("session" %in% names(formals(run_plots)))

    expect_true("session" %in% names(formals(msci)))
    expect_true("session" %in% names(formals(load_fund)))
})

test_that("provider wrappers stay compatible via ellipsis forwarding", {
    wrappers <- list(ishs, spdr, xtra, amun, inve, vang, ubs, hsbc, bnpp, avan)
    has_session <- vapply(wrappers, function(f) "session" %in% names(formals(f)), logical(1))
    expect_false(any(has_session))
})
