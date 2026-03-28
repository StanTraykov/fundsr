test_that("state-touching non-ellipsis wrappers expose session", {
    expect_true("session" %in% names(formals(save_plot)))
    expect_true("session" %in% names(formals(export_pngs)))
    expect_true("session" %in% names(formals(run_plots)))
})

test_that("import_fund stays ellipsis-compatible for session forwarding", {
    expect_false("session" %in% names(formals(import_fund)))
})
