test_that("roll_diffs works for 2 indices + 4 funds", {
    withr::local_seed(1)

    n_days <- 365L
    n_years <- 2L
    n <- n_days * n_years
    date <- seq.Date(from = as.Date("2024-01-01"), by = "day", length.out = n)

    # --- Index LEVELS (additive random walk) ---
    drift1 <- 0.02 / n_days
    drift2 <- 0.018 / n_days

    idx1 <- 100 + cumsum(drift1 + rnorm(n, sd = 0.30))
    idx2 <- 100 + cumsum(drift2 + rnorm(n, sd = 0.32))

    # keep positive (needed for log diffs)
    idx1 <- pmax(idx1, 1)
    idx2 <- pmax(idx2, 1)

    # --- Funds tracking indices (still LEVELS) ---
    mk_fund <- function(idx, drift_extra = 0, te_sd = 0.12, noise_sd = 0.6) {
        # persistent tracking error (walk)
        te <- cumsum(drift_extra + rnorm(n, sd = te_sd))
        y <- idx + te + rnorm(n, sd = noise_sd)
        pmax(y, 1)
    }

    f1 <- mk_fund(idx1, drift_extra = -0.0010 / n_days, te_sd = 0.10, noise_sd = 0.50)
    f2 <- mk_fund(idx1, drift_extra = -0.0020 / n_days, te_sd = 0.14, noise_sd = 0.65)

    g1 <- mk_fund(idx2, drift_extra = -0.0015 / n_days, te_sd = 0.11, noise_sd = 0.55)
    g2 <- mk_fund(idx2, drift_extra = -0.0025 / n_days, te_sd = 0.15, noise_sd = 0.70)

    df <- tibble::tibble(
        date = date,
        IDX1 = idx1,
        IDX2 = idx2,
        f1 = f1,
        f2 = f2,
        g1 = g1,
        g2 = g2
    )

    fund_index_map <- c(
        f1 = "IDX1",
        f2 = "IDX1",
        g1 = "IDX2",
        g2 = "IDX2"
    )

    out <- roll_diffs(
        df = df,
        n_days = n_days,
        fund_index_map = fund_index_map,
        date_col = "date",
        index_level = "net",
        messages = character()
    )

    # --- Snapshot 10 NA rows + 100 non-NA rows (tail) ---
    nn <- nrow(out$log)

    i_na <- seq.int(1L, min(10L, nn))
    i_non <- seq.int(max(1L, nn - 100L + 1L), nn)

    snap <- list(
        log_na = out$log[i_na, , drop = FALSE],
        cagr_na = out$cagr[i_na, , drop = FALSE],
        log_tail = out$log[i_non, , drop = FALSE],
        cagr_tail = out$cagr[i_non, , drop = FALSE]
    )

    snap <- purrr::map(
        snap,
        function(x) dplyr::mutate(x, dplyr::across(where(is.numeric), function(v) round(v, 10)))
    )

    testthat::expect_snapshot(snap)

    # --- Assertions: structure + basic correctness ---
    expect_true(is.list(out))
    expect_named(out, c("cagr", "log"))
    expect_s3_class(out$cagr, "data.frame")
    expect_s3_class(out$log, "data.frame")

    expect_named(out$cagr, c("date", "f1", "f2", "g1", "g2"))
    expect_named(out$log, c("date", "f1", "f2", "g1", "g2"))

    expect_identical(out$cagr$date, df$date)
    expect_identical(out$log$date, df$date)

    # At least some early rows should be NA (window lookback)
    expect_true(all(is.na(out$log$f1[1:10])))
    expect_true(all(is.na(out$cagr$f1[1:10])))

    # Later on we should have finite values (as there are no holes in the data)
    later <- (n_days + 20L):n
    expect_true(all(is.finite(out$log$f1[later])))
    expect_true(all(is.finite(out$cagr$f1[later])))

    # Sanity: not exploding to absurd magnitudes
    expect_lt(max(abs(out$log$f1[later]), na.rm = TRUE), 10)
    expect_lt(max(abs(out$cagr$f1[later]), na.rm = TRUE), 10)
})
