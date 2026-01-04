library(tidyverse)

set.seed(42)

n_days  <- 365L
n_years <- 2L
n <- n_days * n_years
date <- seq.Date(from = as.Date("2024-01-01"), by = "day", length.out = n)

mk_index_pair <- function(mu1_ann = 0.075, mu2_ann = 0.073,
                          vol_ann = 0.16,
                          corr = 0.995,
                          start = 100) {

    mu1_d <- mu1_ann / 365
    mu2_d <- mu2_ann / 365
    vol_d <- vol_ann / sqrt(365)

    z_common <- rnorm(n)
    z_idio   <- rnorm(n)

    r1 <- mu1_d + vol_d * z_common
    r2 <- mu2_d + vol_d * (corr * z_common + sqrt(1 - corr^2) * z_idio)

    list(
        idx1 = list(level = start * exp(cumsum(r1)), ret = r1),
        idx2 = list(level = start * exp(cumsum(r2)), ret = r2)
    )
}

mk_spread <- function(mu_ann = 0.0015, sd_ann = 0.00018, phi = 0.85,
                      reanchor_every = 45L, reanchor_strength = 0.85,
                      shift_at = integer(0), shift_ann = numeric(0)) {
    mu_d <- mu_ann / 365
    sd_d <- sd_ann / sqrt(365)

    e <- rnorm(n, 0, sd_d)
    s <- numeric(n)

    for (i in seq_len(n)) {
        prev <- if (i == 1) mu_d else s[i - 1]
        s_i  <- mu_d + phi * (prev - mu_d) + e[i]

        if (reanchor_every > 0L && i %% reanchor_every == 0L) {
            s_i <- s_i - reanchor_strength * (s_i - mu_d)
        }

        s[i] <- s_i
    }

    if (length(shift_at)) {
        for (k in seq_along(shift_at)) {
            s[shift_at[k]:n] <- s[shift_at[k]:n] + (shift_ann[k] / 365)
        }
    }

    s
}

mk_fund <- function(idx_ret,
                    spread_mu_ann = 0.0015,
                    spread_sd_ann = 0.00018,
                    spread_phi    = 0.85,
                    idio_vol_ann  = 0.0025,
                    start_level   = 100,
                    start_lag_days = 0L,
                    reanchor_every = 45L,
                    reanchor_strength = 0.85,
                    shift_at = integer(0),
                    shift_ann = numeric(0)) {

    idio_sd_d <- idio_vol_ann / sqrt(365)
    spread <- mk_spread(
        mu_ann = spread_mu_ann,
        sd_ann = spread_sd_ann,
        phi    = spread_phi,
        reanchor_every = reanchor_every,
        reanchor_strength = reanchor_strength,
        shift_at = shift_at,
        shift_ann = shift_ann
    )

    r <- idx_ret + spread + rnorm(n, 0, idio_sd_d)
    lvl <- start_level * exp(cumsum(r))

    if (start_lag_days > 0L) {
        lvl[seq_len(start_lag_days)] <- NA_real_
    }

    lvl
}

mk_gross_bonus <- function(start_ann = 0.0040, end_ann = 0.0050,
                           wiggle_sd_ann = 0.00035, wiggle_phi = 0.985,
                           lo_ann = 0.0035, hi_ann = 0.0060) {
    trend <- seq(from = start_ann, to = end_ann, length.out = n) / 365

    sd_d <- wiggle_sd_ann / sqrt(365)
    e <- rnorm(n, 0, sd_d)
    w <- numeric(n)
    for (i in seq_len(n)) {
        prev <- if (i == 1) 0 else w[i - 1]
        w[i] <- wiggle_phi * prev + e[i]
    }

    b <- trend + w
    b <- pmin(pmax(b, lo_ann / 365), hi_ann / 365)
    b
}

# --- Indices (very close pair) ---
pair <- mk_index_pair(mu1_ann = 0.075, mu2_ann = 0.073, vol_ann = 0.16, corr = 0.96, start = 100)

idx1 <- pair$idx1
idx2 <- pair$idx2

IDX1 <- idx1$level
IDX2 <- idx2$level

# --- Gross indices: drift + wiggle, strictly above net ---
bonus1 <- mk_gross_bonus(start_ann = 0.0040, end_ann = 0.0050, wiggle_sd_ann = 0.00035, wiggle_phi = 0.985,
                         lo_ann = 0.0035, hi_ann = 0.0060)
bonus2 <- mk_gross_bonus(start_ann = 0.0041, end_ann = 0.0051, wiggle_sd_ann = 0.00035, wiggle_phi = 0.985,
                         lo_ann = 0.0035, hi_ann = 0.0060)

IDX1_GR <- IDX1[1] * exp(cumsum(idx1$ret + bonus1))
IDX2_GR <- IDX2[1] * exp(cumsum(idx2$ret + bonus2))

# --- Funds: tight tracking; almost never beat gross ---
# gross ~ 40–50 bps/yr above net, so keep funds ~ 10–25 bps/yr above net
f1 <- mk_fund(idx1$ret,
              spread_mu_ann = 0.0018, spread_sd_ann = 0.00016, spread_phi = 0.85, idio_vol_ann = 0.0023)

f2 <- mk_fund(idx1$ret,
              spread_mu_ann = 0.0012, spread_sd_ann = 0.00018, spread_phi = 0.85, idio_vol_ann = 0.0026,
              start_lag_days = 30L)

g1 <- mk_fund(idx2$ret,
              spread_mu_ann = 0.0016, spread_sd_ann = 0.00016, spread_phi = 0.85, idio_vol_ann = 0.0024)

g2 <- mk_fund(idx2$ret,
              spread_mu_ann = 0.0010, spread_sd_ann = 0.00019, spread_phi = 0.85, idio_vol_ann = 0.0028,
              start_lag_days = 180L)

df <- tibble(
    date   = date,
    IDX1   = IDX1,
    `IDX1-GR` = IDX1_GR,
    IDX2   = IDX2,
    `IDX2-GR` = IDX2_GR,
    f1 = f1,
    f2 = f2,
    g1 = g1,
    g2 = g2
)

fund_index_map <- c(
    f1 = "IDX1",
    f2 = "IDX1",
    g1 = "IDX2",
    g2 = "IDX2",
    "IDX1-GR" = "IDX1",
    "IDX2-GR" = "IDX2"
)

gg_par <- scale_color_manual(
    values = c(
        "IDX1-GR" = "black",
        "IDX2-GR" = "grey60",
        "f1" = "#1bbe27",
        "f2" = "#e97f02",
        "g1" = "#b530b3",
        "g2" = "#37598a"
    )
)

nd = 365
fundsr_options(reload = TRUE)
store_timeseries("test", df, fund_index_map)
series <- build_all_series()
diffs_net  <- roll_diffs(series, nd, get_fund_index_map(), index_level = "net")
diffs_gross  <- roll_diffs(series, nd, get_fund_index_map(), index_level = "gross")

cagr_plot <- plot_roll_diffs(diffs_net$cagr,
                             nd,
                             c("f1","f2","g1","g2","IDX1-GR","IDX2-GR"),
                             gg_params = gg_par)
log_plot  <- plot_roll_diffs(diffs_net$log,
                             nd,
                             c("f1","f2","g1","g2","IDX1-GR","IDX2-GR"),
                             use_log = TRUE,
                             gg_params = gg_par)
gr_log_plot  <- plot_roll_diffs(diffs_gross$log,
                             nd,
                             c("f1","f2","g1","g2","IDX1-GR","IDX2-GR"),
                             use_log = TRUE,
                             bmark_type = "gross",
                             gg_params = gg_par)
