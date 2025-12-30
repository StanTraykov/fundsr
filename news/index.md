# Changelog

## fundsr (development version)

### Survival plots, examples, fixes

#### Add survival plots (empirical HMD and projected EUROPOP2023)

#### Examples, translations, vignettes

- add life vignette
- fix BG translation: y axis label on log-return charts
- add more example scripts; translate more index names in common_spec
- translate examples (add BG titles)

## fundsr 0.1.0

### Initial release

#### Configuration

- [`fundsr_options()`](https://stantraykov.github.io/fundsr/reference/fundsr_options.md)
  provides a validated wrapper around
  [`options()`](https://rdrr.io/r/base/options.html) for setting common
  `fundsr.*` options (data directories, export settings, URL registry,
  and more).
- [`add_fund_urls()`](https://stantraykov.github.io/fundsr/reference/add_fund_urls.md)
  adds/updates the fund download registry (option `fundsr.fund_urls`).

#### Import and storage

- Internal caching/storage for imported time series via a storage
  environment
  ([`get_storage()`](https://stantraykov.github.io/fundsr/reference/get_storage.md),
  [`clear_storage()`](https://stantraykov.github.io/fundsr/reference/clear_storage.md),
  [`store_timeseries()`](https://stantraykov.github.io/fundsr/reference/store_timeseries.md)),
  with optional forced refresh via `options(fundsr.reload = TRUE)`.
- Extensible data-loader framework for imports
  ([`add_data_loader()`](https://stantraykov.github.io/fundsr/reference/add_data_loader.md),
  [`run_data_loaders()`](https://stantraykov.github.io/fundsr/reference/run_data_loaders.md),
  [`clear_data_loaders()`](https://stantraykov.github.io/fundsr/reference/clear_data_loaders.md)).
- Download and import helpers:
  - Download configured fund files to a local data directory
    ([`download_fund_data()`](https://stantraykov.github.io/fundsr/reference/download_fund_data.md)).
  - Read generic delimited time series with epoch timestamps
    ([`read_timeseries()`](https://stantraykov.github.io/fundsr/reference/read_timeseries.md)).
  - Read MSCI TSV files
    ([`read_msci_tsv()`](https://stantraykov.github.io/fundsr/reference/read_msci_tsv.md))
    and import MSCI Excel data via
    [`msci()`](https://stantraykov.github.io/fundsr/reference/msci.md).
  - Import fund NAV (and optional benchmark series) from Excel and
    register fund→benchmark mappings
    ([`load_fund()`](https://stantraykov.github.io/fundsr/reference/load_fund.md),
    [`get_fund_index_map()`](https://stantraykov.github.io/fundsr/reference/get_fund_index_map.md)).
  - Fallback parser for Excel 2003 XML (SpreadsheetML) files.
- Convenience helpers for provider-specific Excel formats:
  [`ishs()`](https://stantraykov.github.io/fundsr/reference/ishs.md)
  (iShares),
  [`spdr()`](https://stantraykov.github.io/fundsr/reference/spdr.md)
  (SPDR),
  [`xtra()`](https://stantraykov.github.io/fundsr/reference/xtra.md)
  (Xtrackers),
  [`amun()`](https://stantraykov.github.io/fundsr/reference/amun.md)
  (Amundi),
  [`inve()`](https://stantraykov.github.io/fundsr/reference/inve.md)
  (Invesco),
  [`vang()`](https://stantraykov.github.io/fundsr/reference/vang.md)
  (Vanguard),
  [`ubs()`](https://stantraykov.github.io/fundsr/reference/ubs.md)
  (UBS),
  [`hsbc()`](https://stantraykov.github.io/fundsr/reference/hsbc.md)
  (HSBC).
- [`join_env()`](https://stantraykov.github.io/fundsr/reference/join_env.md)
  combines multiple tables stored in an environment, with optional
  “late” left-joins and optional coalescing of `.x`/`.y` join columns.

#### Rolling differences

- [`roll_diffs()`](https://stantraykov.github.io/fundsr/reference/roll_diffs.md)
  computes rolling, annualized tracking differences for fund–index pairs
  with either log-return or CAGR-style variants, producing `<fund>_rd`
  columns.

#### Plotting and export

- [`plot_roll_diffs()`](https://stantraykov.github.io/fundsr/reference/plot_roll_diffs.md)
  visualizes rolling tracking-difference series with quantile-based
  y-limits and bps axis labeling.
- [`run_plots()`](https://stantraykov.github.io/fundsr/reference/run_plots.md)
  generates plot sets from a specification, producing both CAGR and
  log-return variants.
- SVG-first export pipeline:
  [`save_plot()`](https://stantraykov.github.io/fundsr/reference/save_plot.md)
  writes SVG files and queues PNG exports for batch conversion via
  Inkscape
  ([`export_pngs()`](https://stantraykov.github.io/fundsr/reference/export_pngs.md)),
  with
  [`clear_inkscape_queue()`](https://stantraykov.github.io/fundsr/reference/clear_inkscape_queue.md)
  to reset the queue.
- Optional direct PNG export via
  [`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html)
  without Inkscape (lower quality; controlled by `fundsr.internal_png`).
- Xetra Liquidity Measure (XLM) support: read and plot XLM time series
  ([`read_xlm_directory()`](https://stantraykov.github.io/fundsr/reference/read_xlm_directory.md),
  [`plot_xlms()`](https://stantraykov.github.io/fundsr/reference/plot_xlms.md)),
  with optional back-translation of displayed tickers using
  `options(fundsr.xetra_map)`.

#### Internationalization

- Chart elements are available in two languages (English and Bulgarian),
  with language selection based on `LANGUAGE` / `LC_MESSAGES`.
- Language-aware user-supplied strings are supported via named vectors
  keyed by language code.

#### Utilities

- [`reset_state()`](https://stantraykov.github.io/fundsr/reference/reset_state.md)
  clears cached storage, loader registry, queued exports, and XLM
  bookkeeping for the current session.
