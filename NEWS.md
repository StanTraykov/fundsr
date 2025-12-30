# fundsr (development version)

## Survival plots, examples, fixes

### Add survival plots (empirical HMD and projected EUROPOP2023)

### Examples, translations, vignettes
- add life vignette
- fix BG translation: y axis label on log-return charts
- add more example scripts; translate more index names in common_spec
- translate examples (add BG titles)

# fundsr 0.1.0

## Initial release

### Configuration

- `fundsr_options()` provides a validated wrapper around `options()` for setting common `fundsr.*` options (data directories, export settings, URL registry, and more).
- `add_fund_urls()` adds/updates the fund download registry (option `fundsr.fund_urls`).

### Import and storage

- Internal caching/storage for imported time series via a storage environment (`get_storage()`, `clear_storage()`, `store_timeseries()`), with optional forced refresh via `options(fundsr.reload = TRUE)`.
- Extensible data-loader framework for imports (`add_data_loader()`, `run_data_loaders()`, `clear_data_loaders()`).
- Download and import helpers:
  - Download configured fund files to a local data directory (`download_fund_data()`).
  - Read generic delimited time series with epoch timestamps (`read_timeseries()`).
  - Read MSCI TSV files (`read_msci_tsv()`) and import MSCI Excel data via `msci()`.
  - Import fund NAV (and optional benchmark series) from Excel and register fund→benchmark mappings (`load_fund()`, `get_fund_index_map()`).
  - Fallback parser for Excel 2003 XML (SpreadsheetML) files.
- Convenience helpers for provider-specific Excel formats:
  `ishs()` (iShares), `spdr()` (SPDR), `xtra()` (Xtrackers), `amun()` (Amundi), `inve()` (Invesco), `vang()` (Vanguard), `ubs()` (UBS), `hsbc()` (HSBC).
- `join_env()` combines multiple tables stored in an environment, with optional “late” left-joins and optional coalescing of `.x`/`.y` join columns.

### Rolling differences

- `roll_diffs()` computes rolling, annualized tracking differences for fund–index pairs with either log-return or CAGR-style variants, producing `<fund>_rd` columns.

### Plotting and export

- `plot_roll_diffs()` visualizes rolling tracking-difference series with quantile-based y-limits and bps axis labeling.
- `run_plots()` generates plot sets from a specification, producing both CAGR and log-return variants.
- SVG-first export pipeline: `save_plot()` writes SVG files and queues PNG exports for batch conversion via Inkscape (`export_pngs()`), with `clear_inkscape_queue()` to reset the queue.
- Optional direct PNG export via `ggplot2::ggsave()` without Inkscape (lower quality; controlled by `fundsr.internal_png`).
- Xetra Liquidity Measure (XLM) support: read and plot XLM time series (`read_xlm_directory()`, `plot_xlms()`), with optional back-translation of displayed tickers using `options(fundsr.xetra_map)`.

### Internationalization

- Chart elements are available in two languages (English and Bulgarian), with language selection based on `LANGUAGE` / `LC_MESSAGES`.
- Language-aware user-supplied strings are supported via named vectors keyed by language code.

### Utilities

- `reset_state()` clears cached storage, loader registry, queued exports, and XLM bookkeeping for the current session.
