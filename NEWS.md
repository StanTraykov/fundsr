# fundsr 0.5.0

## Importing

- rename `load_fund()` to `import_fund()` (`load_fund()` remaining as a deprecated alias)
- make `var_name` param optional for all provider wrappers (fund and index)
- add `override_ext` param to `read_timeseries()`

# fundsr 0.4.2

## Bugfix

- export `adjust_for_split()`

# fundsr 0.4.1

## General

- add new structured error/state-checking framework; improve error messages, standardize classes
- add session management to all functions requiring state via `session` parameter (not specifying it uses a default session fo backwards compatibility / simple use cases)

## Importing

- add `adjust_for_split()` helper (for use in `postprocess = `)
- validate imported time series have unique dates (also check for this during tracking difference calculations)
- support/fix numeric sheet selection for Excel 2003 XML files

# fundsr 0.4.0

## Importing
- add `postprocess` param to `store_timeseries()`
- add `line_filter` param to `read_timeseries()`

# fundsr 0.3.0

## Breaking changes
- rename `clear_storage()` argument `clear_fund_index_map` to `clear_map` to avoid name clash with `clear_fund_index_map()`
- change `join_env()` full join order to alphabetical

## Importing
- add support for unabbreviated month names (specify with capital `M` in `date_order` parameters, e.g. `"Mdy"`)
- add download checks, retries, temp + atomic rename for `download_fund_data()`; also prefer curl when available (curl added to `Suggests:`)

## Documentation
- add categories to function reference; improve docs, doc-examples, vignettes
- add a logo

## Fixes
- fix unset `fundsr.data_dir` behavior (stop writing to working directory; use `tools::R_user_dir("fundsr", which = "data")`)

# fundsr 0.2.1

## Documentation
- add Importing vignette and improve docs and vignettes

## Rolling diffs
- add Excel reader function `read_timeseries_excel()`
- improve `read_timeseries()`: support text dates, any column name for date
- add fund_index_map merge/clean functions `add_fund_index_map()`, `clean_fund_index_map()`
- add example Excel + CSV dataset in `extdata`
- `late_join` parameter to `join_env()`
- param rename/depr.: data_sheet -> sheet, coalesce_suffixed -> join_precedence

## Plot output
- add automatic Inkscape detection

## Fixes
- add various `join_env()` df and column checks to make errors easier to understand

# fundsr 0.2.0

## Rolling diffs
- improve gross/net index handling (no state resets needed; `index_level` and `gross_suffix` parameters to `roll_diffs()`)
- add `build_all_series()` (wrapping around `run_data_loaders()` and `join_env()`)
- refactor `roll_diffs()` for speed / better interface

## Survival plots
- add survival curve plots using Human Mortality Database (HMD) empirical data and EUROPOP2023 projections

## Fixes
- skip unavailable funds, indices when calculating differences
- ensure y-limits in rolling-difference plots include the last 30 days (even if outside quantiles)
- make `roll_diffs()` skip benchmarks (in addition to funds) that are not found in supplied df
- more robust `gg_params` list flattening
- date breaks on fixed months, quarter or biannual (once plot >3 yrs)

## Tests
- add rolling test

## Vignettes, translations
- add life vignette
- fix Bulgarian translation: wrong y-axis label on log-return charts

# fundsr 0.1.0

## Configuration

- `fundsr_options()` provides a validated wrapper around `options()` for setting common `fundsr.*` options (data directories, export settings, URL registry, and more).
- `add_fund_urls()` adds/updates the fund download registry (option `fundsr.fund_urls`).

## Import and storage

- Internal caching/storage for imported time series via a storage environment (`get_storage()`, `clear_storage()`, `store_timeseries()`), with optional forced refresh via `options(fundsr.reload = TRUE)`.
- Extensible data-loader framework for imports (`add_data_loader()`, `run_data_loaders()`, `clear_data_loaders()`).
- Download and import helpers:
  - Download configured fund files to a local data directory (`download_fund_data()`).
  - Read generic delimited time series with epoch timestamps (`read_timeseries()`).
  - Import fund NAV (and optional benchmark series) from Excel and register fund→benchmark mappings (`load_fund()`, `get_fund_index_map()`).
  - Fallback parser for Excel 2003 XML (SpreadsheetML) files.
- `join_env()` combines multiple tables stored in an environment, with optional “late” left-joins and optional coalescing of `.x`/`.y` join columns.

## Rolling differences

- `roll_diffs()` computes rolling, annualized tracking differences for fund–index pairs with either log-return or CAGR-style variants, producing `<fund>_rd` columns.

## Plotting and export

- `plot_roll_diffs()` visualizes rolling tracking-difference series with quantile-based y-limits and bps axis labeling.
- `run_plots()` generates plot sets from a specification, producing both CAGR and log-return variants.
- SVG-first export pipeline: `save_plot()` writes SVG files and queues PNG exports for batch conversion via Inkscape (`export_pngs()`), with `clear_inkscape_queue()` to reset the queue.
- Optional direct PNG export via `ggplot2::ggsave()` without Inkscape (lower quality; controlled by `fundsr.internal_png`).

## Internationalization

- Chart elements are available in two languages (English and Bulgarian), with language selection based on `LANGUAGE` / `LC_MESSAGES`.
- Language-aware user-supplied strings are supported via named vectors keyed by language code.

## Utilities

- `reset_state()` clears cached storage, loader registry, queued exports, and other bookkeeping for the current session.
