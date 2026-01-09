# Package index

## Config functions

- [`fundsr_options()`](https://stantraykov.github.io/fundsr/reference/fundsr_options.md)
  : Set fundsr package options
- [`reset_state()`](https://stantraykov.github.io/fundsr/reference/reset_state.md)
  : Clear fundsr session state

## Download functions

- [`add_fund_urls()`](https://stantraykov.github.io/fundsr/reference/add_fund_urls.md)
  : Add entries to the fund download list
- [`download_fund_data()`](https://stantraykov.github.io/fundsr/reference/download_fund_data.md)
  : Download fund data according to the configured download list

## Fund/index file readers

- [`read_msci_tsv()`](https://stantraykov.github.io/fundsr/reference/read_msci_tsv.md)
  : Read an MSCI two-column TSV file
- [`read_timeseries()`](https://stantraykov.github.io/fundsr/reference/read_timeseries.md)
  : Read a time series file (CSV/TSV) with a date + one or more value
  columns
- [`read_timeseries_excel()`](https://stantraykov.github.io/fundsr/reference/read_timeseries_excel.md)
  : Read a time series from an Excel workbook

## Fund/index workflow functions

- [`add_data_loader()`](https://stantraykov.github.io/fundsr/reference/add_data_loader.md)
  : Register a data loader
- [`build_all_series()`](https://stantraykov.github.io/fundsr/reference/build_all_series.md)
  : Run all registered data loaders and join all loaded series into a
  big tibble.
- [`clear_data_loaders()`](https://stantraykov.github.io/fundsr/reference/clear_data_loaders.md)
  : Clear registered data loaders
- [`clear_storage()`](https://stantraykov.github.io/fundsr/reference/clear_storage.md)
  : Clear storage
- [`get_storage()`](https://stantraykov.github.io/fundsr/reference/get_storage.md)
  : Get the internal fund storage
- [`join_env()`](https://stantraykov.github.io/fundsr/reference/join_env.md)
  : Join all tibbles in an environment with optional late left-joins
- [`load_fund()`](https://stantraykov.github.io/fundsr/reference/load_fund.md)
  : Load a fund's NAV data and optionally register its benchmark mapping
- [`run_data_loaders()`](https://stantraykov.github.io/fundsr/reference/run_data_loaders.md)
  : Run registered data loaders
- [`store_timeseries()`](https://stantraykov.github.io/fundsr/reference/store_timeseries.md)
  : Store a cached object in the package storage environment

## Fund-index map mutators

- [`add_fund_index_map()`](https://stantraykov.github.io/fundsr/reference/add_fund_index_map.md)
  : Add to fund-index map
- [`clear_fund_index_map()`](https://stantraykov.github.io/fundsr/reference/clear_fund_index_map.md)
  : Clear fund-index map
- [`get_fund_index_map()`](https://stantraykov.github.io/fundsr/reference/get_fund_index_map.md)
  : Get the internal fund index map

## Provider wrappers

- [`amun()`](https://stantraykov.github.io/fundsr/reference/amun.md) :
  Import an Amundi fund
- [`hsbc()`](https://stantraykov.github.io/fundsr/reference/hsbc.md) :
  Import an HSBC fund
- [`inve()`](https://stantraykov.github.io/fundsr/reference/inve.md) :
  Import an Invesco fund
- [`ishs()`](https://stantraykov.github.io/fundsr/reference/ishs.md) :
  Import an iShares fund
- [`msci()`](https://stantraykov.github.io/fundsr/reference/msci.md) :
  Import an MSCI index sheet and register benchmark mappings
- [`spdr()`](https://stantraykov.github.io/fundsr/reference/spdr.md) :
  Import an SPDR fund
- [`ubs()`](https://stantraykov.github.io/fundsr/reference/ubs.md) :
  Import a UBS fund
- [`vang()`](https://stantraykov.github.io/fundsr/reference/vang.md) :
  Import a Vanguard fund
- [`xtra()`](https://stantraykov.github.io/fundsr/reference/xtra.md) :
  Import an Xtrackers fund

## Rolling-difference functions

- [`plot_roll_diffs()`](https://stantraykov.github.io/fundsr/reference/plot_roll_diffs.md)
  : Plot rolling return differences against benchmark
- [`roll_diffs()`](https://stantraykov.github.io/fundsr/reference/roll_diffs.md)
  : Compute rolling annualized tracking difference statistics

## Plot export utilities

- [`clear_inkscape_queue()`](https://stantraykov.github.io/fundsr/reference/clear_inkscape_queue.md)
  : Clear queued Inkscape exports
- [`export_pngs()`](https://stantraykov.github.io/fundsr/reference/export_pngs.md)
  : Export queued SVGs to PNG via Inkscape
- [`run_plots()`](https://stantraykov.github.io/fundsr/reference/run_plots.md)
  : Generate and export rolling-difference and XLM plots from a
  specification
- [`save_plot()`](https://stantraykov.github.io/fundsr/reference/save_plot.md)
  : Save a plot as SVG and/or PNG and queue for Inkscape conversion

## XLM functions

- [`plot_xlms()`](https://stantraykov.github.io/fundsr/reference/plot_xlms.md)
  : Plot Xetra Liquidity Measure (XLM) time series
- [`read_xlm_directory()`](https://stantraykov.github.io/fundsr/reference/read_xlm_directory.md)
  : Read and combine XLM data from multiple Xetra XLSX files

## Survival-curve functions

- [`chance_alive()`](https://stantraykov.github.io/fundsr/reference/chance_alive.md)
  : Compute conditional survival (chance alive) by age
- [`chance_alive_es_aasmr()`](https://stantraykov.github.io/fundsr/reference/chance_alive_es_aasmr.md)
  : Compute cohort-style survival from Eurostat EUROPOP2023 mortality
  assumptions
- [`plot_chance_alive()`](https://stantraykov.github.io/fundsr/reference/plot_chance_alive.md)
  : Plot chance alive by age
- [`plot_chance_alive_es_aasmr()`](https://stantraykov.github.io/fundsr/reference/plot_chance_alive_es_aasmr.md)
  : Plot cohort-style survival from Eurostat EUROPOP2023 assumptions
- [`read_es_aasmr()`](https://stantraykov.github.io/fundsr/reference/read_es_aasmr.md)
  : Read Eurostat EUROPOP2023 mortality-assumption table (proj_23naasmr)
- [`read_life_table()`](https://stantraykov.github.io/fundsr/reference/read_life_table.md)
  : Read HMD period life tables (1x1) from disk
