# AGENTS.md

Guidance for AI/code agents working in this repository.

## Scope, priorities, and discipline
- This guidance applies to the entire repository.
- Follow **system/developer/user** instructions first, then this file.
- Keep changes **small, reviewable, and reversible**.
- Do not refactor unrelated code. Avoid renames/moves/deletions unless required.
- Prefer incremental edits that keep diffs readable.

## Repository overview
- This is an **R package** named `fundsr` for importing ETF/index data and producing plots and financial/survival visuals.
- Core package code: `R/`
- Tests: `tests/testthat/` (entry: `tests/testthat.R`)
- Docs: roxygen in `R/` → generated `man/` and `NAMESPACE`
- Website/vignettes: `vignettes/` and pkgdown config in `_pkgdown.yml`
- i18n: `R/i18n.R`, `po/`, `inst/po/`

## Architecture map (rough)
- **Checks & errors:** `R/checks.R`
- **State & storage:** `R/state.R`, `R/storage.R`, `R/zzz.R`
- **Options:** `R/options.R`
- **Downloads:** `R/fund_download.R`
- **Ingest/readers:** `R/read_text.R`, `R/read_excel.R`, `R/data_loaders.R`
- **Computation:** `R/fund_calc.R`, `R/fund_index_map.R`
- **Plotting/export:** `R/plot_*.R`, `R/plot_export.R`, `R/xlm.R`, `R/life.R`, `R/es_aasmr.R`
- **Provider wrappers:** `R/wrappers.R`

Preserve these boundaries; don’t mix unrelated concerns.

## Error handling (IMPORTANT)
This repo uses a structured error framework. Prefer it over raw `stop()`.

### Use `stop_bad_arg()` when:
- A function argument is invalid (type/length/range/pattern).
- An input parameter contradicts another parameter.
- A required column name is missing because the user specified it wrong.

### Use `fundsr_abort()` when:
- The failure is **not** primarily an argument issue:
  - IO problems (missing files, unreadable files, cannot create directories)
  - Bad or incomplete input data (file contents don’t match expectations)
  - Invalid internal state (`.fundsr` / `.fundsr_storage` not initialised, registry corrupted)
  - Join conflicts, missing baseline rows, inconsistent datasets, etc.
- Wrap underlying errors via `parent = e` and classify the error with `class = ...`.

### Conventions
- Provide messages as character vectors when useful; they are collapsed with newlines.
- Prefer stable error class names like:
  - `fundsr_bad_state`, `fundsr_io_error`, `fundsr_bad_data`,
    `fundsr_download_failed`, `fundsr_loader_failed`, `fundsr_no_data`,
    `fundsr_incomplete_data`, `fundsr_bad_option`, etc.
- When applicable, set `arg = "<argument name>"` to support caller UX.

## State model & side effects
- Package state lives in `.fundsr` and `.fundsr_storage` (initialised in `.onLoad()` in `R/zzz.R`).
- Functions that rely on state/storage should call:
  - `fundsr_require_state()` or `fundsr_require_state(storage = TRUE)`
- Do not introduce new global variables. Do not bypass these helpers.

## Options and messaging
### Options
Options are namespaced as `fundsr.<name>` and accessed via `fundsr_get_option()`.
Key options:
- `fundsr.data_dir`, `fundsr.out_dir`
- `fundsr.reload` (controls caching in data loaders)
- `fundsr.verbosity` (integer; 0 silences informational messages)

Do not read `getOption("fundsr.*")` directly in new code; use `fundsr_get_option()`.

### Messaging / progress output
- Use `fundsr_msg(..., level = <int>)` for output. Avoid `message()`, `cat()`, `print()`.
- Avoid noisy defaults:
  - Do not enable `readr` progress bars by default.
  - Gate progress output via `interactive()` or `fundsr_verbosity()`/`fundsr_msg(level=...)`.
- Prefer `level = 1L` for normal informative messages, `2L` for detailed diagnostics.

## i18n / gettext
- User-facing strings may be translated.
- When composing translated strings with placeholders:
  - Translate first: `gettext("... {name} ...")`
  - Then interpolate: `glue(..., gettext("..."))`
- Do not change placeholder names casually; it breaks translations.

## Style & coding conventions
- Use idiomatic R with **4-space indentation**.
- Keep lines reasonably short (~100 chars target).
- Avoid deep nesting; use small helpers where appropriate.
- Follow existing conventions:
  - `check_*()` helpers for validation
  - `.data` / `.env` pronouns in dplyr code
  - `imports.R` governs package imports; avoid adding `library()` in package code
- Avoid adding dependencies unless necessary; if you add one, update `DESCRIPTION`.

## Documentation and generated files
- `man/*.Rd` and `NAMESPACE` are generated.
- If you modify roxygen or exports, regenerate via `devtools::document()` and commit the results.
- Keep README/vignettes consistent with user-visible behavior changes.

## Testing and validation
Prefer the narrowest checks first:
1. Targeted tests:
   - `Rscript -e 'testthat::test_file("tests/testthat/test-<file>.R")'`
2. Full tests:
   - `Rscript -e 'testthat::test_dir("tests/testthat")'`
3. Full package check:
   - `R CMD check --no-manual --as-cran .`

If checks cannot be run due to environment limits, state what was attempted and why.

## Data, examples, and downloads
- Do not commit bulky/generated data unless explicitly requested.
- Example scripts live under `inst/scripts/examples`; preserve naming/layout.
- Download helpers enforce safe filenames; do not loosen filename safety checks without a specific reason.

## PR/commit expectations for agents
- Make atomic commits with clear, imperative messages.
- Summarize:
  - what changed,
  - why it changed,
  - how it was validated,
  - any backward-compatibility impact.

## Quick checklist before finishing
- [ ] Changes match the request.
- [ ] Errors use `stop_bad_arg()` / `fundsr_abort()` appropriately.
- [ ] Messages respect `fundsr_msg()` / verbosity.
- [ ] State access uses `fundsr_require_state()` where needed.
- [ ] Docs regenerated if roxygen/exports changed.
- [ ] Tests/checks run (or blockers documented).
- [ ] Diff is focused and reviewable.
