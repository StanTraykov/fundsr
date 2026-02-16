# AGENTS.md

Guidance for AI/code agents working in this repository.

## Scope and priorities
- This file applies to the entire repository tree.
- Follow direct user/developer/system instructions first, then this file.
- Keep changes focused, minimal, and reversible.
- Avoid broad refactors unless explicitly requested.

## Repository overview
- This is an **R package** named `fundsr` for importing ETF/index data and plotting rolling differences, liquidity (XLM), and survival/financial-planning visuals.
- Core package code lives in `R/`.
- Tests live in `tests/testthat/` and are run via `tests/testthat.R`.
- Documentation pages are generated in `man/` from roxygen comments in `R/`.
- Package metadata/config is in `DESCRIPTION`, `NAMESPACE`, `.lintr`, `.Rprofile`, and `_pkgdown.yml`.
- Website/vignettes content lives in `vignettes/` and pkgdown/GitHub workflow config.

## High-level architecture
- **Data ingest/parsing:** `R/read_*.R`, `R/read_text.R`, `R/read_excel.R`, `R/fund_download.R`, `R/data_loaders.R`.
- **State/storage/options:** `R/state.R`, `R/storage.R`, `R/options.R`.
- **Checks/errors/messages:** `R/checks.R`, `R/utils.R` (e.g. `fundsr_abort`, `stop_bad_arg`, `check_*`, `fundsr_msg`).
- **Computation:** `R/fund_calc.R`, `R/fund_index_map.R`.
- **Plotting/export:** `R/plot_*.R`, `R/plot_export.R`, `R/xlm.R`, `R/life.R`, `R/wrappers.R`.
- **i18n/localization:** `R/i18n.R`, `po/`, `inst/po/`.

When touching code, preserve this split and avoid mixing unrelated concerns.

## Style and coding conventions
- Use idiomatic R with **4-space indentation** (see `.lintr` / styler config).
- Keep lines reasonably short (target ~100 chars).
- Prefer explicit names and small helper functions over deeply nested pipelines.
- Follow existing tidyverse style and existing package patterns (`rlang` `.data` pronoun, `check_*` helpers, `fundsr_msg`, etc.).
- Avoid adding new dependencies unless necessary; if you must, update `DESCRIPTION`.

## Errors, checks, and message style
This repo uses a structured, consistent error/checking approach.

### Which helper to use
- **Bad user arguments:** use `stop_bad_arg(arg, msg, call = ...)` (class `fundsr_bad_arg`).
- **Bad state / internal invariants:** use `fundsr_abort(..., class = "fundsr_bad_state" / "fundsr_internal_error")`.
- **I/O / data problems:** use `fundsr_abort(..., class = "fundsr_io_error" / "fundsr_bad_data" / more specific subclasses as appropriate)`.
- Prefer existing `check_*()` helpers (`check_string`, `check_numeric_scalar`, `check_logical`, `check_mapping`) over ad-hoc validation.

### Message style (house conventions)
- Multi-line messages are normal and encouraged when they add structured context.
- Prefer a **1-line headline** followed by a small set of **context lines** (often `key = value.`) that help debugging.
- Use `msg = c(...)` for message vectors; `collapse_msg()` will join with newlines.
- Keep context scan-friendly: short lines, concrete values.
- Include key parameters when relevant: `file`, `path`, `sheet`, `date_col`, `ext`, counts (`n_rows`, `n_unique`), and a few examples (first 3–5 offending values).
- Avoid overly verbose narrative text unless it materially helps debugging.

### Example
```r
if (sheet_idx > length(ws_nodes)) {
    stop_bad_arg(
        "sheet [if numeric]",
        c(
            "must be between 1 and the number of worksheets in the XML file.",
            sprintf("sheet     = %d.", sheet_idx),
            sprintf("n_sheets  = %d.", length(ws_nodes)),
            sprintf("file_path = %s.", sQuote(file_path))
        )
    )
}
```

## Documentation and generated files
- `man/*.Rd` and `NAMESPACE` are generated artifacts.
- If you change roxygen docs or exported functions, regenerate docs (typically via `devtools::document()`), and commit resulting `man/` and `NAMESPACE` updates.
- Do not hand-edit generated files unless there is a specific reason.
- Keep `README.md`/vignettes consistent with behavior changes that affect users.

## Testing and validation
Run the narrowest useful checks first, then broader checks if needed.

### Tests (preferred in this repo/Codex env [package not installed / attached])
- `Rscript -e 'testthat::test_local()'`
- `Rscript -e 'testthat::test_local(filter = "session")'`

### Avoid (fails unless package installed)
- `Rscript -e 'testthat::test_dir("tests/testthat")'`

### Package check (heavy)
- `R CMD check --no-manual --as-cran .`

If runtime/environment limits block checks, report what was attempted and why it failed.

## Linting/formatting
- Lint config is in `.lintr`.
- Styler helper exists at `dev/styler/style.R`.
- Keep exclusions in mind (`inst/extdata`, `data-raw`, `vignettes`, `inst/scripts`).
- Avoid mass reformatting unrelated files.

## Internationalization (i18n)
- User-facing strings may be translated with gettext.
- When changing translatable UI/messages, ensure consistency with i18n helpers and translation templates/catalogs (`po/`, `inst/po/`) when relevant.
- Do not remove translation hooks from existing user-facing text without reason.

## Data and examples
- Example/raw data helpers are under `data-raw/` and `inst/extdata/`.
- Be careful not to commit bulky/generated data unless required.
- Example scripts are maintained directly under `inst/scripts/examples`; keep their current layout and naming conventions.

## CI/CD awareness
- GitHub Actions run multi-OS R CMD checks and pkgdown builds.
- Prefer changes that are deterministic across platforms/locales.
- Avoid assumptions about local paths, locales, or interactive sessions.

## Commit and PR expectations for agents
- Make atomic commits with clear, imperative messages.
- Include a concise summary of:
  - what changed,
  - why it changed,
  - how it was validated.
- If you changed behavior, mention any backward-compatibility impact.

## Safety and editing discipline
- Never rewrite history unless explicitly asked.
- Do not delete or rename files as cleanup unless the task requires it.
- Do not touch unrelated files.
- Prefer incremental edits that are easy to review.

## Quick checklist before finishing
- [ ] Scope matches the request.
- [ ] Code/doc style matches repository conventions.
- [ ] Generated docs updated if needed.
- [ ] Relevant tests/checks run (or blockers documented).
- [ ] `git diff` is focused and reviewable.
