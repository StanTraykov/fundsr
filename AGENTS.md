# AGENTS.md

Guidance for AI/code agents working in this repository.

## Scope and priorities
- This file applies to the entire repository tree.
- Follow direct user/developer/system instructions first, then this file.
- Keep changes focused, minimal, and reversible.

## Repository overview
- This is an **R package** named `fundsr` for importing ETF/index data and plotting rolling differences, liquidity, and survival/financial-planning visuals.
- Core package code lives in `R/`.
- Tests live in `tests/testthat/` and are run via `tests/testthat.R`.
- Documentation pages are generated in `man/` from roxygen comments in `R/`.
- Package metadata/config is in `DESCRIPTION`, `NAMESPACE`, `.lintr`, `.Rprofile`, and `_pkgdown.yml`.
- Website/vignettes content lives in `vignettes/` and `pkgdown/`/GitHub workflow config.

## High-level architecture
- **Data ingest/parsing:** `R/read_*.R`, `R/data_loaders.R`, `R/fund_download.R`.
- **Computation/state:** `R/fund_calc.R`, `R/state.R`, `R/storage.R`, `R/options.R`, `R/checks.R`.
- **Plotting/export:** `R/plot_*.R`, `R/plot_export.R`, `R/wrappers.R`, `R/life.R`, `R/xlm.R`.
- **i18n/localization:** `R/i18n.R`, `po/`, `inst/po/`.

When touching code, preserve this split and avoid mixing unrelated concerns.

## Style and coding conventions
- Use idiomatic R with **4-space indentation** (see `.lintr` / styler config).
- Keep lines reasonably short (target ~100 chars).
- Prefer explicit names and small helper functions over deeply nested pipelines.
- Follow existing tidyverse style and existing package patterns (`rlang` `.data` pronoun, `check_*` helpers, `fundsr_msg`, etc.).
- Do not introduce broad refactors unless explicitly requested.
- Avoid adding new dependencies unless necessary; if you must, update `DESCRIPTION`.

## Documentation and generated files
- `man/*.Rd` and `NAMESPACE` are generated artifacts.
- If you change roxygen docs or exported functions, regenerate docs (typically via `devtools::document()`), and commit resulting `man/` and `NAMESPACE` updates.
- Do not hand-edit generated files unless there is a specific reason.
- Keep `README.md`/vignettes consistent with behavior changes that affect users.

## Testing and validation
Run the narrowest useful checks first, then broader checks if needed.

Preferred commands:
1. Targeted tests for changed behavior (example):
   - `Rscript -e 'testthat::test_file("tests/testthat/test-rolling.R")'`
2. Full tests:
   - `Rscript -e 'testthat::test_dir("tests/testthat")'`
3. Package check (heavier):
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
