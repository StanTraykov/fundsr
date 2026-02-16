# AGENTS.md

Guidance for AI/code agents working in this repository.

## 0) Priority order
1. Follow **system/developer/user instructions** in the chat.
2. Then follow this file.
3. If this file conflicts with the repo’s established conventions, prefer the repo’s existing code patterns.

## 1) Repository summary
`fundsr` is an **R package** for:
- importing fund/index time series from provider exports (Excel/CSV/TSV),
- caching them in a package-owned session store,
- computing rolling differences / tracking diagnostics,
- producing plots (incl. export helpers),
- plus survival / life-table utilities.

Key directories:
- `R/` – package code (source of truth)
- `tests/testthat/` – tests (`tests/testthat.R` runs them)
- `man/` – generated Rd docs (do not edit by hand)
- `vignettes/`, `pkgdown/` – website and long-form docs
- `po/`, `inst/po/` – translations / i18n assets

## 2) Architecture and state model
This package is intentionally **stateful**:

- `.fundsr` is a session environment for mutable registries and metadata:
  - loader registry (`.fundsr$data_loaders`)
  - fund/index map (`.fundsr$fund_index_map`)
  - export queue (`.fundsr$inkscape_queue`)
  - XLM bookkeeping (`.fundsr$done_xlm_sets`)

- `.fundsr_storage` is an environment used as a cache for imported/processed objects.

Initialization happens in `R/zzz.R` via `.onLoad()`.

### Main building blocks
- **Checks & structured errors:** `fundsr_abort()`, `stop_bad_arg()`, `check_*()` helpers (`R/checks.R`)
- **State gate:** `fundsr_require_state()` (`R/state.R`)
- **Storage/cache:** `store_timeseries()`, `get_storage()`, `clear_storage()` (`R/storage.R`)
- **Loader registry:** `add_data_loader()`, `run_data_loaders()`, `clear_data_loaders()` (`R/data_loaders.R`)
- **Wrappers:** provider wrappers are thin adapters around the common import/caching workflow (`R/wrappers.R`)
- **Joining:** `join_env()` and `build_all_series()` assemble storage content into wide tibbles (`R/storage.R`)
- **Utilities & messaging:** `%||%`, `fundsr_msg()`, helper utilities (`R/utils.R`)
- **XLM utilities:** read+plot Xetra XLM exports (`R/xlm.R`)
- **i18n:** gettext usage + catalogs (`R/i18n.R`, `po/`, `inst/po/`)

When modifying code, preserve these separations:
- readers parse; wrappers configure; storage caches; calculations compute; plots render.

## 3) Error handling and validation rules
This repo uses structured errors with `rlang::abort()`.

### Which error helper to use
- Use `stop_bad_arg()` for **invalid user inputs / argument validation**.
- Use `fundsr_abort()` for:
  - state problems (e.g. missing initialization),
  - I/O failures,
  - invalid/ambiguous input data files,
  - internal invariants / “this should not happen” situations,
  - wrapping lower-level failures.

### Checks and consistency
- Prefer existing `check_*()` helpers over ad-hoc validation.
- When wrapping a caught error, pass it as `parent = e` so callers retain context.
- Use meaningful error classes consistent with existing ones (examples seen in code):
  - `fundsr_bad_state`, `fundsr_io_error`, `fundsr_bad_data`,
  - `fundsr_duplicate_dates`, `fundsr_download_failed`, etc.

Message style:
- Keep messages actionable and specific (include key parameters like `file`, `sheet`, etc. when relevant).
- Avoid overly verbose multi-paragraph output unless it materially helps the user debug.

## 4) Coding style and conventions
- Indentation: **4 spaces**.
- Keep lines roughly ≤ 100 characters when practical.
- Match existing tidyverse patterns:
  - use `.data` / `.env` pronouns for NSE,
  - prefer readable pipelines and small helpers over deep nesting,
  - use existing messaging (`fundsr_msg`) for progress output where appropriate.
- Avoid broad refactors unless explicitly requested.
- Avoid new dependencies unless necessary; if added, update `DESCRIPTION`.

## 5) Import pipeline conventions
Imported series typically have:
- a `date` column of class `Date`,
- one or more value columns (often numeric).

Wrappers/loaders generally:
1. call a reader (`read_timeseries()` / `read_timeseries_excel()`),
2. postprocess (select/rename/coerce),
3. store via `store_timeseries()` under a stable key (often lowercase ticker),
4. optionally record benchmark mappings via `fund_index_map`.

If changing a reader, consider downstream effects on:
- provider wrappers in `R/wrappers.R`,
- `build_all_series()` / `join_env()`,
- rolling difference calculations and plotting code.

## 6) Documentation and generated files
- `man/*.Rd` and `NAMESPACE` are generated from roxygen comments.
- Do not hand-edit generated files.
- If exported APIs or roxygen docs change, run `devtools::document()` and commit updated `man/` and `NAMESPACE`.
- Keep README/vignettes consistent with behavior changes that affect users.

## 7) Internationalization (i18n)
- Some user-facing strings are translated via gettext.
- When modifying user-visible strings, preserve translation hooks where present.
- If you introduce new user-facing strings in translated areas, follow existing i18n patterns and consider whether catalogs/templates need updating.

## 8) Tests and validation workflow
Prefer the narrowest relevant checks:
1. Targeted tests (when present):  
   `Rscript -e 'testthat::test_file("tests/testthat/<file>.R")'`
2. All tests:  
   `Rscript -e 'testthat::test_dir("tests/testthat")'`
3. Full package check:  
   `R CMD check --no-manual --as-cran .`

If checks cannot be run (environment limits), state what was attempted and why.

## 9) Change discipline for agents
- Keep changes minimal and reviewable.
- Do not rename/move/delete files unless required by the task.
- Avoid “drive-by” formatting or cleanup in unrelated files.
- Prefer atomic commits with imperative messages and a brief validation note.

## 10) Pre-finish checklist
- [ ] Scope matches the user request.
- [ ] Error handling follows `fundsr_abort()` / `stop_bad_arg()` conventions.
- [ ] Style matches repository conventions.
- [ ] Generated docs updated if needed.
- [ ] Relevant tests/checks run (or blockers documented).
- [ ] `git diff` is focused and reviewable.
