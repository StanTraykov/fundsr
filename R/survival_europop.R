#' Read Eurostat EUROPOP2023 mortality-assumption table (proj_23naasmr)
#'
#' Reads the Eurostat TSV export for EUROPOP2023 age-specific mortality rate
#' assumptions (dataset `proj_23naasmr`), typically downloaded as
#' `estat_proj_23naasmr.tsv.gz`. Returns a tidy long table with metadata columns
#' plus numeric `Age`, `Year`, and `mx`.
#'
#' Eurostat value flags (e.g. provisional/estimated markers) are tolerated: the
#' numeric part is parsed into `mx`, while missing values encoded as `:` are
#' returned as `NA`.
#'
#' The Eurostat `age` dimension uses codes like `Y_LT1` (age < 1), `Y1` (age 1),
#' and `Y_GE85` (age 85+). This function maps `Y_LT1` to `Age = 0` and parses
#' `Y{n}` and `Y_GE{n}` to integer ages.
#'
#' @param directory Directory containing `estat_proj_23naasmr.tsv.gz`.
#'
#' @return A tibble with columns:
#'   `freq`, `projection`, `sex`, `unit`, `geo`, `age`, `Age`, `Year`, `mx`.
#'
#' @family survival curve functions
#' @export
#'
#' @examples
#' \dontrun{
#' es_aasmr <- read_es_aasmr(file.path("data", "life"))
#' es_aasmr %>% dplyr::count(geo, sex, projection, sort = TRUE)
#' }
read_es_aasmr <- function(directory) {
    check_string(directory)
    file <- file.path(directory, "estat_proj_23naasmr.tsv.gz")
    if (!file.exists(file)) {
        fundsr_abort(
            msg = c(
                sprintf("Eurostat aasmr file not found in %s.", sQuote(directory)),
                i = sprintf("Expected file: %s.", sQuote(basename(file)))
            ),
            class = "fundsr_io_error",
            arg   = "directory"
        )
    }

    x <- readr::read_tsv(file,
                         col_types = readr::cols(.default = readr::col_character()),
                         na = c(":", ": "),
                         show_col_types = FALSE,
                         progress = TRUE,
                         trim_ws = TRUE)
    names(x) <- trimws(names(x))
    names(x)[1] <- "key"
    x %>%
        tidyr::separate(.data[["key"]],
                        into = c("freq", "projection", "sex", "age", "unit", "geo"),
                        sep = ",",
                        remove = TRUE) %>%
        tidyr::pivot_longer(cols = -all_of(c("freq", "projection", "sex", "age", "unit", "geo")),
                            names_to = "Year",
                            values_to = "mx_raw") %>%
        mutate(
            Year = as.integer(trimws(.data[["Year"]])),
            Age = dplyr::case_when(
                .data[["age"]] == "Y_LT1" ~ 0L,
                grepl("^Y\\d+$", .data[["age"]]) ~ as.integer(sub("^Y", "", .data[["age"]])),
                grepl("^Y_GE\\d+$", .data[["age"]]) ~ as.integer(sub("^Y_GE", "", .data[["age"]])),
                TRUE ~ NA_integer_
            ),
            mx = readr::parse_number(.data[["mx_raw"]])
        ) %>%
        select(all_of(c("freq", "projection", "sex", "unit", "geo", "age", "Age", "Year", "mx")))
}

#' Compute cohort-style survival from Eurostat EUROPOP2023 mortality assumptions
#'
#' Computes conditional survival (chance alive) for a person aged `age0` in
#' `start_year` using Eurostat EUROPOP2023 age-specific mortality rate assumptions
#' (dataset `proj_23naasmr`). The computation follows a cohort path (diagonal):
#' for age `age0 + k` it uses the mortality rate for year `start_year + k`.
#'
#' The result includes two projection variants: baseline (`BSL`) and lower
#' mortality (`LMRT`).
#'
#' @param es A tibble as returned by [read_es_aasmr()].
#' @param geo Eurostat geo code (e.g. `"BG"`, `"NL"`).
#' @param sex Sex code: `"m"` or `"f"` (case-insensitive).
#' @param age0 Baseline age (integer).
#' @param start_year Starting calendar year (integer). If `NULL`, uses the
#'   earliest year available for the selected `geo/sex`.
#'
#' @return A tibble with columns `geo`, `sex`, `projection`, `Year`, `Age`, `mx`,
#'   `qx`, `chance_alive`.
#'
#' @family survival curve functions
#' @export
#'
#' @examples
#' \dontrun{
#' es <- read_es_aasmr(file.path("data", "life"))
#' ca <- chance_alive_es_aasmr(es, geo = "NL", sex = "m", age0 = 42, start_year = 2022)
#' p <- plot_chance_alive_es_aasmr(ca, sex = "m", population = "NL")
#' p
#' }
chance_alive_es_aasmr <- function(es, geo, sex, age0, start_year = NULL) {
    if (!is.data.frame(es)) stop_bad_arg("es", "must be a data frame.")
    need <- c("projection", "sex", "geo", "Age", "Year", "mx")
    missing <- setdiff(need, names(es))
    if (length(missing)) {
        stop_bad_arg(
            "es",
            sprintf(
                "must contain column(s) %s; missing %s.",
                paste(sQuote(need), collapse = ", "),
                paste(sQuote(missing), collapse = ", ")
            )
        )
    }
    age0 <- check_numeric_scalar(age0, as_integer = TRUE, ge = 0)
    check_string(sex, pattern = "^[mMfF]$")
    sex <- toupper(sex)
    geo <- trimws(geo)
    filtered <- es %>%
        mutate(geo = trimws(.data[["geo"]]),
               sex = trimws(.data[["sex"]]),
               projection = trimws(.data[["projection"]])) %>%
        filter(.data[["geo"]] == .env$geo,
               .data[["sex"]] == .env$sex,
               .data[["projection"]] %in% c("BSL", "LMRT")) %>%
        filter(!is.na(.data[["mx"]]), !is.na(.data[["Year"]]), !is.na(.data[["Age"]]))
    if (nrow(filtered) == 0L) {
        fundsr_abort(
            msg = sprintf(
                "No rows in `es` match the requested geo/sex: %s / %s.",
                sQuote(geo), sQuote(sex)
            ),
            class = "fundsr_no_data",
            arg   = "es"
        )
    }

    if (is.null(start_year)) start_year <- min(filtered[["Year"]], na.rm = TRUE)
    start_year <- as.integer(start_year)

    one <- function(proj) {
        d <- filtered %>% filter(.data[["projection"]] == proj)
        if (nrow(d) == 0L) return(tibble::tibble())
        year_max <- max(d[["Year"]], na.rm = TRUE)
        age_max <- max(d[["Age"]], na.rm = TRUE)
        k_max <- min(year_max - start_year, age_max - age0)
        if (!is.finite(k_max) || k_max < 0L) return(tibble::tibble())
        out <- tibble::tibble(Year = start_year + 0L:k_max, Age = age0 + 0L:k_max) %>%
            left_join(d %>%
                          select(all_of(c("Year", "Age", "mx"))), by = c("Year", "Age")) %>%
            arrange(.data[["Year"]], .data[["Age"]])
        if (is.na(out[["mx"]][1])) {
            fundsr_abort(
                msg = c(
                    sprintf("Missing `mx` at baseline for projection %s.", proj),
                    i = sprintf("Baseline cell: Year = %s, Age = %s.", start_year, age0)
                ),
                class = "fundsr_incomplete_data",
                arg   = "es",
                call  = rlang::caller_env(n = 2)
            )
        }
        if (anyNA(out[["mx"]])) out <-
            out[seq_len(which(is.na(out[["mx"]]))[1] - 1L), , drop = FALSE]
        mx <- out[["mx"]]
        qx <- 1 - exp(-mx)
        qx <- pmin(1, pmax(0, qx))
        out %>% mutate(projection = proj,
                       qx = qx,
                       chance_alive = cumprod(c(1, (1 - qx)[-length(qx)])))
    }

    bind_rows(one("BSL"), one("LMRT")) %>%
        mutate(geo = geo, sex = sex) %>%
        select(all_of(c("geo", "sex", "projection", "Year", "Age", "mx", "qx", "chance_alive")))
}


#' Plot cohort-style survival from Eurostat EUROPOP2023 assumptions
#'
#' Plots the conditional survival curves returned by [chance_alive_es_aasmr()].
#' The baseline projection (`BSL`) is shown in black and the lower-mortality
#' variant (`LMRT`) is shown in dark cyan.
#'
#' @param ca A tibble as returned by [chance_alive_es_aasmr()], with columns
#'   `projection`, `Age`, and `chance_alive` (and typically `geo`, `sex`,
#'   `Year`).
#' @param sex Sex code: `"m"` or `"f"` (case-insensitive). Used for labeling.
#' @param population Population label/code to display in the subtitle (e.g.
#'   `"BG"`).
#'
#' @return A ggplot object.
#'
#' @family survival curve functions
#' @export
#'
#' @examples
#' \dontrun{
#' es_aasmr <- read_es_aasmr(file.path("data", "life"))
#' ca <- chance_alive_es_aasmr(es_aasmr, geo = "BG", sex = "m", age0 = 42, start_year = 2022)
#' p <- plot_chance_alive_es_aasmr(ca, sex = "m", population = "BG")
#' p
#' }
plot_chance_alive_es_aasmr <- function(ca, sex, population) {
    if (!is.data.frame(ca)) stop_bad_arg("ca", "must be a data frame.")
    need <- c("projection", "Age", "chance_alive")
    missing <- setdiff(need, names(ca))
    if (length(missing)) {
        stop_bad_arg(
            "ca",
            sprintf(
                "must contain column(s) %s; missing %s.",
                paste(sQuote(need), collapse = ", "),
                paste(sQuote(missing), collapse = ", ")
            )
        )
    }
    check_string(population)
    check_string(sex, pattern = "^[mMfF]$")
    sex <- tolower(sex)
    sex_label <- if (sex == "m") gettext("male") else gettext("female")
    age_min <- min(ca[["Age"]], na.rm = TRUE)
    age_max <- max(ca[["Age"]], na.rm = TRUE)
    breaks_major <- seq(age_min, age_max, by = 2L)
    breaks_minor <- seq(age_min, age_max, by = 1L)
    start_year <- if ("Year" %in% names(ca)) min(ca[["Year"]], na.rm = TRUE) else NA_integer_
    ca2 <- ca %>% filter(.data[["projection"]] %in% c("BSL", "LMRT"))
    ggplot2::ggplot(ca2, ggplot2::aes(x = .data[["Age"]],
                                      y = .data[["chance_alive"]],
                                      color = .data[["projection"]])) +
        ggplot2::geom_line(linewidth = 1.0) +
        ggplot2::geom_hline(yintercept = c(0.10, 0.05),
                            linetype = "dashed",
                            linewidth = 0.5,
                            alpha = 0.6) +
        ggplot2::annotate("label",
                          x = Inf,
                          y = 0.10,
                          label = "10%",
                          hjust = 1.05,
                          vjust = -0.4,
                          size = 3) +
        ggplot2::annotate("label",
                          x = Inf,
                          y = 0.05,
                          label = "5%",
                          hjust = 1.05,
                          vjust = -0.4,
                          size = 3) +
        ggplot2::scale_color_manual(values = c(BSL = "black", LMRT = "#60A0C0"),
                                    breaks = c("BSL", "LMRT"),
                                    labels = c(BSL = gettext("baseline"),
                                               LMRT = gettext("lower mortality"))) +
        ggplot2::scale_x_continuous(breaks = breaks_major,
                                    minor_breaks = breaks_minor,
                                    expand = ggplot2::expansion(mult = c(0, 0), add = c(0, 2))) +
        ggplot2::scale_y_continuous(labels = scales::label_percent(accuracy = 1),
                                    limits = c(0, 1)) +
        ggplot2::labs(
            title = gettext("Survival by age per EUROPOP2023 projection"),

            subtitle = glue(start_year = start_year,
                            sex_label = sex_label,
                            gettext("{population} \u00b7 {sex_label} \u00b7 age {age_min} \u00b7 start year {start_year}")), # nolint: line_length_linter
            x = gettext("age"),
            y = gettext("survival"),
            color = gettext("projection")
        ) +
        ggplot2::coord_cartesian(clip = "off") +
        ggplot2::theme_minimal() +
        ggplot2::theme(panel.grid.minor.x = ggplot2::element_line(),
                       plot.margin = ggplot2::margin(5.5, 5.5, 5.5, 5.5))
}
