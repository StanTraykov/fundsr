#' Read HMD period life tables (1x1) from disk
#'
#' Reads a Human Mortality Database (HMD) period life table file (1x1, by single
#' year of age) for the selected sex and returns only the last `look_back`
#' years based on the latest year present in the file. The open-ended age group
#' (e.g. `"110+"`) is parsed as its numeric lower bound (e.g. `110`).
#'
#' @param directory Directory containing the HMD life table files
#'   (`"mltper_1x1.txt"` / `"fltper_1x1.txt"` or gzipped variants
#'   `"mltper_1x1.txt.gz"` / `"fltper_1x1.txt.gz"`).
#' @param sex Sex code: `"m"` (male) or `"f"` (female).
#' @param look_back Number of most recent years to keep (inclusive of the latest
#'   available year). Must be >= 1.
#'
#' @return A tibble with columns:
#'   `PopName`, `Year`, `Age`, `mx`, `qx`, `ax`, `lx`, `dx`, `Lx`, `Tx`, `ex`.
#'   `Age` is returned as integer.
#'
#' @family survival curve functions
#' @export
#'
#' @examples
#' \dontrun{
#' lt_m <- read_life_table(file.path("data", "life"), sex = "m", look_back = 20)
#' lt_m %>% dplyr::distinct(PopName) %>% dplyr::arrange(PopName)
#' }
read_life_table <- function(directory, sex = c("f", "m"), look_back = 20) {
    sex <- match.arg(sex)
    look_back <- max(1L, as.integer(look_back))

    base <- file.path(directory, paste0(sex, "ltper_1x1"))
    candidates <- c(paste0(base, ".txt"), paste0(base, ".txt.gz"))
    existing <- candidates[file.exists(candidates)]

    if (length(existing) == 0L) {
        stop(
            "No life table file found for sex '", sex, "' in '", directory,
            "'. Expected one of: ", paste(basename(candidates), collapse = ", "),
            call. = FALSE
        )
    }

    file <- existing[[1L]]

    lt <- readr::read_table(
        file,
        skip = 2,
        na = ".",
        col_types = readr::cols(
            PopName = readr::col_character(),
            Year    = readr::col_integer(),
            Age     = readr::col_character(),
            mx      = readr::col_double(),
            qx      = readr::col_double(),
            ax      = readr::col_double(),
            lx      = readr::col_double(),
            dx      = readr::col_double(),
            Lx      = readr::col_double(),
            Tx      = readr::col_double(),
            ex      = readr::col_double()
        ),
        show_col_types = FALSE
    ) %>%
        mutate(Age = as.integer(readr::parse_number(.data[["Age"]])))

    if (nrow(lt) == 0L) {
        return(lt)
    }

    year_max <- max(lt[["Year"]], na.rm = TRUE)
    year_min <- year_max - as.integer(look_back) + 1L
    lt %>% filter(.data[["Year"]] >= year_min)
}

#' Compute conditional survival (chance alive) by age
#'
#' Computes the conditional probability of being alive at each age `x >= age0`,
#' given survival to `age0`, from an HMD-style period life table. For each year,
#' the returned series is:
#' `chance_alive(x | age0) = lx(x) / lx(age0)`.
#'
#' @param lt A life table tibble as returned by [read_life_table()], containing
#'   at least `PopName`, `Year`, `Age`, and `lx`.
#' @param pop_name Population code (HMD `PopName`) to filter within `lt`
#'   (e.g. `"BGR"`, `"USA"`, `"DEUTNP"`).
#' @param age0 Baseline age (integer). Returned ages start at `age0`.
#'
#' @return A tibble with columns `Year`, `Age`, and `chance_alive`, sorted by
#'   `Year` then `Age`.
#'
#' @family survival curve functions
#' @export
#'
#' @examples
#' \dontrun{
#' lt_m <- read_life_table(file.path("data", "life"), sex = "m", look_back = 10)
#' ca <- chance_alive(lt_m, pop_name = "BGR", age0 = 27)
#' ca
#' }
chance_alive <- function(lt, pop_name, age0) {
    age0 <- as.integer(age0)
    lt_pop <- lt %>% filter(.data[["PopName"]] == pop_name)
    base <- lt_pop %>%
        filter(.data[["Age"]] == age0) %>%
        select(all_of("Year"), lx0 = all_of("lx"))
    if (nrow(base) == 0L) stop("`age0` not found for `pop_name` in `lt$Age`.", call. = FALSE)
    lt_pop %>%
        inner_join(base, by = "Year") %>%
        filter(.data[["Age"]] >= age0) %>%
        mutate(chance_alive = .data[["lx"]] / .data[["lx0"]]) %>%
        select(all_of(c("Year", "Age", "chance_alive"))) %>%
        arrange(.data[["Year"]], .data[["Age"]])
}

#' Plot chance alive by age
#'
#' Plots conditional survival curves produced by [chance_alive()]. The most
#' recent year is highlighted in black; earlier years are shown with a
#' teal-to-orange-to-light gradient. Horizontal reference lines mark 10% and 5%
#' survival levels.
#'
#' @param ca A tibble as returned by [chance_alive()], with columns `Year`, `Age`,
#'   and `chance_alive`.
#' @param sex Sex code: `"m"` (male) or `"f"` (female). Used for labeling.
#' @param population Population label/code to display in the subtitle (e.g.
#'   `"BGR"`, `"USA"`, `"DEUTNP"`).
#'
#' @return A ggplot object.
#'
#' @family survival curve functions
#' @export
#'
#' @examples
#' \dontrun{
#' lt_m <- read_life_table(file.path("data", "life"), sex = "m", look_back = 10)
#' ca <- chance_alive(lt_m, pop_name = "BGR", age0 = 27)
#' p <- plot_chance_alive(ca, sex = "m", population = "BGR")
#' p
#' }
plot_chance_alive <- function(ca, sex = c("m", "f"), population) {
    stopifnot(all(c("Year", "Age", "chance_alive") %in% names(ca)))
    sex <- match.arg(sex)
    sex_label <- if (sex == "m") gettext("male") else gettext("female")
    year_max <- max(ca[["Year"]], na.rm = TRUE)
    age_min <- min(ca[["Age"]], na.rm = TRUE)
    age_max <- max(ca[["Age"]], na.rm = TRUE)
    breaks_major <- seq(age_min, age_max, by = 2L)
    breaks_minor <- seq(age_min, age_max, by = 1L)
    ca_past <- ca %>% filter(.data[["Year"]] < year_max)
    ca_last <- ca %>% filter(.data[["Year"]] == year_max)
    ggplot2::ggplot() +
        ggplot2::geom_line(data = ca_past,
                           ggplot2::aes(x = .data[["Age"]], y = .data[["chance_alive"]], group = .data[["Year"]], color = .data[["Year"]]),
                           linewidth = 0.6) +
        ggplot2::geom_line(data = ca_last,
                           ggplot2::aes(x = .data[["Age"]], y = .data[["chance_alive"]]),
                           color = "black",
                           linewidth = 1.1) +
        ggplot2::geom_hline(yintercept = c(0.10, 0.05), linetype = "dashed", linewidth = 0.5, alpha = 0.6) +
        ggplot2::annotate("label", x = age_max, y = 0.10, label = "10%", hjust = 1.05, vjust = -0.4, size = 3) +
        ggplot2::annotate("label", x = age_max, y = 0.05, label = "5%", hjust = 1.05, vjust = -0.4, size = 3) +
        ggplot2::scale_color_gradientn(
            colours = c("#F9F4EE", "#FFE0B2", "#FFB74D", "#FF8F00", "#0A6F74"),
            values  = scales::rescale(c(0, 0.25, 0.55, 0.75, 1))
        ) +
        ggplot2::scale_x_continuous(breaks = breaks_major,
                                    minor_breaks = breaks_minor,
                                    expand = ggplot2::expansion(mult = c(0, 0), add = c(0, 0))) +
        ggplot2::scale_y_continuous(labels = scales::label_percent(accuracy = 1), limits = c(0, 1)) +
        ggplot2::labs(
            title = gettext("Survival by age per HMD life table"),
            subtitle = glue(gettext("{population} \u00b7 {sex_label} \u00b7 age {age_min} \u00b7 latest data from {year_max} (black line)")),
            x = gettext("age"),
            y = gettext("survival"),
            color = gettext("data")
        ) +
        ggplot2::coord_cartesian(clip = "off") +
        ggplot2::theme_minimal() +
        ggplot2::theme(panel.grid.minor.x = ggplot2::element_line(),
                       plot.margin = ggplot2::margin(5.5, 5.5, 5.5, 5.5))
}

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
    file <- file.path(directory, "estat_proj_23naasmr.tsv.gz")
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
        mutate(Year = as.integer(trimws(.data[["Year"]])),
               Age = dplyr::case_when(
                   .data[["age"]] == "Y_LT1" ~ 0L,
                   grepl("^Y\\d+$", .data[["age"]]) ~ as.integer(sub("^Y", "", .data[["age"]])),
                   grepl("^Y_GE\\d+$", .data[["age"]]) ~ as.integer(sub("^Y_GE", "", .data[["age"]])),
                   TRUE ~ NA_integer_
               ),
               mx = readr::parse_number(.data[["mx_raw"]])) %>%
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
    stopifnot(all(c("projection", "sex", "geo", "Age", "Year", "mx") %in% names(es)))
    age0 <- as.integer(age0)
    sex <- toupper(sex)
    if (!sex %in% c("M", "F")) stop("`sex` must be 'm' or 'f'.", call. = FALSE)
    geo <- trimws(geo)
    filtered <- es %>%
        mutate(geo = trimws(.data[["geo"]]),
               sex = trimws(.data[["sex"]]),
               projection = trimws(.data[["projection"]])) %>%
        filter(.data[["geo"]] == .env$geo,
               .data[["sex"]] == .env$sex,
               .data[["projection"]] %in% c("BSL", "LMRT")) %>%
        filter(!is.na(.data[["mx"]]), !is.na(.data[["Year"]]), !is.na(.data[["Age"]]))
    if (nrow(filtered) == 0L) stop("No rows for given geo/sex in `es`.", call. = FALSE)
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
            left_join(d %>% select(all_of(c("Year", "Age", "mx"))), by = c("Year", "Age")) %>%
            arrange(.data[["Year"]], .data[["Age"]])
        if (is.na(out[["mx"]][1])) stop("Missing mx at baseline (Year, Age) for ", proj, ".", call. = FALSE)
        if (anyNA(out[["mx"]])) out <- out[seq_len(which(is.na(out[["mx"]]))[1] - 1L), , drop = FALSE]
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
plot_chance_alive_es_aasmr <- function(ca, sex = c("m", "f"), population) {
    stopifnot(all(c("projection", "Age", "chance_alive") %in% names(ca)))
    sex <- match.arg(sex)
    sex_label <- if (sex == "m") gettext("male") else gettext("female")
    age_min <- min(ca[["Age"]], na.rm = TRUE)
    age_max <- max(ca[["Age"]], na.rm = TRUE)
    breaks_major <- seq(age_min, age_max, by = 2L)
    breaks_minor <- seq(age_min, age_max, by = 1L)
    start_year <- if ("Year" %in% names(ca)) min(ca[["Year"]], na.rm = TRUE) else NA_integer_
    ca2 <- ca %>% filter(.data[["projection"]] %in% c("BSL", "LMRT"))
    ggplot2::ggplot(ca2, ggplot2::aes(x = .data[["Age"]], y = .data[["chance_alive"]], color = .data[["projection"]])) +
        ggplot2::geom_line(linewidth = 1.0) +
        ggplot2::geom_hline(yintercept = c(0.10, 0.05), linetype = "dashed", linewidth = 0.5, alpha = 0.6) +
        ggplot2::annotate("label", x = Inf, y = 0.10, label = "10%", hjust = 1.05, vjust = -0.4, size = 3) +
        ggplot2::annotate("label", x = Inf, y = 0.05, label = "5%", hjust = 1.05, vjust = -0.4, size = 3) +
        ggplot2::scale_color_manual(values = c(BSL = "black", LMRT = "#60A0C0"),
                                    breaks = c("BSL", "LMRT"),
                                    labels = c(BSL = gettext("baseline"), LMRT = gettext("lower mortality"))) +
        ggplot2::scale_x_continuous(breaks = breaks_major,
                                    minor_breaks = breaks_minor,
                                    expand = ggplot2::expansion(mult = c(0, 0), add = c(0, 2))) +
        ggplot2::scale_y_continuous(labels = scales::label_percent(accuracy = 1), limits = c(0, 1)) +
        ggplot2::labs(
            title = gettext("Survival by age per EUROPOP2023 projection"),
            subtitle = glue(gettext("{population} \u00b7 {sex_label} \u00b7 age {age_min} \u00b7 start year {start_year}")),
            x = gettext("age"),
            y = gettext("survival"),
            color = gettext("projection")
        ) +
        ggplot2::coord_cartesian(clip = "off") +
        ggplot2::theme_minimal() +
        ggplot2::theme(panel.grid.minor.x = ggplot2::element_line(),
                       plot.margin = ggplot2::margin(5.5, 5.5, 5.5, 5.5))
}
