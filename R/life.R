#' Read HMD period life tables (1x1) from disk
#'
#' Reads a Human Mortality Database (HMD) period life table file (1x1, by single
#' year of age) for the selected sex and returns only the last `look_back`
#' years based on the latest year present in the file. The open-ended age group
#' (e.g. `"110+"`) is parsed as its numeric lower bound (e.g. `110`).
#'
#' @param directory Directory containing the HMD life table files (e.g.
#'   `"mltper_1x1.txt"` / `"fltper_1x1.txt"` or `"mltper_1x1.txt"` / `"fltper_1x1.txt"`
#'   depending on your naming convention). This function expects files named
#'   `paste0(sex, "ltper_1x1.txt")`, where `sex` is `"m"` or `"f"`.
#' @param sex Sex code: `"m"` (male) or `"f"` (female).
#' @param look_back Number of most recent years to keep (inclusive of the latest
#'   available year). Must be >= 1.
#'
#' @return A tibble with columns:
#'   `PopName`, `Year`, `Age`, `mx`, `qx`, `ax`, `lx`, `dx`, `Lx`, `Tx`, `ex`.
#'   `Age` is returned as integer.
#'
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
    file <- file.path(directory, paste0(sex, "ltper_1x1.txt"))
    lt <- readr::read_table(file,
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
                            show_col_types = FALSE) %>%
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
        dplyr::inner_join(base, by = "Year") %>%
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
    sex_label <- if (sex == "m") "male" else "female"
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
            title = "Chance alive by age",
            subtitle = glue("{population} \u00b7 {sex_label} \u00b7 age {age_min} \u00b7 latest data from {year_max} (black line)"),
            x = "age",
            y = "chance alive",
            color = "data"
        ) +
        ggplot2::coord_cartesian(clip = "off") +
        ggplot2::theme_minimal() +
        ggplot2::theme(panel.grid.minor.x = ggplot2::element_line(),
                       plot.margin = ggplot2::margin(5.5, 5.5, 5.5, 5.5))
}





