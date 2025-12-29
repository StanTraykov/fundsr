library(tidyverse)
source("dev/examples/common_spec.R")

start_from <- 2025

specs <- tribble(
    ~geo, ~sex, ~age, ~start,
    "BG", "m", 42L, start_from,
    "BG", "f", 42L, start_from,
    "NL", "m", 42L, start_from,
    "NL", "f", 42L, start_from
)

es <- read_es_aasmr(file.path("data", "life"))

specs %>%
    mutate(
        ca = pmap(list(geo, sex, age, start), \(geo, sex, age, start) chance_alive_es_aasmr(es, geo, sex, age, start)),
        p  = pmap(list(ca, sex, geo), plot_chance_alive_es_aasmr),
        file_id = glue("esp_{geo}_{sex}_{age}")
    ) %>%
    select(file_id, p) %>%
    pwalk(\(file_id, p) save_plot(file_id, p, height = std_h, width = std_w, px_width = 1700))

##### Export via Inkscape
# export_pngs()
