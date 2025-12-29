library(tidyverse)
library(fundsr)
source("common_spec.R")

specs <- tribble(
    ~geo, ~sex, ~age,
    "BG", "m", 42L,
    "BG", "f", 42L,
    "NL", "m", 42L,
    "NL", "f", 42L,
)

es <- read_es_aasmr(file.path("data", "life"))

specs %>%
    mutate(
        ca = pmap(list(geo, sex, age), \(geo, sex, age) chance_alive_es_aasmr(es, geo, sex, age)),
        p  = pmap(list(ca, sex, geo), plot_chance_alive_es_aasmr),
        file_id = glue("esp_{geo}_{sex}_{age}")
    ) %>%
    select(file_id, p) %>%
    pwalk(\(file_id, p) save_plot(file_id, p, height = std_h, width = std_w, px_width = 1700))

##### Export via Inkscape
# export_pngs()
