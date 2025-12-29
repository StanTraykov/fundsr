library(tidyverse)
library(fundsr)
source("common_spec.R")

specs <- tribble(
    ~population, ~sex, ~age,
    "GBR_NP", "m", 42L,
    "NLD",   "m",   42L,
    "NLD",   "f",   42L,
    "BGR",   "m",   42L,
    "BGR",   "f",   42L
)

lt_m <- read_life_table(file.path("data", "life"), "m", look_back = 20)
lt_f <- read_life_table(file.path("data", "life"), "f", look_back = 20)

specs %>%
    mutate(
        lt = if_else(sex == "m", list(lt_m), list(lt_f)),
        ca = pmap(list(lt, population, age), chance_alive),
        p  = pmap(list(ca, sex, population), plot_chance_alive),
        file_id = glue("srv_{population}_{sex}_{age}")
    ) %>%
    select(file_id, p) %>%
    pwalk(\(file_id, p) save_plot(file_id, p, height = std_h, width = std_w, px_width = 1700))

##### Export via Inkscape
# export_pngs()

##### To see available populations:
# pops <- lt_m %>% distinct(PopName) %>% arrange(PopName)
# View(pops) # RStudio
