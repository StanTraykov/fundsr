library(tidyverse)
library(fundsr)
source("common_spec.R")

specs <- tribble(
    ~population, ~sex, ~age,
    "NLD",   "m",   52L,
    "IRL",   "m",   52L,
    "NZL_MA",    "f",   32L,
    "DEUTNP",    "m",   42L,
)

lt_m <- read_life_table(file.path("data", "life"), "m", look_back = 20)
lt_f <- read_life_table(file.path("data", "life"), "f", look_back = 20)

specs %>%
    mutate(
        lt = if_else(sex == "m", list(lt_m), list(lt_f)),
        ca = pmap(list(lt, population, age), chance_alive),
        p  = pmap(list(ca, sex, population), plot_chance_alive),
        file_id = glue("CA_{population}_{sex}_{age}")
    ) %>%
    select(file_id, p) %>%
    pwalk(\(file_id, p) save_plot(file_id, p, height = std_h, width = std_w, px_width = 1700))

##### Export via Inkscape
# export_pngs()

##### To see available populations:
# pops <- lt_m %>% distinct(PopName) %>% arrange(PopName)
# View(pops) # RStudio
