##### Plots #####
funds <- c("praz", "sxr7", "xemu", "c50", "h4zz", "xesc", "sxrt", "EMUEUR-GR", "EZLM-GR")
title <- c(
    en = "Eurozone funds incl. SX5E (selection)",
    bg = "Фондове за Еврозоната вкл. SX5E (селекция)"
)
gg_par <- fund_colors(breaks = funds,
                           special = c(`EMUEUR-GR` = "black", `EZLM-GR` = "grey50"))

# plot specification
plot_spec <- tribble(
    ~plot_id, ~title, ~data_filter,
    ~gg_params, ~width,  ~height,
    ~funds,

    "esemu", title, no_filter,
    gg_par, std_w, std_h,
    funds,

    "esemuZ", title, zoom_filter,
    gg_par, std_w, std_h,
    funds
)

spec_list <- c(spec_list, list(plot_spec))
