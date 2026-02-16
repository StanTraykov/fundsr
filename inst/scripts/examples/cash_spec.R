##### Plots #####
funds <- c("xeon", "edeg", "SOL85", "SOLDLY")
title <- c(
    en = "Cash funds",
    bg = "Кеш фондове"
)
gg_par <- fund_colors(breaks = funds)

# plot specification
plot_spec <- tribble(
    ~plot_id, ~title, ~data_filter,
    ~gg_params, ~width,  ~height,
    ~funds,

    "cash", title, no_filter,
    gg_par, std_w, std_h,
    funds,

    "cashZ", title, zoom_filter,
    gg_par, std_w, std_h,
    funds
)
spec_list <- c(spec_list, list(plot_spec))

##### Data #####
add_data_loader(function() {
    ####### Indices #######
    store_timeseries("solovr", read_timeseries("SOLOVR.csv"))
    store_timeseries("soldly", read_timeseries("SOLDLY.csv"),
                     fund_index_map = set_names("SOLOVR", "SOLDLY"))
    store_timeseries("sol85", read_timeseries("SOL85.csv"),
                     fund_index_map = set_names("SOLOVR", "SOL85"))
    ####### Funds #######
    xtra("XEON", benchmark = "SOL85", file = "HistoricalData-LU0290358497.xlsx")
    bnpp("EDEG", benchmark = "SOLOVR", file = "EDEG.xlsx")
})
