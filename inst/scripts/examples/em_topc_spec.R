##### Plots #####
idx <- c("CHINA", "TAIWAN", "INDIA", "KOREA", "BRAZIL", "S_AFR")
title <- c(
    en = "EM net indices",
    bg = "нетни индески за разв. п-ри"
)
gg_par <- fund_colors(breaks = idx)

# plot specification
plot_spec <- tribble(
    ~plot_id, ~title, ~data_filter,
    ~gg_params, ~width,  ~height,
    ~funds,

    "em_topc", title, no_filter,
    gg_par, std_w, std_h,
    idx,

    "em_topcZ", title, zoom_filter,
    gg_par, std_w, std_h,
    idx
)
spec_list <- c(spec_list, list(plot_spec))

##### Data #####
add_data_loader(function() {
    ####### Indices #######
    msci(var_name = "msci-em-nt",
         col_trans = net_idx_trans,
         benchmarks = set_names(names(gross_idx_trans), names(net_idx_trans)),
         file = "MSCI-EM-NT.xls")
    msci(var_name = "msci-em-gr",
         col_trans = gross_idx_trans,
         file = "MSCI-EM-GR.xls")
})
