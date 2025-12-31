##### Plots #####
funds <- c("exus", "wexe", "ixua", "WxUSA-GR")
title <- c(
    en = "World ex USA funds",
    bg = "Фондове World ex USA"
)
gg_par <- fund_colors(breaks = funds,
                      special = c(`WxUSA-GR` = "black"))

# plot specification
plot_spec <- tribble(
    ~plot_id, ~title, ~data_filter,
    ~gg_params, ~width,  ~height,
    ~funds,

    "exus", title, no_filter,
    gg_par, std_w, std_h,
    funds,

    "exusZ", title, zoom_filter,
    gg_par, std_w, std_h,
    funds,
)
spec_list <- c(spec_list, list(plot_spec))

##### Data #####
add_fund_urls(c(
    IXUA = "https://www.ishares.com/ch/professionals/en/products/340748/fund/1535604580403.ajax?fileType=xls&fileName=iShares-MSCI-World-ex-USA-UCITS-ETF-USD-Acc_fund&dataType=fund"
))

add_data_loader(function() {
    ####### Indices #######
    msci(var_name = "msci-nt",
         col_trans = net_idx_trans,
         file = "MSCI-NT.xls")
    msci(var_name = "msci-gr",
         col_trans = gross_idx_trans,
         benchmarks = set_names(names(net_idx_trans), names(gross_idx_trans)),
         file = "MSCI-GR.xls")

    ####### Funds #######
    xtra("EXUS", benchmark = "WxUSA", file = "HistoricalData-IE0006WW1TQ4.xlsx")
    amun("WEXE", benchmark = "WxUSA", file = "NAV History_Amundi MSCI World Ex USA UCITS ETF Acc_IE00085PWS28_03_09_2024.xlsx")
    ishs("IXUA", benchmark = "WxUSA")
})
