##### Plots #####
funds <- c("iwda", "sppw", "xdwd", "mwrd", "uetw", "h4zy", "WORLD-GR")
title <- "MSCI World phys. funds"
dm_pal <- c(c(`WORLD-GR` = "black"), fund_palette)
gg_par <- fund_colors(breaks = funds, special = c(`WORLD-GR` = "black"))
# plot specification
plot_spec <- tribble(
    ~plot_id, ~title, ~filter,
    ~gg_params, ~width,  ~height,
    ~funds,

    "dm_phys", title, no_filter,
    gg_par, std_w, std_h,
    funds,

    "dm_physZ", title, zoom_filter,
    gg_par, std_w, std_h,
    funds
)
spec_list <- c(spec_list, list(plot_spec))

##### Data #####
add_to_dl_list(c(
    IWDA = "https://www.ishares.com/uk/individual/en/products/251882/ishares-msci-world-ucits-etf-acc-fund/1535604580409.ajax?fileType=xls&fileName=iShares-Core-MSCI-World-UCITS-ETF_fund&dataType=fund",
    SPPW = "https://www.ssga.com/uk/en_gb/intermediary/library-content/products/fund-data/etfs/emea/navhist-emea-en-sppw-gy.xlsx"
))

add_import_fun(function() {
    ####### Indices #######
    msci(var_name = "msci-nt",
         col_trans = net_idx_trans,
         file = "MSCI-NT.xls")
    msci(var_name = "msci-gr",
         col_trans = gross_idx_trans,
         benchmarks = set_names(names(net_idx_trans), names(gross_idx_trans)),
         file = "MSCI-GR.xls")

    ####### Funds #######
    hsbc("H4ZY", benchmark = "WORLD", file = "NAV_history _ IE000UQND7H4.xlsx")
    ubs("UETW", benchmark = "WORLD", file = "UBS_UBS Core MSCI World UCITS ETFPrices.xlsx")
    amun("MWRD", benchmark = "WORLD", file = "NAV History_Amundi Core MSCI World UCITS ETF Acc_IE000BI8OT95_18_01_2024.xlsx")
    xtra("XDWD", benchmark = "WORLD", file = "HistoricalData-IE00BJ0KDQ92.xlsx")
    spdr("SPPW", benchmark = "WORLD")
    ishs("IWDA", benchmark = "WORLD")
})
