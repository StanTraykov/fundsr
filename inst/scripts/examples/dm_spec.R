##### Plots #####
funds_phys <- c("iwda", "sppw", "xdwd", "mwrd", "uetw", "h4zy", "WORLD-GR")
title_phys <- "MSCI World phys. funds"
gg_par_phys <- fund_colors(breaks = funds_phys, special = c(`WORLD-GR` = "black"))

funds_swap <- c("iwda", "sppw", "cw8", "dbxw", "wpea", "cw8u", "wldc", "iwds", "sc0j")
title_swap <- "MSCI World swap funds (IWDA, SPPW for reference)"
gg_par_swap <- fund_colors(breaks = funds_swap,
                           special = c(iwda = "black", sppw = "grey50"))
# plot specification
plot_spec <- tribble(
    ~plot_id, ~title, ~data_filter,
    ~gg_params, ~width,  ~height,
    ~funds,

    "dm_phys", title_phys, no_filter,
    gg_par_phys, std_w, std_h,
    funds_phys,

    "dm_physZ", title_phys, zoom_filter,
    gg_par_phys, std_w, std_h,
    funds_phys,

    "dm_swap", title_swap, no_filter,
    gg_par_swap, std_w, std_h,
    funds_swap,

    "dm_swapZ", title_swap, zoom_filter,
    gg_par_swap, std_w, std_h,
    funds_swap
)
spec_list <- c(spec_list, list(plot_spec))

##### Data #####
add_fund_urls(c(
    IWDA = "https://www.ishares.com/uk/individual/en/products/251882/ishares-msci-world-ucits-etf-acc-fund/1535604580409.ajax?fileType=xls&fileName=iShares-Core-MSCI-World-UCITS-ETF_fund&dataType=fund",
    SPPW = "https://www.ssga.com/uk/en_gb/intermediary/library-content/products/fund-data/etfs/emea/navhist-emea-en-sppw-gy.xlsx",
    WPEA = "https://www.ishares.com/ch/professionals/en/products/335178/fund/1535604580403.ajax?fileType=xls&fileName=iShares-MSCI-World-Swap-PEA-UCITS-ETF-EUR-Acc_fund&dataType=fund",
    IWDS = "https://www.ishares.com/uk/individual/en/products/335184/fund/1535604580409.ajax?fileType=xls&fileName=iShares-MSCI-World-Swap-UCITS-ETF-USD-Acc_fund&dataType=fund"
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
    msci(var_name = "msci-nt-eur",
         col_trans = net_idx_trans_ccy("EUR"),
         file = "MSCI-NT-EUR.xls")
    msci(var_name = "msci-gr-eur",
         col_trans = gross_idx_trans_ccy("EUR"),
         benchmarks = set_names(names(net_idx_trans_ccy("EUR")), names(gross_idx_trans_ccy("EUR"))),
         file = "MSCI-GR-EUR.xls")

    ####### Phys #######
    amun("MWRD", benchmark = "WORLD", file = "NAV History_Amundi Core MSCI World UCITS ETF Acc_IE000BI8OT95_18_01_2024.xlsx")
    hsbc("H4ZY", benchmark = "WORLD", file = "NAV_history _ IE000UQND7H4.xlsx")
    ishs("IWDA", benchmark = "WORLD")
    ubs("UETW", benchmark = "WORLD", file = "UBS_UBS Core MSCI World UCITS ETFPrices.xlsx")
    spdr("SPPW", benchmark = "WORLD")
    xtra("XDWD", benchmark = "WORLD", file = "HistoricalData-IE00BJ0KDQ92.xlsx")

    ####### Swap #######
    amun("CW8", benchmark = "WORLDEUR", file = "NAV History_Amundi MSCI World Swap UCITS ETF EUR Acc_LU1681043599_18_04_2018.xlsx")
    amun("CW8U", benchmark = "WORLD", file = "NAV History_Amundi MSCI World Swap UCITS ETF USD Acc_LU1681043672_18_04_2018.xlsx")
    amun("WLDC", benchmark = "WORLDEUR", file = "NAV History_Amundi MSCI World Swap II UCITS ETF Acc_FR0014003IY1_02_06_2021.xlsx")
    amun("DCAM", benchmark = "WORLDEUR", file = "NAV History_Amundi PEA Monde (MSCI World) UCITS ETF_FR001400U5Q4_04_03_2025.xlsx")
    inve("SC0J", benchmark = "WORLD")
    ishs("WPEA", benchmark = "WORLDEUR")
    ishs("IWDS", benchmark = "WORLD")
    xtra("DBXW", benchmark = "WORLD", file = "HistoricalData-LU0274208692.xlsx")
})
