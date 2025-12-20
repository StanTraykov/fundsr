##### Plots #####
funds_phys <- c("emim", "xmme", "aeme", "emmusc", "iema", "h4z3", "spym", "pram")
title_phys <- "EM phys. funds"
gg_par_phys <- fund_colors(breaks = funds_phys)

funds_phys2 <- c("emim", "xmme", "aeme", "emmusc", "iema", "spym", "pram", "EM-GR", "EMLM-GR", "EM_IMI-GR")
title_phys2 <- "EM phys. funds (-H4Z3) + gross benchmarks"
gg_par_phys2 <- fund_colors(breaks = funds_phys2,
                            special = c(`EM-GR` = "black", `EMLM-GR` = "grey40", `EM_IMI-GR` = "grey75"))

funds_swap <- c("aeem", "auem", "lem", "leml", "xmem", "emsm", "emgeas", "emim", "lema")
title_swap <- "EM swap funds (EMIM for reference)"
gg_par_swap <- fund_colors(breaks = funds_swap,
                           special = c(emim = "black"))
# plot specification
plot_spec <- tribble(
    ~plot_id, ~title, ~data_filter,
    ~gg_params, ~width,  ~height,
    ~funds,

    "em_phys", title_phys, no_filter,
    gg_par_phys, std_w, std_h,
    funds_phys,

    "em_phys2", title_phys2, no_filter,
    gg_par_phys2, std_w, std_h,
    funds_phys2,

    "em_physZ", title_phys, zoom_filter,
    gg_par_phys, std_w, std_h,
    funds_phys,

    "em_phys2Z", title_phys2, zoom_filter,
    gg_par_phys2, std_w, std_h,
    funds_phys2,

    "em_swap", title_swap, no_filter,
    gg_par_swap, std_w, std_h,
    funds_swap,

    "em_swapZ", title_swap, zoom_filter,
    gg_par_swap, std_w, std_h,
    funds_swap
)
spec_list <- c(spec_list, list(plot_spec))

##### Data #####
add_fund_urls(c(
    EMIM = "https://www.ishares.com/uk/individual/en/products/264659/ishares-msci-emerging-markets-imi-ucits-etf/1535604580409.ajax?fileType=xls&fileName=iShares-Core-MSCI-EM-IMI-UCITS-ETF-USD-Acc_fund&dataType=fund",
    IEMA = "https://www.ishares.com/uk/individual/en/products/251858/ishares-msci-emerging-markets-ucits-etf-acc-fund/1535604580409.ajax?fileType=xls&fileName=iShares-MSCI-EM-UCITS-ETF-USD-Acc_fund&dataType=fund",
    SPYM = "https://www.ssga.com/uk/en_gb/institutional/library-content/products/fund-data/etfs/emea/navhist-emea-en-spym-gy.xlsx"
))

add_data_loader(function() {
    ####### Indices #######
    store_timeseries("emlm", read_timeseries("EMLM.csv"))
    store_timeseries("emlm-gr", read_timeseries("EMLM-GR.csv"),
                     fund_index_map = set_names("EMLM", "EMLM-GR"))
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

    amun("AUEM", benchmark = "EM", file = "NAV History_Amundi MSCI Emerging Markets Swap UCITS ETF USD Acc_LU1681045453_18_04_2018.xlsx")
    amun("LEML", benchmark = "EM", file = "NAV History_Amundi MSCI Emerging Markets Swap II UCITS ETF USD Acc_FR0010435297_01_10_2012.xlsx")
    amun("LEMA", benchmark = "EM", file = "NAV History_Amundi Core MSCI Emerging Markets Swap UCITS ETF Acc_LU2573967036_16_03_2023.xlsx")
    amun("AEME", benchmark = "EMEUR", file = "NAV History_Amundi Core MSCI Emerging Markets UCITS ETF Acc_LU1437017350_17_07_2017.xlsx")
    amun("AEEM", benchmark = "EMEUR", file = "NAV History_Amundi MSCI Emerging Markets Swap UCITS ETF EUR Acc_LU1681045370_18_04_2018.xlsx")
    amun("LEM", benchmark = "EMEUR", file = "NAV History_Amundi MSCI Emerging Markets Swap II UCITS ETF EUR Acc_FR0010429068_01_10_2012.xlsx")
    amun("PRAM", benchmark = "EMLM", file = "NAV History_Amundi Prime Emerging Markets UCITS ETF DR (C)_LU2300295123_14_09_2021.xlsx")
    inve("EMSM", benchmark = "EM")
    xtra("XMME", benchmark = "EM", file = "HistoricalData-IE00BTJRMP35.xlsx")
    xtra("XMEM", benchmark = "EM", file = "HistoricalData-LU0292107645.xlsx")
    ubs("EMMUSC", benchmark = "EM", file = "UBS_UBS Core MSCI EM UCITS ETFPrices.xlsx")
    ubs("EMGEAS", benchmark = "EM", file = "UBS_UBS MSCI EM SF UCITS ETFPrices.xlsx")
    hsbc("H4Z3", benchmark = "EM", file = "NAV_history _ IE000KCS7J59.xlsx")
    spdr("SPYM", benchmark = "EM")
    ishs("IEMA", benchmark = "EM")
    ishs("EMIM", benchmark = "EM_IMI")

})
