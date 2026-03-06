##### Plots #####
funds <- c("sxr8", "spxs", "vuaa", "spyl", "i500", "sc0h", "sp5c", "USA-GR", "SP500-GR")
funds_swap <- c("xmus", "spxs", "musd", "d5bm", "i500", "sc0h", "sp5c", "esd", "500u")
funds_pea <- c("spea", "esd", "ese", "psp5", "i500")

title <- c(
    en = "USA funds (selection)",
    bg = "фондове за САЩ (селекция)"
)
title_swap <- c(
    en = "USA funds (swap)",
    bg = "фондове за САЩ (суап)"
)
title_pea <- c(
    en = "USA funds (PEA; I500 for comparsion)",
    bg = "фондове за САЩ (PEA; I500 за сравнение)"
)
us_sel_pal <- c("sc0h" = "#1156cB",
                "i500" = "#ED0000",
                "sxr8" = "#008200",
                "spxs" = "#46B8DA",
                "vuaa" = "#DD7700",
                "USA-GR" = "black",
                "sp5c" = "#880088",
                "SP500-GR" = "gray50",
                "spyl" = "#206666")
us_swap_pal <- c("sc0h" = "#1156cB",
                "i500" = "#ED0000",
                "musd" = "#008200",
                "spxs" = "#46B8DA",
                "xmus" = "#DD7700",
                "esd" = "black",
                "sp5c" = "#880088",
                "d5bm" = "gray50",
                "500u" = "#22cc44")
us_pea_pal <- c("ese" = "#1156cB",
                 "i500" = "#ED0000",
                 "spea" = "#DD7700",
                 "psp5" = "#46B8DA",
                 "esd" = "black")
us_colors <- function(values = us_sel_pal, ...) {
    scale_color_manual(values = values, na.value = "grey70", labels = toupper, ...)
}

# plot specification
plot_spec <- tribble(
    ~plot_id, ~title, ~data_filter,
    ~gg_params, ~width,  ~height,
    ~funds,

    "usa_sel", title, no_filter,
    us_colors(), std_w, std_h,
    funds,

    "usa_selZ", title, zoom_filter,
    us_colors(), std_w, std_h,
    funds,

    "usa_swap", title_swap, no_filter,
    us_colors(values = us_swap_pal), std_w, std_h,
    funds_swap,

    "usa_swapZ", title_swap, zoom_filter,
    us_colors(values = us_swap_pal), std_w, std_h,
    funds_swap,

    "usa_pea", title_pea, no_filter,
    us_colors(values = us_pea_pal), std_w, std_h,
    funds_pea,

    "usa_peaZ", title_pea, zoom_filter,
    us_colors(values = us_pea_pal), std_w, std_h,
    funds_pea
)
spec_list <- c(spec_list, list(plot_spec))

##### Data #####
add_fund_urls(c(
    SXR8 = "https://www.ishares.com/uk/professional/en/products/253743/ishares-sp-500-b-ucits-etf-acc-fund/1535604580409.ajax?fileType=xls&fileName=iShares-Core-SP-500-UCITS-ETF-USD-Acc_fund&dataType=fund",
    SPYL = "https://www.ssga.com/nl/en_gb/intermediary/library-content/products/fund-data/etfs/emea/navhist-emea-en-spyl-gy.xlsx",
    I500 = "https://www.ishares.com/uk/professional/en/products/314989/fund/1535604580409.ajax?fileType=xls&fileName=iShares-SP-500-Swap-UCITS-ETF-USD-Acc_fund&dataType=fund",
    SPEA = "https://www.ishares.com/ch/professionals/en/products/342916/fund/1535604580403.ajax?fileType=xls&fileName=iShares-SP-500-Swap-PEA-UCITS-ETF-EUR-Acc_fund&dataType=fund",
    MUSD = "https://www.ishares.com/uk/individual/en/products/329501/fund/1535604580409.ajax?fileType=xls&fileName=iShares-MSCI-USA-Swap-UCITS-ETF_fund&dataType=fund"
))

add_data_loader(function() {
    ####### Indices #######
    msci(var_name = "msci2-nt",
         col_trans = net_idx_trans,
         file = "MSCI2-NT.xls")
    msci(var_name = "msci2-gr",
         col_trans = gross_idx_trans,
         benchmarks = set_names(names(net_idx_trans), names(gross_idx_trans)),
         file = "MSCI2-GR.xls")
    spdj(var_name = "sp500",
         col_trans = net_idx_trans,
         file = "SP500-NT.xls")
    spdj(var_name = "sp500-gr",
         col_trans = gross_idx_trans,
         benchmarks = set_names(names(net_idx_trans), names(gross_idx_trans)),
         file = "SP500-GR.xls")
    spdj(var_name = "sp500-nt-eur",
         col_trans = net_idx_trans_ccy("EUR"),
         file = "SP500-NT-EUR.xls")
    spdj(var_name = "sp500-gr-eur",
         col_trans = gross_idx_trans_ccy("EUR"),
         benchmarks = set_names(names(net_idx_trans_ccy("EUR")), names(gross_idx_trans_ccy("EUR"))),
         file = "SP500-GR-EUR.xls")

    ####### Funds #######
    #ishs("SXR8", benchmark = "SP500", retrieve_benchmark = T)
    ishs("MUSD", benchmark = "USA")
    ishs("SXR8", benchmark = "SP500")
    ishs("I500", benchmark = "SP500")
    #ishs("SPEA", benchmark = "SP500EUR", retrieve_benchmark = T)
    ishs("SPEA", benchmark = "SP500EUR")
    spdr("SPYL", benchmark = "SP500")
    inve("SPXS", benchmark = "SP500")
    inve("SC0H", benchmark = "USA")
    xtra("XD9U", benchmark = "USA", file = "HistoricalData-IE00BJ0KDR00.xlsx")
    xtra("XMUS", benchmark = "USA", file = "HistoricalData-LU0274210672.xlsx")
    xtra("D5BM", benchmark = "SP500", file = "HistoricalData-LU0490618542.xlsx")
    vang("VUAA", benchmark = "SP500", file = "Historical Prices - Vanguard S&P 500 UCITS ETF (USD) Accumulating.xlsx")
    amun("SP5C", benchmark = "SP500EUR", file = "NAV History_Amundi Core S&P 500 Swap UCITS ETF Acc_LU1135865084_09_12_2014.xlsx")
    amun("500U", benchmark = "SP500", file = "NAV History_Amundi S&P 500 Swap UCITS ETF USD Acc_LU1681049018_22_03_2018.xlsx")
    amun("PSP5", benchmark = "SP500EUR", file = "NAV History_Amundi PEA S&P 500 UCITS ETF Acc_FR0011871128_20_05_2014.xlsx")
    bnpp("ESD", benchmark = "SP500")
    bnpp("ESE", benchmark = "SP500EUR")
})
