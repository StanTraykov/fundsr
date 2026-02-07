##### Plots #####
funds <- c("ijpa", "dbxj", "lcuj", "praj", "sxr5", "zpdj", "sc0i", "JPN-GR", "JPN_IMI-GR", "GBSJJPY-GR")
title <- c(
    en = "Japan funds",
    bg = "Фондове за Япония"
)
gg_par <- fund_colors(breaks = funds,
                      special = c(`JPN-GR` = "black", `JPN_IMI-GR` = "gray75", `GBSJJPY-GR` = "gray40"))

# plot specification
plot_spec <- tribble(
    ~plot_id, ~title, ~data_filter,
    ~gg_params, ~width,  ~height,
    ~funds,

    "japan", title, no_filter,
    gg_par, std_w, std_h,
    funds,

    "japanZ", title, zoom_filter,
    gg_par, std_w, std_h,
    funds,
)
spec_list <- c(spec_list, list(plot_spec))

##### Data #####
add_fund_urls(c(
    IJPA = "https://www.ishares.com/uk/individual/en/products/251867/ishares-msci-japan-ucits-etf-acc-fund/1535604580409.ajax?fileType=xls&fileName=iShares-Core-MSCI-Japan-IMI-UCITS-ETF-USD-Acc_fund&dataType=fund",
    SXR5 = "https://www.ishares.com/uk/individual/en/products/253732/ishares-msci-japan-b-ucits-etf-acc-fund/1535604580409.ajax?fileType=xls&fileName=iShares-MSCI-Japan-UCITS-ETF-USD-Acc_fund&dataType=fund",
    ZPDJ = "https://www.ssga.com/ch/en_gb/intermediary/library-content/products/fund-data/etfs/emea/navhist-emea-en-zpdj-gy.xlsx"
))

add_data_loader(function() {
    ####### Indices #######
    store_timeseries("gbsjjpy", read_timeseries("GBSJJPY.csv"))
    store_timeseries("gbsjjpy-gr", read_timeseries("GBSJJPY-GR.csv"),
                     fund_index_map = set_names("GBSJJPY", "GBSJJPY-GR"))
    msci(var_name = "msci2-nt",
         col_trans = net_idx_trans,
         file = "MSCI2-NT.xls")
    msci(var_name = "msci2-gr",
         col_trans = gross_idx_trans,
         benchmarks = set_names(names(net_idx_trans), names(gross_idx_trans)),
         file = "MSCI2-GR.xls")
    msci(var_name = "msci2-nt-jpy",
         col_trans = net_idx_trans_ccy("JPY"),
         file = "MSCI2-NT-JPY.xls")
    msci(var_name = "msci2-gr-eur",
         col_trans = gross_idx_trans_ccy("JPY"),
         benchmarks = set_names(names(net_idx_trans_ccy("JPY")), names(gross_idx_trans_ccy("JPY"))),
         file = "MSCI2-GR-JPY.xls")

    ####### Funds #######
    ishs(
        "IJPA",
        benchmark = "JPN_IMI",
        postprocess = function(x) {
            # tracked index change (JAPAN -> JAPAN IMI)
            x %>% filter(.data[["date"]] >= lubridate::as_date("2014-05-30"))
        }
    )
    xtra("DBXJ", benchmark = "JPN", file = "HistoricalData-LU0274209740.xlsx")
    amun("LCUJ", benchmark = "JPNJPY", file = "NAV History_Amundi Core MSCI Japan UCITS ETF Acc_LU1781541252_28_02_2018.xlsx")
    amun("PRAJ", benchmark = "GBSJJPY", file = "NAV History_Amundi Prime Japan UCITS ETF DR (C)_LU2089238385_21_01_2020.xlsx")
    ishs("SXR5", benchmark = "JPN")
    spdr("ZPDJ", benchmark = "JPNJPY")
    inve("SC0I", benchmark = "JPN")
})
