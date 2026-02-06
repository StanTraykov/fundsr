##### Plots #####
funds <- c("zpre", "praz", "mfec", "sxr7", "xemu", "EMUEUR-GR", "EZLM-GR")
title <- c(
    en = "Eurozone funds",
    bg = "Фондове за Еврозоната"
)
gg_par <- fund_colors(breaks = funds,
                           special = c(`EMUEUR-GR` = "black", `EZLM-GR` = "grey50"))

# plot specification
plot_spec <- tribble(
    ~plot_id, ~title, ~data_filter,
    ~gg_params, ~width,  ~height,
    ~funds,

    "emu", title, no_filter,
    gg_par, std_w, std_h,
    funds,

    "emuZ", title, zoom_filter,
    gg_par, std_w, std_h,
    funds
)

spec_list <- c(spec_list, list(plot_spec))

##### Data #####
add_fund_urls(c(
    SXR7 = "https://www.ishares.com/uk/individual/en/products/253729/ishares-msci-emu-ucits-etf/1535604580409.ajax?fileType=xls&fileName=iShares-Core-MSCI-EMU-UCITS-ETF-EUR-Acc_fund&dataType=fund",
    ZPRE = "https://www.ssga.com/de/en_gb/intermediary/library-content/products/fund-data/etfs/emea/navhist-emea-en-zpre-gy.xlsx"
))

add_data_loader(function() {
    ####### Indices #######
    store_timeseries("ezlm", read_timeseries("EZLM.csv"))
    store_timeseries("ezlm-gr", read_timeseries("EZLM-GR.csv"),
                     fund_index_map = set_names("EZLM", "EZLM-GR"))
    msci(var_name = "msci2-nt-eur",
         col_trans = net_idx_trans_ccy("EUR"),
         file = "MSCI2-NT-EUR.xls")
    msci(var_name = "msci2-gr-eur",
         col_trans = gross_idx_trans_ccy("EUR"),
         benchmarks = set_names(names(net_idx_trans_ccy("EUR")), names(gross_idx_trans_ccy("EUR"))),
         file = "MSCI2-GR-EUR.xls")
    ####### Phys #######
    ishs("SXR7", benchmark = "EMUEUR")
    spdr("ZPRE", benchmark = "EMUEUR")
    amun("MFEC", benchmark = "EMUEUR", file = "NAV History_Amundi Core MSCI EMU UCITS ETF Acc_LU1646361276_06_02_2020.xlsx")
    xtra("XEMU", benchmark = "EMUEUR", file = "HistoricalData-LU1920015366.xlsx")
    amun("PRAZ",
        benchmark = "EZLM",
        file = "NAV History_Amundi Prime Eurozone UCITS ETF DR (C)_LU2089238112_21_01_2020.xlsx",
        postprocess = function(x) {
             x %>% filter(.data[["date"]] >= lubridate::as_date("2021-03-16"))
        }
    )
})
