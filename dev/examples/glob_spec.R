##### Plots #####
phys_h_funds <- c("spyy", "iusq", "vwce", "fwra", "webn", "scwx", "ACWI-GR", "GMLM-GR")
phys_s_funds <- c("spyy", "iusq", "vwce", "fwra", "webn", "acwia", "acwi", "acwu", "ACWI-GR", "GMLM-GR")
phys_h_title <- c(en = "Global funds (phys. and hybrid) + some gross indices",
           bg = "Глобални фондове (физ. и хибридни) + брутни индекси")
phys_s_title <- c(en = "Global funds (phys. and swap) + some gross indices",
                  bg = "Глобални фондове (физ. и суап) + брутни индекси")
spyi_title <- c(en = "SPYI in comparison to a few global funds",
                bg = "SPYI в сравнение с някои глобални фондове")
glob_fund_pal <- c("webn" = "red",           "iusq" = "#00BFC4",
                   "spyy" = "#89AE00",       "vwce" = "#600000",
                   "spyi" = "darkgreen",     "acwi" = "pink",
                   "acwu" = "#ADD8E6",       "acwia" = "orange",
                   "GMLM-GR" = "grey50",     "ACWI-GR" = "black",
                   "ACWI_IMI-GR" = "grey50", "scwx" = "orange",
                   "fwra" = "blue"
)
glob_colors <- function(...) {
    scale_color_manual(values = glob_fund_pal, na.value = "grey70", labels = toupper, ...)
}

# plot specification
plot_spec <- tribble(
    ~plot_id, ~title, ~data_filter,
    ~gg_params, ~width,  ~height,
    ~funds,

    "glob_ph", phys_h_title, no_filter,
    glob_colors(), std_w, std_h,
    phys_h_funds,

    "glob_phZ", phys_h_title, zoom_filter,
    glob_colors(), std_w, std_h,
    phys_h_funds,

    "glob_ps", phys_s_title, no_filter,
    glob_colors(), std_w, std_h,
    phys_s_funds,

    "glob_psZ", phys_s_title, zoom_filter,
    glob_colors(), std_w, std_h,
    phys_s_funds,

    "globI", spyi_title, no_filter,
    glob_colors(), std_w, std_h,
    c("spyy", "spyi", "vwce", "iusq", "ACWI-GR", "ACWI_IMI-GR")
)
spec_list <- c(spec_list, list(plot_spec))

##### Data #####
add_fund_urls(c(
    IUSQ = "https://www.ishares.com/uk/individual/en/products/251850/ishares-msci-acwi-ucits-etf/1535604580409.ajax?fileType=xls&fileName=iShares-MSCI-ACWI-UCITS-ETF-USD-Acc_fund&dataType=fund",
    SPYY = "https://www.ssga.com/ie/en_gb/institutional/library-content/products/fund-data/etfs/emea/navhist-emea-en-spyy-gy.xlsx",
    SPYI = "https://www.ssga.com/uk/en_gb/institutional/library-content/products/fund-data/etfs/emea/navhist-emea-en-spyi-gy.xlsx"
))

add_data_loader(function() {
    ####### Indices #######
    store_timeseries("gmlm", read_timeseries("GMLM.csv"))
    store_timeseries("gmlm-gr", read_timeseries("GMLM-GR.csv"),
                     fund_index_map = set_names("GMLM", "GMLM-GR"))
    store_timeseries("ftaw", read_timeseries("FTAW.csv"))
    msci(var_name = "msci-nt",
         col_trans = net_idx_trans,
         file = "MSCI-NT.xls")
    msci(var_name = "msci-gr",
         col_trans = gross_idx_trans,
         benchmarks = set_names(names(net_idx_trans), names(gross_idx_trans)),
         file = "MSCI-GR.xls")

    # Phys / hybrid
    inve("FWRA", benchmark = "FTAW", retrieve_benchmark = T)
    amun("WEBN", benchmark = "GMLM", file = "NAV History_Amundi Prime All Country World UCITS ETF Acc_IE0003XJA0J9_10_06_2024.xlsx")
    vang("VWCE", benchmark = "FTAW", file = "Historical Prices - Vanguard FTSE All-World UCITS ETF (USD) Accumulating.xlsx")
    xtra("SCWX", benchmark = "ACWI", file = "HistoricalData-LU2903252349.xlsx")
    spdr("SPYY", benchmark = "ACWI")
    spdr("SPYI", benchmark = "ACWI_IMI")
    ishs("IUSQ", benchmark = "ACWI")
    # Swap
    amun("ACWU", benchmark = "ACWI", file = "NAV History_Amundi MSCI All Country World UCITS ETF USD Acc_LU1829220133_03_10_2012.xlsx")
    amun("ACWI", benchmark = "ACWIEUR", file = "NAV History_Amundi MSCI All Country World UCITS ETF EUR Acc_LU1829220216_03_10_2012.xlsx")
    ubs("ACWIA", benchmark = "ACWI", file = "UBS_UBS MSCI ACWI SF UCITS ETFPrices.xlsx")
})
