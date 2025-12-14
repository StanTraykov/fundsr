##### Plots #####
funds <- c("spyy", "iusq", "vwce", "fwra", "webn", "ACWI-GR", "GMLM-GR")
title <- c(en = "Global funds + some gross indices",
           bg = "Глобални фондове + брутни индески")
spyi_title <- c(en = "SPYI in comparison to a few global funds",
                bg = "SPYI в сравнение с някои глобални фондове")
glob_fund_pal <- c("webn" = "red",           "iusq" = "#00BFC4",
                   "spyy" = "#89AE00",       "vwce" = "#600000",
                   "fwra" = "blue",          "fwia" = "blue",
                   "spyi" = "darkgreen",     "acwi" = "pink",
                   "acwu" = "lightblue",     "acwia" = "orange",
                   "GMLM-GR" = "grey50",     "ACWI-GR" = "black",
                   "ACWI_IMI-GR" = "grey50", "scwx" = "#206666",
                   "exus" = "black")
glob_colors <- function(...) {
    scale_color_manual(values = glob_fund_pal, na.value = "grey70", labels = toupper, ...)
}

# plot specification
plot_spec <- tribble(
    ~plot_id, ~title, ~filter,
    ~gg_params, ~width,  ~height,
    ~funds,

    "glob", title, no_filter,
    glob_colors(), std_w, std_h,
    funds,

    "globZ", title, zoom_filter,
    glob_colors(), std_w, std_h,
    funds,

    "globI", spyi_title, no_filter,
    glob_colors(), std_w, std_h,
    c("spyy", "spyi", "vwce", "iusq", "ACWI-GR", "ACWI_IMI-GR")
)
spec_list <- c(spec_list, list(plot_spec))

##### Data #####
add_to_dl_list(c(
    IUSQ = "https://www.ishares.com/uk/individual/en/products/251850/ishares-msci-acwi-ucits-etf/1535604580409.ajax?fileType=xls&fileName=iShares-MSCI-ACWI-UCITS-ETF-USD-Acc_fund&dataType=fund",
    SPYY = "https://www.ssga.com/ie/en_gb/institutional/library-content/products/fund-data/etfs/emea/navhist-emea-en-spyy-gy.xlsx",
    SPYI = "https://www.ssga.com/uk/en_gb/institutional/library-content/products/fund-data/etfs/emea/navhist-emea-en-spyi-gy.xlsx"
))

add_import_fun(function() {
    ####### Indices #######
    setg("gmlm", get_csv("GMLM.csv"))
    setg("gmlm-gr", get_csv("GMLM-GR.csv"), add_fi_pairs = set_names("GMLM", "GMLM-GR"))
    setg("ftaw", get_csv("FTAW.csv"))
    msci(var_name = "msci-nt",
         col_trans = net_idx_trans,
         file = "MSCI-NT.xls")
    msci(var_name = "msci-gr",
         col_trans = gross_idx_trans,
         benchmarks = set_names(names(net_idx_trans), names(gross_idx_trans)),
         file = "MSCI-GR.xls")

    ####### Funds #######
    inve("FWRA", benchmark = "FTAW", retrieve_benchmark = T)
    amun("WEBN", benchmark = "GMLM", file = "NAV History_Amundi Prime All Country World UCITS ETF Acc_IE0003XJA0J9_10_06_2024.xlsx")
    vang("VWCE", benchmark = "FTAW", file = "Historical Prices - Vanguard FTSE All-World UCITS ETF (USD) Accumulating.xlsx")
    spdr("SPYY", benchmark = "ACWI")
    spdr("SPYI", benchmark = "ACWI_IMI")
    ishs("IUSQ", benchmark = "ACWI")
})
