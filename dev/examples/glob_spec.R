##### Plots #####
glob_funds <- c("spyy", "iusq", "vwce", "fwra", "webn", "ACWI-GR", "GMLM-GR")
glob_funds_title <- c(en = "Global funds + some gross indices",
                      bg = "Глобални фондове + брутни индески")
glob_spyi_title <- c(en = "SPYI in comparison to a few global funds",
                     bg = "SPYI в сравнение с някои глобални фондове")
glob_fund_pal <- c("WEBN" = "red",           "IUSQ" = "#00BFC4",
                   "SPYY" = "#89AE00",       "VWCE" = "#600000",
                   "FWRA" = "blue",          "FWIA" = "blue",
                   "SPYI" = "darkgreen",     "ACWI" = "pink",
                   "ACWU" = "lightblue",     "ACWIA" = "orange",
                   "GMLM-GR" = "grey50",     "ACWI-GR" = "black",
                   "ACWI_IMI-GR" = "grey50", "SCWX" = "#206666",
                   "EXUS" = "black")

# plot specification
plot_glob <- tribble(
    ~plot_id, ~title, ~filter,
    ~gg_params, ~width,  ~height,
    ~funds,

    "glob", glob_funds_title, no_filter,
    scale_color_manual(values = glob_fund_pal), std_w, std_h,
    glob_funds,

    "globZ", glob_funds_title, zoom_filter,
    scale_color_manual(values = glob_fund_pal), std_w, std_h,
    glob_funds,

    "globI", glob_spyi_title, no_filter,
    scale_color_manual(values = glob_fund_pal), std_w, std_h,
    c("spyy", "spyi", "vwce", "iusq", "ACWI-GR", "ACWI_IMI-GR")
)

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
    net_idx_trans <- c(
        WORLD = "^WORLD Standard",
        ACWI = "^ACWI Standard",
        ACWI_IMI = "^ACWI IMI"
    )
    gross_idx_trans <- set_names(net_idx_trans, paste0(names(net_idx_trans), "-GR"))
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
