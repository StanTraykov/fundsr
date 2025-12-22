##### Plots #####
funds <- c("sxr8", "spxs", "vuaa", "spyl", "i500", "xd9u", "sc0h", "USA-GR")
title <- c(
    en = "USA funds (selection)",
    bg = "фондове за САЩ (селекция)"
)
us_sel_pal <- c("sc0h" = "#11569B",
                "i500" = "#ED0000",
                "sxr8" = "#118200",
                "spxs" = "#46B8DA",
                "vuaa" = "#DD7700",
                "USA-GR" = "black",
                "spyl" = "#880088",
                "xd9u" = "#206666")
us_colors <- function(...) {
    scale_color_manual(values = us_sel_pal, na.value = "grey70", labels = toupper, ...)
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
    funds
)
spec_list <- c(spec_list, list(plot_spec))

##### Data #####
add_fund_urls(c(
    SXR8 = "https://www.ishares.com/uk/professional/en/products/253743/ishares-sp-500-b-ucits-etf-acc-fund/1535604580409.ajax?fileType=xls&fileName=iShares-Core-SP-500-UCITS-ETF-USD-Acc_fund&dataType=fund",
    SPYL = "https://www.ssga.com/nl/en_gb/intermediary/library-content/products/fund-data/etfs/emea/navhist-emea-en-spyl-gy.xlsx",
    I500 = "https://www.ishares.com/uk/professional/en/products/314989/fund/1535604580409.ajax?fileType=xls&fileName=iShares-SP-500-Swap-UCITS-ETF-USD-Acc_fund&dataType=fund"
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

    ####### Funds #######
    ishs("SXR8", benchmark = "SP500", retrieve_benchmark = T)
    ishs("I500", benchmark = "SP500")
    spdr("SPYL", benchmark = "SP500")
    inve("SPXS", benchmark = "SP500")
    inve("SC0H", benchmark = "USA")
    xtra("XD9U", benchmark = "USA", file = "HistoricalData-IE00BJ0KDR00.xlsx")
    vang("VUAA", benchmark = "SP500", file = "Historical Prices - Vanguard S&P 500 UCITS ETF (USD) Accumulating.xlsx")
})
