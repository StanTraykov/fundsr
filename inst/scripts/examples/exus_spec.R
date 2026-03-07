##### Plots #####
funds <- c("exus", "wexe", "ixua", "WxUSA-GR")
title <- c(
    en = "World ex USA funds",
    bg = "Фондове World ex USA"
)

funds2 <- c("exus", "wexe", "ixua", "chsi", "WxUSA-GR")
title2 <- c(
    en = "World ex USA funds (incl. low-res CHSI)",
    bg = "Фондове World ex USA (вкл. CHSI в ниска резолюция)"
)

exus_pal <- c("exus" = "#1156cB",
              "wexe" = "#ED0000",
              "ixua" = "#008800",
              "chsi" = "#991188",
              "WxUSA-GR" = "black")

exus_colors <- function(values = exus_pal, ...) {
    scale_color_manual(values = values, na.value = "grey50", labels = toupper, ...)
}

# plot specification
plot_spec <- tribble(
    ~plot_id, ~title, ~data_filter,
    ~gg_params, ~width,  ~height,
    ~funds,

    "exus", title, no_filter,
    exus_colors(), std_w, std_h,
    funds,

    "exusZ", title, zoom_filter,
    exus_colors(), std_w, std_h,
    funds,

    "exus2", title2, no_filter,
    exus_colors(), std_w, std_h,
    funds2,

    "exus2Z", title2, zoom_filter,
    exus_colors(), std_w, std_h,
    funds2,
)
spec_list <- c(spec_list, list(plot_spec))

##### Data #####
add_fund_urls(c(
    IXUA = "https://www.ishares.com/ch/professionals/en/products/340748/fund/1535604580403.ajax?fileType=xls&fileName=iShares-MSCI-World-ex-USA-UCITS-ETF-USD-Acc_fund&dataType=fund"
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

    ####### Funds #######
    xtra("EXUS", benchmark = "WxUSA", file = "HistoricalData-IE0006WW1TQ4.xlsx")
    amun("WEXE", benchmark = "WxUSA", file = "NAV History_Amundi MSCI World Ex USA UCITS ETF Acc_IE00085PWS28_03_09_2024.xlsx")
    ishs("IXUA", benchmark = "WxUSA")
    ubs("CHSI", benchmark = "WxUSA", file = "UBSFunds_Prices_CHSI.xlsx")
})
