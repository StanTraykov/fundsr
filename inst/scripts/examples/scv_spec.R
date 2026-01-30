##### Plots #####
funds <- c("zprx", "zprv", "avws")
title <- c(
    en = "SCV funds",
    bg = "SCV фондове"
)
gg_par <- geom_blank()

# plot specification
plot_spec <- tribble(
    ~plot_id, ~title, ~data_filter,
    ~gg_params, ~width,  ~height,
    ~funds,

    "scv", title, no_filter,
    gg_par, std_w, std_h,
    funds
)
spec_list <- c(spec_list, list(plot_spec))

##### Data #####
add_fund_urls(c(
    ZPRX = "https://www.ssga.com/ie/en_gb/institutional/library-content/products/fund-data/etfs/emea/navhist-emea-en-zprx-gy.xlsx",
    ZPRV = "https://www.ssga.com/se/en_gb/intermediary/library-content/products/fund-data/etfs/emea/navhist-emea-en-zprv-gy.xlsx"
))

add_data_loader(function() {
    ####### Indices #######
    msci(var_name = "msci-scv-nt",
         col_trans = net_idx_trans,
         file = "MSCI-SCV-NT.xls")
    msci(var_name = "msci-scv-gr",
         col_trans = gross_idx_trans,
         benchmarks = set_names(names(net_idx_trans), names(gross_idx_trans)),
         file = "MSCI-SCV-GR.xls")
    msci(var_name = "msci-scv-nt-eur",
         col_trans = net_idx_trans_ccy("EUR"),
         file = "MSCI-SCV-NT-EUR.xls")
    msci(var_name = "msci-scv-gr-eur",
         col_trans = gross_idx_trans_ccy("EUR"),
         benchmarks = set_names(names(net_idx_trans_ccy("EUR")), names(gross_idx_trans_ccy("EUR"))),
         file = "MSCI-SCV-GR-EUR.xls")

    ####### Phys #######
    spdr("ZPRX", benchmark = "EURSCVWEUR")
    spdr("ZPRV", benchmark = "USASCVW")
    avan("AVWS", file = "Avantis-Global-Small-Cap-Value-UCITS-ETF.csv", benchmark = "WSCV")
})
