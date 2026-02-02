##### Plots #####
funds <- c("mse", "c50", "xesc", "sc0d", "h4zz", "sxrt")
title <- c(
    en = "EURO STOXX 50 funds",
    bg = "Фондове следящи EURO STOXX 50"
)
gg_par <- fund_colors(breaks = funds)

# plot specification
plot_spec <- tribble(
    ~plot_id, ~title, ~data_filter,
    ~gg_params, ~width,  ~height,
    ~funds,

    "esx50", title, no_filter,
    gg_par, std_w, std_h,
    funds,

    "esx50Z", title, zoom_filter,
    gg_par, std_w, std_h,
    funds
)

spec_list <- c(spec_list, list(plot_spec))

##### Data #####
add_fund_urls(c(
    SXRT = "https://www.ishares.com/uk/individual/en/products/253712/ishares-euro-stoxx-50-b-ucits-etf-acc-fund/1535604580409.ajax?fileType=xls&fileName=iShares-Core-EURO-STOXX-50-UCITS-ETF-EUR-Acc_fund&dataType=fund"
))

add_data_loader(function() {
    amun("MSE", benchmark = "ESX50", file = "NAV History_Amundi EURO STOXX 50 II UCITS ETF Acc_FR0007054358_01_10_2012.xlsx")
    amun("C50", benchmark = "ESX50", file = "NAV History_Amundi Core EURO STOXX 50 UCITS ETF EUR Acc_LU1681047236_14_02_2018.xlsx")
    xtra("XESC", benchmark = "ESX50", file = "HistoricalData-LU0380865021.xlsx")
    inve("SC0D", benchmark = "ESX50")
    hsbc("H4ZZ", benchmark = "ESX50", file = "NAV_history _ IE000MWUQBJ0.xlsx")
    ishs("SXRT", benchmark = "ESX50", retrieve_benchmark = TRUE)
})
