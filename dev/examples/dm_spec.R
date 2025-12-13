##### Plots #####
# plot spec helpers
std_w <- 14
std_h <- 9
no_filter <- NULL
zoom_filter <- function(x) {x %>% filter(date >= as_date("2022-01-01"))}
dm_phys <- c("uetw", "sppw", "h4zy", "iwda", "xdwd", "mwrd")

# plot specification
plot_dm <- tribble(
    ~plot_id, ~title, ~filter,
    ~gg_params, ~width,  ~height,
    ~funds,

    "dm", list("DM phys. funds"), no_filter,
    geom_blank(), std_w, std_h,
    dm_phys,

    "dmZ", "DM phys. funds: zoom", zoom_filter,
    geom_blank(), std_w, std_h,
    dm_phys
)

##### Data #####
add_to_dl_list(c(
    IWDA = "https://www.ishares.com/uk/individual/en/products/251882/ishares-msci-world-ucits-etf-acc-fund/1535604580409.ajax?fileType=xls&fileName=iShares-Core-MSCI-World-UCITS-ETF_fund&dataType=fund",
    SPPW = "https://www.ssga.com/uk/en_gb/intermediary/library-content/products/fund-data/etfs/emea/navhist-emea-en-sppw-gy.xlsx"
))

# import function definition
import_dm <- function() {
    ####### Indices #######
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
    hsbc("H4ZY", benchmark = "WORLD", file = "NAV_history _ IE000UQND7H4.xlsx")
    ubs("UETW", benchmark = "WORLD", file = "UBS_UBS Core MSCI World UCITS ETFPrices.xlsx")
    amun("MWRD", benchmark = "WORLD", file = "NAV History_Amundi Core MSCI World UCITS ETF Acc_IE000BI8OT95_18_01_2024.xlsx")
    xtra("XDWD", benchmark = "WORLD", file = "HistoricalData-IE00BJ0KDQ92.xlsx")
    spdr("SPPW", benchmark = "WORLD")
    ishs("IWDA", benchmark = "WORLD")
}

