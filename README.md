# Overview
fundsr imports fund NAVs and index levels and creates tracking difference plots (CAGR and log-return). Plotting Xetra liquidity data (XLM) is also supported.

# Installation
``` r
# install.packages("pak")
pak::pak("StanTraykov/fundsr")
```

# Usage
* [Intro vignette](https://stantraykov.github.io/fundsr/articles/fundsr-intro.html) contains a simple walkthrough comparing IUSQ and SPYY (no data setup needed)
* `inst/scripts/examples` (e.g. open `.Rproj` file in R Studio after installing the package)
  * `glob_funds.R` a more comprehensive example featuring global funds but requiring manual index & fund data setup outside the package
  * `all_funds.R` an example containing the global funds and others
* [Reference](https://stantraykov.github.io/fundsr/reference/index.html)

# Importing data
## Funds
### Supported formats (Amundi, HSBC, Invesco, iShares, SPDR, UBS, Xtrackers)
Fund providers allow downloading a NAV history in Excel format (even if it's sometimes incomplete). iShares and SPDR downloads can be automated easily and fundsr supports this via the [`add_to_dl_list()`](https://stantraykov.github.io/fundsr/reference/add_to_dl_list.html) and [`dl_funds()`](https://stantraykov.github.io/fundsr/reference/dl_funds.html) functions—see the intro vignette for more info. For the others, manual downloads (or non-trivial automation) seem necessary. In this case, the fund `.xls` or `.xlsx` file must be made available to fundsr in the data directory. If the filename matches the ticker (e.g. `FWRA.xlsx`) it can be imported without specifying a file.
```
inve("FWRA", benchmark = "FTAW")
amun("WEBN", benchmark = "GMLM", file = "NAV History_Amundi Prime All Country World UCITS ETF Acc_IE0003XJA0J9_10_06_2024.xlsx")
```
### Other funds
Prepare a tibble/dataframe or a CSV file with a date column and one or more data columns that must be lowercase for funds (e.g. `sxr8`, `vwce`, `spyy`) and uppercase for indices (e.g. `SP500`, `FTAW`, `ACWI`). Suppose you have a dataframe ``funds_data`` with fund NAVs and a CSV file with index levels. You can import them via:
```
fi_map <- c(
    fwra = "FTAW",
    webn = "GMLM",
    vwce = "FTAW", 
    spyy = "ACWI",
    spyi = "ACWI_IMI",
    iusq = "ACWI")

setg("funds", funds_data, add_fi_pairs = fi_map)
setg("indices", get_csv("indices.csv"))
```
Note: dates in the CSV file must be Unix timestamps (in second or millisecond precision), see [`get_csv()`](https://stantraykov.github.io/fundsr/reference/get_csv.html).
## Indices
### From fund files
Some fund providers' files include index series. These can be retrieved when importing the fund (supported for iShares, Xtrackers, Invesco), e.g.
```
import_fun <- function() {
    spdr("SPYY", benchmark = "ACWI")
    ishs("IUSQ", benchmark = "ACWI", retrieve_benchmark = TRUE) # retrieve ACWI (R) from fund file
    inve("FWRA", benchmark = "FTAW", retrieve_benchmark = TRUE) # retrieve FTSE All-World (R) from fund file
}
```
Index series retrieved in this way may have holes (e.g. fund domicile holidays and such) that can potentially remove data points for funds that did publish a NAV for that day (e.g. different domicile). The overall effect on plots is negligible, however.
### MSCI
MSCI provides [end-of-day level downloads](https://www-cdn.msci.com/web/msci/index-tools/end-of-day-index-data-search). These can be imported by the provided [`msci()`](https://stantraykov.github.io/fundsr/reference/msci.html) function.
### FTSE & Solactive
These index providers make it more difficult to get index levels for free.

# Disclaimer
This project is provided for informational and analytical purposes (commentary and performance comparison). Charts and statistics shown are derived measures (e.g., tracking differences/return differentials) and are not intended to reproduce or redistribute underlying benchmark or index datasets. Any third-party material, where present, is shown only as necessary for identification and discussion and is provided as low-resolution raster images (not high-resolution or vector graphics) to prevent reuse as a substitute for source data.
All trademarks and registered trademarks are the property of their respective owners. References to products, indices, and providers are for identification purposes only and do not imply endorsement or affiliation. Use is intended to fall within applicable copyright exceptions/limitations (such as quotation for criticism/review), to the extent permitted by law.
