add_data_loader(function() {
    ##### Interest rates #####
    store_timeseries(var_name = ".estr",
                     expr = read_timeseries("ESTR.csv", date_col = "DATE") |>
                         select("date", contains("EU000A2X2A25")),
                     postprocess = function(d) { names(d) <- c("date", "ESTR"); d }
    )
    store_timeseries(var_name = ".eonia",
                     expr = read_timeseries("EONIA.csv", date_col = "DATE") |>
                         select("date", contains("EONIA_TO")),
                     postprocess = function(d) { names(d) <- c("date", "EONIA"); d }
    )
    ##### MSCI #####
    msci(col_trans = net_idx_trans,
         file = "WUSA_FULL-NT.xls")
    msci(col_trans = gross_idx_trans,
         benchmarks = set_names(names(net_idx_trans), names(gross_idx_trans)),
         file = "WUSA_FULL-GR.xls")
    msci(col_trans = net_idx_trans_ccy("EUR"),
         file = "WUSA_FULL-NT-EUR.xls")
    msci(col_trans = gross_idx_trans_ccy("EUR"),
         benchmarks = set_names(names(net_idx_trans_ccy("EUR")), names(gross_idx_trans_ccy("EUR"))),
         file = "WUSA_FULL-GR-EUR.xls")
    msci_tsv(file = "90479.49.all.xls", index_id = "WORLD_L2-GR")
})
series <- build_all_series()
