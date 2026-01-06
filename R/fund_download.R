#' Download fund data according to the configured download list
#'
#' Retrieves all fund data files listed in the `fundsr.fund_urls` option and saves
#' them into the directory specified by `fundsr.data_dir`.
#'
#' @param redownload Logical; if `TRUE`, existing files are overwritten. If
#'   `FALSE`, only missing files are downloaded.
#'
#' @return Invisibly returns `NULL`. Files are written as a side effect.
#'
#' @export
download_fund_data <- function(redownload = FALSE) {
    fund_urls <- getOption("fundsr.fund_urls")
    fund_data_dir <- getOption("fundsr.data_dir", ".")
    download_as(fund_urls, path = fund_data_dir, redownload = redownload)
    invisible(NULL)
}

download_as <- function(named_urls,
                        path = getOption("fundsr.data_dir", "."),
                        redownload = FALSE) {
    if (!dir.exists(path)) dir.create(path, recursive = TRUE)
    files <- sample(names(named_urls)) # randomize order
    for (file in files) {
        url <- named_urls[[file]]
        # hack .xls extension for iShares downloads (not critical but allows Excel to open
        # these files with a warning instead of blocking [outdated XML format])
        ext <- if (grepl("ishares", url, ignore.case = TRUE)) {
            ".xls"
        } else {
            ".xlsx"
        }
        full_file <- file.path(path, paste0(file, ext))
        if (redownload || !file.exists(full_file)) {
            message(glue("Downloading '{file}'"))
            Sys.sleep(stats::runif(1, 0.5, 1.0))
            utils::download.file(url, full_file, mode = "wb")
        } else {
            message(glue("Skipping download '{file}': already exists."))
        }
    }
}
