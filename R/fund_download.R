#' Download fund data according to the configured download list
#'
#' Retrieves all fund data files listed in the `fundsr.fund_urls` option and saves them into the
#' directory specified by the `fundsr.data_dir` option.
#'
#' @param redownload Logical; if `TRUE`, existing files are overwritten. If `FALSE`, only missing
#'   files are downloaded.
#'
#' @return Invisibly returns `NULL`. Files are written as a side effect.
#' @seealso [add_fund_urls()] to add/update entries in `fundsr.fund_urls`.
#' @family download functions
#' @export
download_fund_data <- function(redownload = FALSE) {
    fund_urls <- fundsr_get_option("fund_urls")
    fund_data_dir <- fundsr_get_option("data_dir")
    .fundsr_download_as(fund_urls, path = fund_data_dir, redownload = redownload)
    invisible(NULL)
}

#' Download a set of named URLs to disk
#'
#' Internal helper used by [download_fund_data()]. Skips existing files unless `redownload = TRUE`.
#' Returns a named character vector of file paths (in the randomized download order).
#'
#' @keywords internal
#' @noRd
.fundsr_download_as <- function(named_urls,
                                path = fundsr_get_option("data_dir"),
                                redownload = FALSE,
                                attempts = 3L,
                                polite_sleep = TRUE) {
    if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
        stop("`path` must be a length-1 non-empty character string.", call. = FALSE)
    }
    redownload <- isTRUE(redownload)
    polite_sleep <- isTRUE(polite_sleep)
    if (is.null(named_urls) || length(named_urls) == 0L) {
        fundsr_msg("No fund URLs configured (option 'fundsr.fund_urls' is empty).", level = 0L)
        return(invisible(character()))
    }
    nms <- names(named_urls)
    if (is.null(nms) || anyNA(nms) || any(nms == "")) {
        stop("`named_urls` must be a named vector or named list.", call. = FALSE)
    }
    if (anyDuplicated(nms)) {
        stop("`named_urls` must have unique names.", call. = FALSE)
    }
    keys <- withr::with_preserve_seed(sample(nms))
    out <- set_names(character(length(keys)), keys)
    did_download <- FALSE

    for (key in keys) {
        url <- as.character(named_urls[[key]])
        if (length(url) != 1L || is.na(url) || !nzchar(url)) {
            stop(glue("Invalid URL for '{key}'."), call. = FALSE)
        }
        # Hack .xls instead of .xlsx for iShares downloads, which use an outdated XML spreadsheet
        # format. Not critical for fundsr, but it lets Excel open the file (with a warning).
        ext <- if (grepl("ishares", url, ignore.case = TRUE)) ".xls" else ".xlsx"
        full_file <- file.path(path, paste0(key, ext))
        alt_file <- file.path(path, paste0(key, if (ext == ".xls") ".xlsx" else ".xls"))
        if (!redownload && (file.exists(full_file) || file.exists(alt_file))) {
            fundsr_msg(glue("Skipping download '{key}': already exists."), level = 2L)
            out[[key]] <- if (file.exists(full_file)) full_file else alt_file
            next
        }
        if (polite_sleep && did_download) Sys.sleep(stats::runif(1, 0.5, 1.0))
        fundsr_msg(glue("Downloading '{key}'"), level = 1L)
        .fundsr_download(url = url, destfile = full_file, attempts = attempts, quiet = TRUE)
        did_download <- TRUE
        out[[key]] <- full_file
    }

    if (!did_download) {
        fundsr_msg("All downloads skipped due to existing files.")
    }
    invisible(out)
}

#' Download a single URL to a file with retries
#'
#' Internal helper. Downloads to a temporary file in the destination directory and then moves it
#' into place to avoid partial destination files. Uses `curl::curl_download()` if available;
#' otherwise uses `utils::download.file()`.
#'
#' @keywords internal
#' @noRd
.fundsr_download <- function(url,
                             destfile,
                             attempts = 3L,
                             backoff = 0.5,
                             quiet = TRUE,
                             overwrite = TRUE) {
    if (!is.character(url) || length(url) != 1L || is.na(url) || !nzchar(url)) {
        stop("`url` must be a length-1 non-empty character string.", call. = FALSE)
    }
    if (!is.character(destfile) || length(destfile) != 1L || is.na(destfile) || !nzchar(destfile)) {
        stop("`destfile` must be a length-1 non-empty character string.", call. = FALSE)
    }
    attempts <- as.integer(attempts)
    if (length(attempts) != 1L || is.na(attempts) || attempts < 1L) {
        stop("`attempts` must be a single integer >= 1.", call. = FALSE)
    }
    if (!is.numeric(backoff) ||
            length(backoff) != 1L ||
            is.na(backoff) ||
            !is.finite(backoff) ||
            backoff < 0) {
        stop("`backoff` must be a single non-negative finite number.", call. = FALSE)
    }
    quiet <- isTRUE(quiet)
    overwrite <- isTRUE(overwrite)
    destdir <- dirname(destfile)
    if (!dir.exists(destdir)) dir.create(destdir, recursive = TRUE, showWarnings = FALSE)
    if (!overwrite && file.exists(destfile)) return(invisible(destfile))
    tmp <- tempfile(pattern = "fundsr-dl-", tmpdir = destdir)
    on.exit(unlink(tmp, force = TRUE), add = TRUE)
    last_err <- NULL
    status <- NA_integer_

    for (i in seq_len(attempts)) {
        if (file.exists(tmp)) unlink(tmp, force = TRUE)
        status <- tryCatch({
            if (requireNamespace("curl", quietly = TRUE)) {
                curl::curl_download(url, destfile = tmp, quiet = quiet, mode = "wb")
                0L
            } else {
                utils::download.file(url, destfile = tmp, mode = "wb", quiet = quiet)
            }
        }, error = function(e) {
            last_err <<- e
            NA_integer_
        })
        ok <- identical(status, 0L) && file.exists(tmp) && isTRUE(file.info(tmp)$size > 0)
        if (ok) {
            if (file.exists(destfile)) unlink(destfile)
            if (!file.rename(tmp, destfile)) {
                if (!file.copy(tmp, destfile, overwrite = TRUE)) {
                    stop(glue("Download succeeded but couldn't write to '{destfile}'."),
                         call. = FALSE)
                }
                unlink(tmp)
            }
            return(invisible(destfile))
        }
        if (i < attempts) Sys.sleep(backoff * 2^(i - 1L))
    }

    details <- if (!is.null(last_err)) {
        conditionMessage(last_err)
    } else if (is.na(status)) {
        "Unknown download failure."
    } else {
        glue("download returned status {status}.")
    }
    stop(glue("Failed to download '{url}' to '{destfile}' after {attempts} attempt(s): {details}"),
         call. = FALSE)
}
