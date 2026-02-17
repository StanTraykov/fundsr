#' Download fund data according to the configured download list
#'
#' Retrieves all Excel files with fund identifiers and URLs listed in the `fundsr.fund_urls` option
#' and saves them into the directory specified by the `fundsr.data_dir` option.
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
    .fundsr_download_as(fund_urls,
                        path = fund_data_dir,
                        redownload = redownload,
                        hint = missing(redownload))
    invisible(NULL)
}

#' Download a set of named URLs pointing to Excel downloads
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
                                polite_sleep = TRUE,
                                hint = FALSE) {
    check_string(path)
    check_logical(redownload)
    attempts <- check_numeric_scalar(attempts, as_integer = TRUE, ge = 1L)
    check_logical(polite_sleep)
    check_logical(hint)
    named_urls <- check_mapping(
        named_urls,
        allow_null = TRUE,
        allow_empty = TRUE,
        type = "character",
        scalar_values = TRUE,
        unique_case_insensitive = TRUE
    )

    if (!length(named_urls)) {
        fundsr_msg("No fund URLs configured (option 'fundsr.fund_urls' is empty).", level = 0L)
        return(invisible(character()))
    }

    if (!dir.exists(path)) {
        ok <- dir.create(path, recursive = TRUE, showWarnings = FALSE)
        if (!ok || !dir.exists(path)) {
            fundsr_abort(
                msg = c(
                    "Cannot download fund files: directory could not be created.",
                    sprintf("path = %s.", sQuote(path))
                ),
                class = "fundsr_io_error"
            )
        }
    }

    nms <- names(named_urls)
    check_string(
        nms,
        "names(named_urls)",
        n = NULL,
        allow_empty = FALSE,
        pattern = "^[^<>:\"/\\\\|?*[:cntrl:]]*[^<>:\"/\\\\|?*[:cntrl:] .]$" # safe filenames
    )
    keys <- withr::with_preserve_seed(sample(nms))
    out <- set_names(character(length(keys)), keys)
    did_download <- FALSE

    for (key in keys) {
        url <- check_string(
            named_urls[[key]],
            arg = paste0("named_urls[[", sQuote(key), "]]"),
            trim = TRUE
        )
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
        tryCatch(
            .fundsr_download(url = url, destfile = full_file, attempts = attempts, quiet = TRUE),
            error = function(e) {
                fundsr_abort(
                    msg    = glue("Download failed for '{key}'."),
                    class  = "fundsr_download_failed",
                    parent = e
                )
            }
        )
        did_download <- TRUE
        out[[key]] <- full_file
    }

    if (!did_download) {
        msg <- "Downloads skipped: all files already exist."
        if (hint) {
            msg <- paste(msg, "Use redownload = TRUE to overwrite.")
        }
        fundsr_msg(msg)
    }
    invisible(out)
}

#' Download a single URL to a file with retries
#'
#' Internal helper. Downloads to a temporary file in the destination directory and then moves it
#' into place. Uses `curl::curl_download()` if available; otherwise uses `utils::download.file()`.
#'
#' @keywords internal
#' @noRd
.fundsr_download <- function(url,
                             destfile,
                             attempts = 3L,
                             backoff = 0.5,
                             quiet = TRUE,
                             overwrite = TRUE) {
    check_string(url)
    check_string(destfile)
    attempts <- check_numeric_scalar(attempts, as_integer = TRUE, ge = 1L)
    backoff  <- check_numeric_scalar(backoff, ge = 0)
    check_logical(quiet)
    check_logical(overwrite)

    destdir <- dirname(destfile)
    if (!dir.exists(destdir)) {
        ok <- dir.create(destdir, recursive = TRUE, showWarnings = FALSE)
        if (!ok && !dir.exists(destdir)) {
            fundsr_abort(
                msg = c(
                    "Cannot download: destination directory could not be created.",
                    sprintf("destdir = %s.", sQuote(destdir))
                ),
                class = "fundsr_io_error"
            )
        }
    }

    if (!overwrite && file.exists(destfile)) {
        return(invisible(destfile))
    }

    tmp <- tempfile(pattern = "fundsr-dl-", tmpdir = destdir)
    on.exit(unlink(tmp, force = TRUE), add = TRUE)

    last_err <- NULL
    status <- NA_integer_

    for (i in seq_len(attempts)) {
        if (file.exists(tmp)) unlink(tmp, force = TRUE)

        status <- tryCatch(
            {
                if (requireNamespace("curl", quietly = TRUE)) {
                    curl::curl_download(url, destfile = tmp, quiet = quiet, mode = "wb")
                    0L
                } else {
                    utils::download.file(url, destfile = tmp, mode = "wb", quiet = quiet)
                }
            },
            error = function(e) {
                last_err <<- e
                NA_integer_
            }
        )

        ok <- isTRUE(status == 0L) &&
            file.exists(tmp) &&
            isTRUE(file.info(tmp)$size > 0)

        if (ok) {
            if (!isTRUE(file.rename(tmp, destfile))) {
                if (!isTRUE(file.copy(tmp, destfile, overwrite = TRUE))) {
                    if (file.exists(destfile)) unlink(destfile, force = TRUE)
                    fundsr_abort(
                        msg = c(
                            "Download succeeded but couldn't write the destination file.",
                            sprintf("destfile = %s.", sQuote(destfile))
                        ),
                        class = "fundsr_io_error"
                    )
                }
                unlink(tmp, force = TRUE)
            }
            return(invisible(destfile))
        }

        if (i < attempts) {
            Sys.sleep(backoff * 2^(i - 1L))
        }
    }

    details <- if (!is.null(last_err)) {
        conditionMessage(last_err)
    } else if (is.na(status)) {
        "Unknown download failure."
    } else {
        glue("Download returned status {status}.")
    }

    fundsr_abort(
        msg = c(
            glue("Failed to download {sQuote(url)} to {sQuote(destfile)}"),
            glue("    after {attempts} attempt(s)."),
            details
        ),
        class  = "fundsr_download_failed",
        parent = last_err
    )
}
