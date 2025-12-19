.onLoad <- function(libname, pkgname) {
    .fundsr_storage <<- new.env(parent = emptyenv())
    .fundsr <<- new.env(parent = emptyenv())
    .fundsr$data_loaders <- list()
    .fundsr$fund_index_map <- character()
    .fundsr$inkscape_queue <- character()
    .fundsr$done_xlm_sets <- character()
}

.onAttach <- function(libname, pkgname) {
    packageStartupMessage("fundsr loaded.")
}
