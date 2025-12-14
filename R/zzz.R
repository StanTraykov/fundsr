.onLoad <- function(libname, pkgname) {
    .fundsr_storage <<- new.env(parent = emptyenv())
    .fundsr <<- new.env(parent = emptyenv())
    .fundsr$import_funs <- list()
    .fundsr$fund_index <- character()
    .fundsr$ink_queue <- character()
    .fundsr$done_xlms <- character()
}

.onAttach <- function(libname, pkgname) {
    packageStartupMessage("fundsr loaded.")
}
