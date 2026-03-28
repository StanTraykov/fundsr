.onLoad <- function(libname, pkgname) {
    .fundsr_default_session <<- fundsr_session()
}

.onAttach <- function(libname, pkgname) {
    packageStartupMessage("fundsr loaded.")
}
