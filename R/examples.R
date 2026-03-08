#' Get the path to an example file shipped with the package.
#'
#' @param file The name of the example file.
#' @family example helpers
#' @export
#' @examples
#' fundsr_example_data("FNDA.xlsx")
#' fundsr_example_data()

fundsr_example_data <- function(file = ".") {
    system.file("extdata", file, package = "fundsr", mustWork = TRUE)
}
