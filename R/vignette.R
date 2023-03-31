##### VIGNETTE MODULE ####
# 1 list_vignettes
# 2 display_vignettes
#### End of contents ####

#' @title list_vignettes
#'
#' @description This function will list vignettes available for a package in your notebook/console.
#'
#' @param package The package you wish to check the vignettes for (string).
#'
#' @return A character vector of available vignettes
#'
#' @export
#'
#' @examples
#'
#' # Load the library to ensure example data sets are available
#' library(gcptools)
#'
#' # calling function
#' list_vignettes("readr")
list_vignettes <- function(package) {
  assertive.types::assert_is_character(package)
  vignettes <- utils::vignette(package = package)$results[, "Item"]
  return(vignettes)
}

#' @title display_vignette
#'
#' @description This function will display a chosen vignette of any local ("cis") package in your notebook/console. N.b. It will not work in terminal.
#'
#' @param vignette The name of the vignette you wish to display, do not include .html at the end (string).
#' @param package The package containing the vignette you wish to display (string).
#'
#' @return invisible
#'
#' @export
display_vignette <- function(vignette, package) {
  assertive.types::assert_is_character(vignette)
  assertive.types::assert_is_character(package)

  path <- paste0(package, "/vignettes/", vignette, ".html")

  assertthat::assert_that(dir.exists(package), msg = "package not found, check working directory")
  assertthat::assert_that(file.exists(path), msg = "vignette not found, check spelling")

  IRdisplay::display_html(htmltools::renderDocument(htmltools::htmlTemplate(path)))
  return(invisible())
}
