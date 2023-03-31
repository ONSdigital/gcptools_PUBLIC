#### PACKAGE UTILS MODULE ####
# 1 save_html
# 2 clean_file
# 3 pipe
#### End of contents ####

#' @title save_html
#'
#' @description function to read in html files
#'
#' @param input object to be read in html format
#' @param output html file
#'
#' @export

save_html <- function(input,
                      output) {
  htmltools::save_html(html = input, file = output)
}

#' @title clean_file
#'
#' @description function to remove extension from file path and remove all of path up to last path separator
#'
#' @param path path to clean
#'
#' @return character
#'
#' @export
clean_file <- function(path) {
  return(tools::file_path_sans_ext(basename(path)))
}

#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL
