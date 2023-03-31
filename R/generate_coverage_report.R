##### GENERATE COVERAGE REPORT MODULE ####
# 1 generate_coverage_report
#### End of contents ####

#' @title Generate coverage report
#'
#' @description This function takes a file path to a package directory (e.g. cismrp). It creates a folder called coverage_report_<dir>_<yyyymmdd>.
#'              It runs covr::package_coverage and then generates a file report for each module in the package. It returns a list containing the
#'              test coverage for each module and for the package overall that can be added to an integrated test report
#'
#' @param dir A string to be included in the output file name along with the date (usually the package name)
#'
#' @returns a list of two elements filecoverage (each module) and totalcoverage (whole_package)
#'
#' @export
generate_coverage_report <- function(dir) {
  output_dir <- paste0("coverage_report_", dir, "_", gsub("-", "", Sys.Date()))

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  package_coverage <- tryCatch(covr::package_coverage(dir),
    error = function(e) {
      stop("package_coverage returned error are all your tests passing?")
    }
  )

  modules <- paste0("R/", list.files(paste0(dir, "/R/"), pattern = ".R"))
  out_names <- gsub(pattern = "R/|\\.R", replacement = "", x = modules)
  sapply(1:length(modules), FUN = function(X) {
    covr::file_report(package_coverage,
      file = modules[[X]],
      out_file = paste0(output_dir, "/", out_names[[X]], ".html")
    )
  })
  output_list <- covr::coverage_to_list(package_coverage)

  return(output_list)
}
