#### GCP WRITE OBJECT MODULE ####
# 1 gcp_write_rds
# 2 gcp_write_yaml
# 3 gcp_write_xlsx
# 4 gcp_write_csv
# 5 gcp_write_html
# 6 gcp_write_jpeg
# 7 gcp_write_log
#### End of contents ####

#' gcp_write_rds
#'
#' @param input NULL used internally by gcs_upload
#' @param output NULL used internally by gcs_upload
#' @return None (object_function)
#' @noRd
gcp_write_rds <- function(input, output) {
  saveRDS(input, file = output)
}

#' gcp_write_yaml
#'
#' @param input NULL used internally by gcs_upload
#' @param output NULL used internally by gcs_upload
#' @return None (object_function)
#' @noRd
gcp_write_yaml <- function(input, output) {
  yaml::write_yaml(input, file = output)
}

#' gcp_write_xlsx
#'
#' @param input NULL used internally by gcs_upload
#' @param output NULL used internally by gcs_upload
#' @return None (object_function)
#' @noRd
gcp_write_xlsx <- function(input, output) {
  xlsx::write.xlsx(input, row.names = FALSE, file = output)
}

#' gcp_write_csv
#'
#' @param input NULL used internally by gcs_upload
#' @param output NULL used internally by gcs_upload
#' @return None (object_function)
#' @noRd
gcp_write_csv <- function(input, output) {
  write.table(input, file = output, row.names = FALSE, sep = ",")
}

#' gcp_write_html
#'
#' @param input NULL used internally by gcs_upload
#' @param output NULL used internally by gcs_upload
#' @return None (object_function)
#' @noRd
gcp_write_html <- function(input, output) {
  htmltools::save_html(html = input, file = output)
}

#' gcp_write_jpeg
#'
#' @param input NULL used internally by gcs_upload
#' @param output NULL used internally by gcs_upload
#' @return None (object_function)
#' @noRd
gcp_write_jpeg <- function(input, output) {
  ggplot2::ggsave(filename = output, plot = input, width = 10, height = 10)
}

#' @title gcp_write_log
#'
#' @description function to read in log files
#'
#' @param input object to be read in log format
#' @param output log file
#'
#' @export

gcp_write_log <- function(input,
                          output) {
  return(writeLines(input, con = output))
}
