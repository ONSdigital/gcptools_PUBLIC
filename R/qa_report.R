#### QA REPORT MODULE ####
# 1 download_qa_report_to_notebook
# 2 save_qa_report
# 3 save_qa_report_age_contour
#### End of contents ####

#' @title download_qa_report_to_notebook
#'
#' @description function to read in qa reports from bucket
#'
#' @param file full path to file
#' @param bucket the bucket containing the qa report (defaults to WIP)
#'
#' @export

# MRP version
# download_qa_report_to_notebook <- function(file,
#                                            bucket = "polestar-prod-process-wip") {
#   options("googleAuthR.httr_oauth_cache" = "gce.oauth")
#   googleAuthR::gar_gce_auth()

#   html <- googleCloudStorageR::gcs_get_object(
#     object_name = file,
#     bucket = bucket,
#     parseFunction = xml2::read_html
#   )

#   html_qa <- htmltools::renderDocument(htmltools::htmlTemplate(text_ = html))


#   htmltools::save_html(html_qa,
#     file = basename(file)
#   )

#   return(invisible())
# }


# Age_contour version
download_qa_report_to_notebook <- function(file,
                                           bucket = "polestar-prod-process-wip") {
  options("googleAuthR.httr_oauth_cache" = "gce.oauth")
  googleAuthR::gar_gce_auth()

  fetch_html_file <- function(file) {
    html <- xml2::read_html(file)
    html_body <- xml2::xml_find_all(html, xpath = "//body//text()")
    parsed_html <- xml2::xml_text(html_body)

    return(parsed_html)
  }

  html <- googleCloudStorageR::gcs_get_object(
    object_name = file,
    bucket = bucket,
    parseFunction = fetch_html_file
  )

  html_qa <- htmltools::renderDocument(htmltools::htmlTemplate(text_ = html))

  file_name <- paste("/home/jupyter/QA_reports/", basename(file))

  htmltools::save_html(html_qa, file = file_name)

  return(invisible())
}

# MRP version
#' @title save QA report
#'
#' @description save QA report to the selected bucket
#'
#' @param html_file_to_read path to qa report, includes object name
#' @param filename which the qa report will be saved in the bucket
#' @param bucket either review_bucket or wip_bucket
#' @param config config list containing the output path
#'
#' @export

save_qa_report <- function(html_file_to_read,
                           bucket = "polestar-prod-process-wip",
                           config,
                           filename) {
  # change end date to string before saving because this is changed in the load_config function
  config$run_settings$end_date <- gsub(
    pattern = "-",
    replacement = "",
    x = as.character(config$run_settings$end_date)
  )


  # GCP convention on filenames
  # Folder name = [data_run]_[model]_<sub_model>

  data_run_date <- gsub(
    pattern = "-",
    replacement = "",
    x = config$run_settings$data_run
  )

  folder_name <- paste0(data_run_date, "_mrp/")

  if (config$run_settings$test == TRUE) {
    folder_name <- gsub(
      pattern = " ",
      replacement = "_",
      x = paste0(
        "TEST_",
        config$run_settings$analyst,
        "_",
        folder_name
      )
    )
  }

  # suffix_to_file_name <- paste0("_mrp_", end_date, "_", timestamp)

  options("googleAuthR.httr_oauth_cache" = "gce.oauth")
  googleAuthR::gar_gce_auth()

  # html <- textreadr::read_html(html_file_to_read)
  html <- paste0(readLines(html_file_to_read), collapse = "\n")

  # add option to not include filename
  if (missing(filename)) {
    filename <- tools::file_path_sans_ext(basename(html_file_to_read))
  }

  file_name <- paste0(
    folder_name,
    filename, # suffix_to_file_name,
    ".html"
  )
  googleCloudStorageR::gcs_upload(html,
    name = file_name,
    bucket = bucket,
    object_function = save_html,
    predefinedAcl = "bucketLevel"
  )

  return(paste0(html_file_to_read, " uploaded: saved in ", bucket, " as ", file_name))
}

# AgeContour Version
#' @title save QA report
#'
#' @description save QA report to the review and wip buckets
#'
#' @param html_file_to_read path to qa report, includes object name
#' @param config config list containing the output path
#' @param time_stamp the time_stamp of this model run
#'
#' @export

save_qa_report_age_contour <- function(html_file_to_read,
                                       config,
                                       time_stamp) {
  wip_bucket <- config$GCP$wip_bucket
  review_bucket <- config$GCP$review_bucket
  options("googleAuthR.httr_oauth_cache" = "gce.oauth")
  googleAuthR::gar_gce_auth()

  swab_or_AB <- config$run_settings$swabs_or_antibodies
  end_date <- gsub(
    pattern = "-", replacement = "",
    x = config$run_settings$end_date
  )

  # See folder and file name conventions for details
  folder_path <- paste0(
    config$run_settings$data_run,
    config$paths$output[[swab_or_AB]]
  )
  suffix <- paste0(swab_or_AB, "_", end_date, "_", time_stamp)

  # Read in the QA html from users area
  html <- paste(readLines(html_file_to_read), collapse = "\n")

  # Save files to both WIP and review buckets
  destination_buckets <- list(
    review = review_bucket,
    wip = wip_bucket
  )

  for (bucket in destination_buckets) {
    file_name <- paste0(
      folder_path, "/qareport_age_contour_",
      suffix, ".html"
    )
    save_html <- function(input, output) htmltools::save_html(html = input, file = output)
    googleCloudStorageR::gcs_upload(html,
      name = file_name,
      bucket = bucket,
      type = "html",
      object_function = save_html,
      predefinedAcl = "bucketLevel"
    )
  }

  #    return(paste(html_file_to_read, "uploaded: saved in", bucket,
  #                     "as", file_name))
}
