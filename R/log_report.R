#### LOG REPORT MODULE ####
# 1 write_log_report_to_bucket
# 2 save_log_report
#### End of contents ####

#' write_log_report_to_bucket
#'
#' @description reads in processed logs combines them into a single csv file and
#' saves that single file back to the bucket
#'
#' @param pipeline string set specific pipeline or all pipelines
#' @param top_level_path string path to search for log files
#' @param data_run_from string setting specific date to run from
#' @param data_run_to setting string specific date to run to
#' @param destination_folder folder to write report
#' @param bucket desired output bucket
#'
#' @return None (file uploaded to bucket)
#' @export

write_log_report_to_bucket <- function(pipeline = c("mrp", "incidence", "inla", "age_contour", "all"),
                                       top_level_path = "logs/processed/",
                                       data_run_from = NULL,
                                       data_run_to = NULL,
                                       destination_folder = "logs/report/",
                                       bucket = "polestar-prod-process-wip") {
  authenticate_gcp()

  pipeline <- match.arg(pipeline)

  # retrieve list of files in the bucket
  file_list <- googleCloudStorageR::gcs_list_objects(
    bucket = bucket,
    prefix = top_level_path
  )

  # remove prefix
  file_list$name <- sub(".*/", "", file_list$name)

  file_list <- file_list$name

  # filter by dates
  files_to_download_by_date <- filter_log_by_date(
    file_list = file_list,
    data_run_to = data_run_to,
    data_run_from = data_run_from
  )

  # convert list to data.frame
  files_to_download_by_date <- as.data.frame(unlist(files_to_download_by_date))
  colnames(files_to_download_by_date)[1] <- "filename"

  # filter by pipileine
  files_to_download_by_pipeline <- filter_log_by_pipeline(
    file_list = file_list,
    pipeline = pipeline
  )

  # convert list to data.frame
  files_to_download_by_pipeline <- as.data.frame(unlist(files_to_download_by_pipeline))
  colnames(files_to_download_by_pipeline)[1] <- "filename"

  # merge by inner join
  files_to_download <- files_to_download_by_pipeline %>%
    dplyr::inner_join(files_to_download_by_date, by = "filename")

  # download logs from GCP
  log_list <- get_log_dataframe_list(
    files_to_download,
    top_level_path,
    bucket
  )

  # merge logs into a single data.frame
  stitch_log <- dplyr::bind_rows(log_list, .id = "filename")

  # extract min data_run and max data run date or filter by data dates
  stitch_log$time <-
    format(
      as.POSIXct(stitch_log$timestamp, format = "%Y:%m:%d %H:%M:%S"),
      "%H:%M:%S"
    )

  stitch_log$date <-
    format(
      as.POSIXct(stitch_log$timestamp, format = "%Y:%m:%d %H:%M:%S"),
      "%Y%m%d"
    )

  stitch_log$timestamp <- NULL
  stitch_log$`...1` <- NULL

  save_log_report(
    report = stitch_log,
    destination_folder = destination_folder,
    pipeline = pipeline,
    data_run_to = data_run_to,
    data_run_from = data_run_from,
    bucket = bucket
  )
}

#' save_log_report
#'
#' @description saves report to the bucket
#'
#' @param report csv file containing stitch logs
#' @param destination_folder folder to write report
#' @param pipeline string set specific pipeline or all pipelines
#' @param data_run_from This is the earliest date (inclusive) you want logs for (set as NULL if you don't want to restrict logs to after a selected date).
#' @param data_run_to This is the latest date (inclusive) you want logs for (set as NULL if you don't want to restrict logs to before a selected date).
#' @param bucket desired output bucket
#'
#' @return None (file uploaded to bucket)
#' @export

save_log_report <- function(report,
                            destination_folder,
                            pipeline,
                            data_run_to,
                            data_run_from,
                            bucket) {
  authenticate_gcp()

  # save csv file to bucket
  googleCloudStorageR::gcs_upload(
    report,
    name = paste0(
      destination_folder,
      pipeline,
      "_to_",
      data_run_to,
      "_from_",
      data_run_from,
      "_report"
    ),
    bucket = bucket,
    predefinedAcl = "bucketLevel"
  )
}
