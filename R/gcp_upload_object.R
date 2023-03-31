#### GCP UPLOAD OBJECT MODULE ####
# 1 gcp_upload_csv
# 2 gcp_upload_rds
# 3 gcp_upload_rdata
# 4 gcp_upload_raw_log
# 5 gcp_upload_object
# 6 get_object_write_function
# 7 gcp_upload_processed_log
#### End of contents ####

#' gcp_upload_csv
#'
#' @param data data to upload
#' @param filename desired output filename including folder
#' @param bucket desired output bucket
#'
#' @return None (file uploaded to bucket)
#' @export
gcp_upload_csv <- function(data, filename, bucket) {
  googleCloudStorageR::gcs_upload(
    data,
    name = filename,
    bucket = bucket,
    object_function = gcp_write_csv,
    predefinedAcl = "bucketLevel"
  )
  message("gcp_upload_csv is now depricated, consider using gcp_upload_object")
}

#' gcp_upload_rds
#'
#' @param data data to upload
#' @param filename desired output filename including folder
#' @param bucket desired output bucket
#'
#' @return None (file uploaded to bucket)
#' @export
gcp_upload_rds <- function(data, filename, bucket) {
  if (grepl(x = filename, pattern = ".Rds") != TRUE) {
    stop("please add .Rds suffix to your filename")
  }

  googleCloudStorageR::gcs_upload(data,
    name = filename,
    bucket = bucket,
    type = "Rds",
    object_function = gcp_write_rds,
    predefinedAcl = "bucketLevel"
  )
  message("gcp_upload_rds is now depricated, consider using gcp_upload_object")
}
#' gcp_upload_rdata
#'
#' @param data data to upload
#' @param filename desired output filename including folder
#' @param bucket desired output bucket
#'
#' @return None (file uploaded to bucket)
#' @export
gcp_upload_rdata <- function(data, filename, bucket) {
  if (grepl(x = filename, pattern = ".RData") != TRUE) {
    stop("please add .RData suffix to your filename")
  }

  googleCloudStorageR::gcs_upload(data,
    name = filename,
    bucket = bucket,
    type = "RData",
    object_function = gcp_write_rds,
    predefinedAcl = "bucketLevel"
  )
  message("gcp_upload_rdata is now depricated, consider using gcp_upload_object")
}

#' gcp_upload_raw_log
#'
#' @param file_path path to the raw data
#' @param destination_folder folder name in bucket
#' @param pipeline desired output pipeline name
#' @param bucket desired output bucket
#' @param config configuration settings read in by config.yaml
#'
#' @return None (file uploaded to bucket)
#' @export

gcp_upload_raw_log <- function(file_path,
                               destination_folder = "logs/raw/",
                               pipeline = c("mrp", "incidence", "inla", "age_contour"),
                               bucket = "polestar-prod-process-wip",
                               config) {
  authenticate_gcp()

  pipeline <- match.arg(pipeline)

  # unique identifier
  timestamp <- format(Sys.time(), "DTS%y%m%d_%H%M%Z")

  suppressWarnings(data <- readLines(file_path))

  googleCloudStorageR::gcs_upload(
    data,
    name = paste0(destination_folder, pipeline, "_", config$run_settings$data_run, "_", timestamp, ".log"),
    bucket = bucket,
    object_function = gcp_write_log,
    predefinedAcl = "bucketLevel"
  )
}

#' gcp_upload_object
#'
#' @param object a named object within the environment which you want to upload
#' @param filepath the desired filename and folder name combined as one string
#' @param file_type a string containing the file extension without the '.' at the start e.g. 'csv'
#' @param bucket desired output bucket
#'
#' @return None (file uploaded to bucket)
#' @export
gcp_upload_object <- function(object, filepath, file_type, bucket) {
  object_function <- get_object_write_function(file_type)

  googleCloudStorageR::gcs_upload(
    file = object,
    bucket = bucket,
    type = file_type,
    name = filepath,
    object_function = object_function,
    predefinedAcl = "bucketLevel"
  )
}

#' @title get_object_write_function
#'
#' @description function to select an appropriate write function to parse by filetype
#'
#' @param file_type a string containing the file type extension e.g. 'csv'
#'
#' @return a write function
#'

get_object_write_function <- function(file_type = c("rdata", "yaml", "jpeg", "html", "csv", "xlsx", "rds")) {
  file_type <- match.arg(file_type)

  function_list <- list(
    "rdata" = gcp_write_rds,
    "rds" = gcp_write_rds,
    "csv" = gcp_write_csv,
    "xlsx" = gcp_write_xlsx,
    "yaml" = gcp_write_yaml,
    "html" = gcp_write_html,
    "jpeg" = gcp_write_jpeg
  )

  return_function <- function_list[[file_type]]

  return(return_function)
}

#' gcp_upload_processed_log
#'
#' @param file_path path to the raw data
#' @param destination_folder folder name in bucket
#' @param pipeline desired output pipeline name
#' @param bucket desired output bucket
#' @param config configuration settings read in by config.yaml
#'
#' @return None (file uploaded to bucket)
#' @export

gcp_upload_processed_log <- function(file_path,
                                     destination_folder = "logs/processed/",
                                     pipeline = c("mrp", "incidence", "inla", "age_contour"),
                                     bucket = "polestar-prod-process-wip",
                                     config) {
  authenticate_gcp()

  pipeline <- match.arg(pipeline)

  # unique identifier
  timestamp <- format(Sys.time(), "DTS%y%m%d_%H%M%Z")

  data <- convert_log_to_df(file_path)

  googleCloudStorageR::gcs_upload(
    data,
    name = paste0(destination_folder, pipeline, "_", config$run_settings$data_run, "_processed_log_", timestamp),
    bucket = bucket,
    predefinedAcl = "bucketLevel"
  )
}
