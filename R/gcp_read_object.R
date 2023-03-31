#### GCP READ OBJECT MODULE ####
# 1 gcp_read_as_raw
# 2 gcp_read_single_csv
# 3 gcp_read_multiple_csv
# 4 gcp_read_csv
# 5 gcp_read_rda
# 6 gcp_read_yaml
#### End of contents ####

#' @title gcp_read_as_raw
#'
#' @description a parse function supplied to gcs_get_object to read in a data object in a .raw format.
#'
#' @param object the object to read in - not needed to specify this when using as a parse function in #'gcs_get_objects
#' @return invisible as this is only used as a parse function in gcs_get_objects
#'
#' @export
gcp_read_as_raw <- function(object) {
  suppressWarnings(httr::content(object, as = "raw"))
}

#' @title gcp_read_single_csv
#'
#' @description function to read in a single csv file from a GCP bucket. This function is used inside the #'gcp_read_csv wrapper.
#'
#' @param file_path - the file path to be read in.
#' @param bucket GCP bucket to be read in from (default is data bucket)
#' @param col_types a list of column names and col_types across the file you are reading in.
#' @return a dataframe
#'
#' @export
gcp_read_single_csv <- function(file_path, bucket, col_types = NULL) {
  # validation checks
  assertive.types::assert_is_character(file_path)

  # this reads in the data if only one filepath is provided - no need for a loop
  data <- as.data.frame(readr::read_csv(
    googleCloudStorageR::gcs_get_object(file_path,
      bucket = bucket,
      parseFunction = gcp_read_as_raw
    ),
    col_types = col_types
  ))
  return(data)
}


#' @title gcp_read_multiple_csvs
#'
#' @description function to read in multiple CSV files. This function is used inside the gcp_read_csv #'wrapper.
#'
#' @param file_paths a list of file_paths to be read in. It is easier to provide this as a named list as the data will be returned with the same names.
#' @param bucket GCP bucket to be read in from (default is data bucket)
#' @param col_types a list of column names and col_types across all files you are reading in
#' @return a dataframe if one file path is provided or a list of dataframes if multiple file paths are provided
#'
#' @export
gcp_read_multiple_csv <- function(file_paths, bucket, col_types = NULL) {
  # validation check - need to use lapply as is.character does not work with lists
  purrr::map(file_paths, ~ {
    assertthat::assert_that(is.character(.x))
  })

  data <- purrr::map(.x = file_paths, .f = function(x) {
    as.data.frame(readr::read_csv(
      googleCloudStorageR::gcs_get_object(
        object_name = x,
        bucket = bucket,
        parseFunction = gcp_read_as_raw
      ),
      col_types = col_types
    ))
  })
  return(data)
}

#' @title gcp_read_csv
#'
#' @description function to read in csv files from the GCP buckets. This function is a wrapper for the gcp_read_single_csv and gcp_read_multiple_csvs functions.
#'
#' @param file_paths one or more file_paths to be read in. If more than one file path is provided, it is easier to provide this as a named list as the data will be returned with the same names.
#' @param bucket GCP bucket to be read in from (default is data bucket)
#' @param col_types a list of column names and col_types across all files you are reading in
#' @return a dataframe if one file path is provided or a list of dataframes if multiple file paths are provided
#'
#' @export

gcp_read_csv <- function(file_paths, bucket = c("wip_bucket", "data_bucket", "review_bucket"),
                         col_types = NULL) {
  authenticate_gcp()

  # validation checks
  bucket <- match.arg(bucket)
  assertthat::assert_that((length(file_paths) > 0) & (!missing(file_paths)),
    msg = "You have not provided any file paths to the file_paths argument"
  )

  # getting the bucket_path from the gcp_paths object
  bucket_path <- gcptools::gcp_paths[[bucket]]

  if (length(file_paths) == 1) {
    data <- gcp_read_single_csv(file_paths, bucket = bucket_path, col_types)
  } else {
    data <- gcp_read_multiple_csv(file_paths, bucket = bucket_path, col_types)
  }
}

#' @title gcp_load_rda
#'
#' @description function to read in rda files, N.B this currently only works if the .Rda file contains only a single object
#'
#' @param file rda object to read in
#'
#' @export

gcp_read_rda <- function(file, bucket = gcptools::gcp_paths$data_bucket) {
    x <- googleCloudStorageR::gcs_get_object(
        object_name = file,
        parseFunction = googleCloudStorageR::gcs_parse_download,
        bucket = bucket
    )
    
    e <- new.env()
    load(rawConnection(x), envir = e)

    return(e[[file]])
    message("gcp_read_rda is now depricated, please use gcp_read_rdata")
}


#' @title gcp_read_yaml
#'
#' @description function to read yaml files in to the environment from a gcpbucket
#'
#' @param filepath a string containing file path (including folder name) to the yaml file which you want to read in
#' @param bucket a string containing the name of the gcp bucket you want to read the yaml file from
#'
#' @return a list containing the contents of the yaml file
#'
#' @export

gcp_read_yaml <- function(filepath, bucket = gcp_paths$review_bucket) {
  tmp <- tempfile(fileext = ".yaml")

  suppressMessages(gcs_get_object(filepath, bucket = bucket, saveToDisk = tmp))

  yaml <- yaml::read_yaml(tmp)

  unlink(tmp)

  return(yaml)
}


#' @title gcp_read_rdata
#'
#' @description function to read rdata files in to the environment from a gcpbucket
#'
#' @param filepath a string containing file path (including folder name) to the yaml file which you want to read in
#' @param bucket a string containing the name of the gcp bucket you want to read the yaml file from
#'
#' @return a data object which you have read in from the bucket
#'
#' @export

gcp_read_rdata <- function(filepath = "inla_graphs", bucket = gcptools::gcp_paths$data_bucket) {
  object <- googleCloudStorageR::gcs_get_object(
    object_name = filepath,
    parseFunction = googleCloudStorageR::gcs_parse_download,
    bucket = bucket,
    saveToDisk = "temp.rdata",
    overwrite = TRUE
  )

  downloaded_file <- readRDS("temp.rdata")

  unlink("temp.rdata", recursive = TRUE, force = TRUE)
  return(downloaded_file)
  message("gcp_read_rda is now depricated, please use gcp_read_rdata")
}