#### GCP UTILS MODULE ####
# 1 gcp_paths
# 2 authenticate_gcp
# 3 gsutil_wrapper
#### End of contents ####

#' gcp_paths
#'
#' A list containing paths for three buckets on polestar, data, wip and review
#'    wip_bucket = "ons-psplus-data-prod-psplus-cis-data",
#'    data_bucket = "ons-psplus-data-prod-psplus-cis-data",
#'    review_bucket = "ons-psplus-analysis-prod-cis-review"
#'
#' @source NA
"gcp_paths"

#' @title authenticate gcp
#'
#' @description set up credentials for storage access
#'
#' @export

authenticate_gcp <- function() {
  options("googleAuthR.httr_oauth_cache" = "gce.oauth")
  googleAuthR::gar_gce_auth()
}

#' @title Move files from notebook to bucket
#'
#' @description This function is a wrapper around a system call to gsutil mv/cp which moves / copies a file from your notebook to either the WIP or the review bucket.
#'
#' @param input The name of the object to upload to the bucket.
#'
#' @param output The desired name to save the object to in the bucket (accepts '/'). It currently only works with ".csv"s.
#'
#' @param bucket The bucket to save to one of 'wip_bucket' or 'review_bucket'
#'
#' @param keep.local Boolean (defaults FALSE) controls whether th interim file where the object is saved to the notebook disk is retained.
#'
#' @return invisible
#'
#' @examples
#'
#' # create a test data frame
#' test_data <- data.frame(sample_column <- 1:10)
#'
#' # Call the function with correct parameters
#' gsutil_wrapper(
#'   input = test_data,
#'   output = "test_data.csv",
#'   bucket = "wip_bucket",
#'   keep.local = FALSE
#' )
#'
#' @export

gsutil_wrapper <- function(input,
                           output,
                           bucket = c("wip_bucket", "review_bucket"),
                           keep.local = FALSE) {
  bucket <- match.arg(bucket)

  file_dir <- dirname(output)

  if (dir.exists(file_dir) == FALSE) {
    dir.create(path = file_dir, recursive = TRUE)
  }

  output_file_ext <- tools::file_ext(output)

  if (output_file_ext == "csv") {
    readr::write_csv(input, file = output)
  }

  if (keep.local == FALSE) {
    system(paste("gsutil mv", output, paste0("gs://", gcp_paths[[bucket]], "/", output)))

    if (length(dir(file_dir)) >= 1) {
      warning("Folder contains folder/files. It will not be deleted.")
    } else {
      unlink(file_dir, recursive = T)
    }
  } else {
    system(paste("gsutil cp", output, paste0("gs://", gcp_paths[[bucket]], "/", output)))
  }

  return(invisible)
}
