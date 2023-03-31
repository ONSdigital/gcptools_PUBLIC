#### LOG MODEL OUTPUTS MODULE ####
# 1 log_output_files
# 2 log_age_contour_outputs
# 3 log_inla_outputs
# 4 log_mrp_model_outputs
# 5 log_incidence_model_outputs
#### End of contents ####

#' @title log_output_files
#'
#' @description logs whether expected files have been saved to the wip and review buckets
#'
#' @param config config list read in by load_config
#' @param expected_wip_files a named vector or list containing files that are expected to be saved to the wip bucket - leave blank or set as NULL if you don't want to log outputs that are saved to the WIP bucket
#' @param expected_review_files a named vector or list containing files that are expected to be saved to the review bucket - leave blank or set as NULL if you don't want to log outputs that are saved to the review bucket
#' @param folder_path the folder path where the files are expected to be saved
#'
#' @export

log_output_files <- function(config, expected_wip_files = NULL, expected_review_files = NULL, folder_path) {
  # creating a list of buckets dependent on expected_files supplied as arguments
  buckets <- c()
  if (!is.null(expected_wip_files)) {
    buckets <- append(buckets, "polestar-prod-process-wip")
  }
  if (!is.null(expected_review_files)) {
    buckets <- append(buckets, "polestar-prod-review")
  }

  if (length(buckets) == 0) {
    stop("You must provide one or both of expected_review_files or expected_wip_files as arguments")
  }

  # will run for either one or both buckets depending on which expected_files were provided
  for (bucket in buckets) {
    # creating a list of files in the relevant bucket
    bucket_file_list <- googleCloudStorageR::gcs_list_objects(
      bucket = bucket,
      prefix = folder_path
    )

    # renaming bucket for brevity when used in the logs and setting expected files depending on bucket
    if (bucket == "polestar-prod-process-wip") {
      bucket <- "WIP bucket"
      expected_files <- expected_wip_files
    }

    if (bucket == "polestar-prod-review") {
      bucket <- "review bucket"
      expected_files <- expected_review_files
    }

    # this section will only run if length(bucket_file_list) is greater than zero
    if (length(bucket_file_list) > 0) {
      # checking whether folders have been created - whether the folder_path is found within any of the file names in bucket_file_list
      if (any(grepl(folder_path, bucket_file_list))) {
        futile.logger::flog.info(paste0("The folder ", folder_path, " has been created in the ", bucket))
      } else {
        futile.logger::flog.info(paste("The folder ", folder_path, " has NOT been created in the ", bucket))
      }

      # logging number of files uploaded to the relevant folder
      n_expected_files_found_in_bucket <- sum(sapply(expected_files, grepl, bucket_file_list))
      n_expected_files <- length(expected_files)
      prop_expected_files_found_msg <-
        paste0(n_expected_files_found_in_bucket, " files out of ", n_expected_files, " found in ", bucket)

      futile.logger::flog.info(prop_expected_files_found_msg)

      # getting a list of indices where expected files match what is in the buckets
      matching_indices <- sapply(expected_files, grep, bucket_file_list[[1]])

      # looping through files in expected_wip_files and logging whether these have been saved or not
      for (file in names(expected_files)) {
        # using expected file_name
        file_name <- basename(expected_files[[file]])
        # if no matching file names have been found
        if (length(matching_indices[[file]]) == 0) {
          futile.logger::flog.debug(paste0(file_name, " has NOT been saved to the ", bucket))
        } else {
          # if file_name is found, use file_name in bucket rather than in the expected file list
          file_name <- basename(bucket_file_list$name[matching_indices[[file]]])
          file_size <- bucket_file_list$size[matching_indices[[file]]]
          futile.logger::flog.debug(paste0(
            file_name, " has been saved to the ", bucket, ", file size: ", file_size
          ))
        }
      }
      # else if length(bucket_file_list) == 0 (no files found in the bucket for supplied folder_path prefix)
    } else {
      futile.logger::flog.info(paste0(folder_path, " has not been created in the ", bucket, " and no outputs found"))
    }
  }
}

#' @title log_age_contour_outputs
#'
#' @description log whether expected outputs are saved to WIP and review buckets for the age contour pipeline
#'
#' @param config config list read in by load_config
#' @param time_stamp time_stamp generated in main
#'
#' @export

log_age_contour_outputs <- function(config, time_stamp) {
  swab_or_AB <- config$run_settings$swabs_or_antibodies
  end_date <- gsub(
    pattern = "-", replacement = "",
    x = config$run_settings$end_date
  )

  # getting the folder_path using logic from save_outputs function
  folder_path <- paste0(
    config$run_settings$data_run,
    config$paths$output[[swab_or_AB]]
  )

  if (config$run_settings$rerun == TRUE) {
    folder_path <- paste0(folder_path, "_rerun")
  }

  # suffix for filenames
  suffix <- paste0(swab_or_AB, "_", end_date, "_", time_stamp)

  # creating a list of all expected files to check against files in buckets
  expected_wip_files <- list()

  # expected plots - saved to both buckets
  for (country in c(
    "England",
    "Scotland",
    "Wales",
    "NI",
    "regions"
  ))
  {
    plot_suffix <- paste0(swab_or_AB, "_", country, "_", end_date, "_", time_stamp)
    expected_wip_files[[country]] <- paste0(folder_path, "/plot_age_contour_", plot_suffix, ".jpeg")
  }

  # expected config file - saved to both buckets
  expected_wip_files$config <- paste0(folder_path, "/config_age_contour_", suffix, ".yaml")

  # expected probabilities - saved to both buckets
  expected_wip_files$probs <- paste0(folder_path, "/probs_age_contour_", suffix, ".csv")

  # expected input file - saved to both buckets
  expected_wip_files$input <- paste0(folder_path, "/input_age_contour_", suffix, ".csv")

  # expected model file - saved to wip bucket only
  expected_wip_files$model <- paste0(folder_path, "/model_age_contour_", suffix, ".Rds")

  # expected QA report - saved to both buckets
  expected_wip_files$qa_report <- paste0(paste0(folder_path, "/qareport_age_contour_", suffix, ".html"))

  expected_review_files <- expected_wip_files
  # removing model from expected_review_files
  expected_review_files$model <- NULL

  # flattening lists so they can be looped through
  expected_wip_files <- unlist(expected_wip_files)
  expected_review_files <- unlist(expected_review_files)

  log_output_files(
    config = config,
    expected_wip_files = expected_wip_files,
    expected_review_files = expected_review_files,
    folder_path = folder_path
  )
}

#' @title log_inla_outputs
#'
#' @description logs whether expected outputs are saved to WIP and review buckets for the INLA pipeline
#'
#' @param config config as read in by load_config
#'
#' @export
log_inla_outputs <- function(config) {
  # creating an empty list to store expected files
  expected_review_files <- list()
  expected_wip_files <- list()

  # save_outputs logic
  timestamp <- format(Sys.time(), "DTS%y%m%d")
  data_run_date <- gsub("-", "", config$run_settings$data_run)
  folder_name <- paste0(data_run_date, "_inla/")
  suffix_to_file_name <- paste0("_inla_", data_run_date, "_", timestamp)

  if (!(is.null(config$run_settings$test))) {
    if (config$run_settings$test == TRUE) {
      folder_name <- gsub(
        pattern = " ",
        replacement = "_",
        x = paste0(
          "TEST_",
          config$run_settings$analyst,
          "_", folder_name
        )
      )
    }
  }

  # unified_outputs
  expected_wip_files$unified_output <- paste0(folder_name, "antibodies_vaccines", suffix_to_file_name)

  # qa_report
  expected_wip_files$qa_report <- paste0(folder_name, "INLA_QA_Datarun", config$run_settings$data_run)

  # config
  expected_wip_files$config <- paste0(folder_name, "inla_config", suffix_to_file_name)

  expected_review_files <- expected_wip_files

  log_output_files(
    config = config,
    expected_wip_files = expected_wip_files,
    expected_review_files = expected_review_files,
    folder_path = folder_name
  )
}

#' @title log_mrp_outputs
#'
#' @description logs whether expected outputs are saved to WIP and review buckets for the MRP pipeline
#'
#' @param region_diffs region_diffs generated in main.R
#' @param config main config read in by load_config
#' @param time_stamp timestamp of when outputs were saved
#'
#' @export
log_mrp_outputs <- function(region_diffs, config, time_stamp) {
  # creating empty list for expected_files
  expected_wip_files <- list()
  expected_review_files <- list()

  # using logic from the save_outputs function
  data_run_date <- gsub("-", "", config$run_settings$data_run)
  end_date <- gsub("-", "", config$run_settings$end_date)

  folder_name <- paste0(data_run_date, "_mrp/")
  if (config$run_settings$test == TRUE) {
    folder_name <- gsub(
      pattern = " ",
      replacement = "_",
      x = paste0("TEST_", config$run_settings$analyst, "_", folder_name)
    )
  }

  if (config$run_settings$rerun == TRUE) {
    folder_name <- gsub(
      pattern = " ",
      replacement = "_",
      x = paste0(data_run_date, "_mrp_rerun/")
    )
  }

  suffix_to_file_name <- paste0("_mrp_", end_date, "_", time_stamp)

  # saving files to expected files
  # probs_over_time
  expected_wip_files$probs_over_time <- paste0(folder_name, "probs_over_time", suffix_to_file_name, ".csv")

  # region_diffs
  if (!is.null(region_diffs)) {
    expected_wip_files$region_diffs <- paste0(folder_name, "probs_over_time_by_region", suffix_to_file_name, ".csv")
  }

  # prev_time_series
  expected_wip_files$prev_time_series <- paste0(folder_name, "prev_time_series", suffix_to_file_name, ".csv")

  # pos_by_day
  expected_wip_files$pos_by_day <- paste0(folder_name, "positive_swabs_by_day_", suffix_to_file_name, ".csv")

  # pos_ct
  expected_wip_files$pos_ct <- paste0(folder_name, "positive_swabs_by_ct_", suffix_to_file_name, ".csv")

  # configs
  expected_wip_files$configs <- paste0(folder_name, "configs", suffix_to_file_name, ".yaml")

  # qa reports
  for (country in c("England", "Northern_Ireland", "Scotland", "Wales")) {
    expected_wip_files[[country]] <- paste0("MRP_QA_", country, "_Datarun", data_run_date)
  }

  # only review bucket if config$run_settings$test != TRUE
  if (config$run_settings$test == TRUE) {
    expected_review_files <- NULL
  } else {
    expected_review_files <- expected_wip_files
  }

  # model filepath - wip bucket only
  expected_wip_files$model <- paste0(folder_name, "models", suffix_to_file_name, ".RData")

  log_output_files(
    config = config,
    expected_wip_files = expected_wip_files,
    expected_review_files = expected_review_files,
    folder_path = folder_name
  )
}

#' @title log_incidence_outputs
#'
#' @description logs whether expected outputs are saved to WIP and review buckets for the Incidence pipeline
#'
#' @param config main config read in by load_config
#'
#' @export
log_incidence_outputs <- function(config) {
  expected_wip_files <- list()
  expected_review_files <- list()

  # using logic from save_outputs
  end_date <- gsub(pattern = "-", replacement = "", x = as.character(config$run_settings$end_date))
  data_run_date <- gsub("-", "", config$run_settings$data_run)
  timestamp <- format(Sys.time(), "DTS%y%m%d")
  folder_name <- paste0(data_run_date, "_incidence/")
  suffix_to_file_name <- paste0("_incidence_", end_date, "_", timestamp)

  # all files are currently hardcoded to go into the review file except for QA files
  # artifacts list
  expected_review_files$model <- paste0(folder_name, "artifacts_list", suffix_to_file_name)

  # config
  expected_review_files$config <- paste0(folder_name, "configs", suffix_to_file_name)

  # comparative_probs_tbl
  expected_review_files$comparative_probs_tbl <- paste0(folder_name, "comparative_probs_tbl", suffix_to_file_name)

  # deconv_final_sum_tbl
  expected_review_files$deconv_final_sum_tbl <- paste0(folder_name, "deconv_final_sum_tbl", suffix_to_file_name)

  # summaries_tbl
  expected_review_files$summaries_tbl <- paste0(folder_name, "summaries_tbl", suffix_to_file_name)

  # runtime_tbl
  expected_review_files$runtime_tbl <- paste0(folder_name, "runtime_tbl", suffix_to_file_name)

  # percentage_clearance_10_tbl
  expected_review_files$percentage_clearance_10_tbl <- paste0(folder_name, "percentage_clearance_10_tbl", suffix_to_file_name)

  # QA files - saved into both buckets as default as no test in config
  expected_review_files <- paste0(folder_name, "Incidence_QA_Datarun", data_run_date)
  expected_wip_files <- paste0(folder_name, "Incidence_QA_Datarun", data_run_date)

  log_output_files(
    config = config,
    expected_wip_files = expected_wip_files,
    expected_review_files = expected_review_files,
    folder_path = folder_name
  )
}
