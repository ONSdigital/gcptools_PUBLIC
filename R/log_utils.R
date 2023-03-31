#### LOG UTILS MODULE ####
# 1 initialise_log
# 2 count_warnings_and_errors
# 3 filter_log_by_date
# 4 filter_log_by_pipeline
# 5 filter_log_level
# 6 convert_log_to_df
#### End of contents ####

#' @title initialise_log
#' @description Opens up a new log file
#' @param log_dir the directory to store log files (if missing defaults to current working directory)
#' @param log_file the output file name
#' @param log_threshold one of "TRACE", "DEBUG", "INFO", "WARN", "ERROR", "FATAL"
#'
#' @return log_file_name the file path for the log file that is created
#' @export

initialise_log <- function(log_dir,
                           log_file,
                           log_threshold = c("TRACE", "DEBUG", "INFO", "WARN", "ERROR", "FATAL")) {
  log_threshold <- match.arg(log_threshold)

  if (missing(log_dir)) {
    log_dir <- paste0(getwd(), "/")
  } else if (length(log_dir) != 1) {
    stop("Only one directory for logs can be supplied to `initialise_log`")
  } else if (grepl(x = log_dir, pattern = "/$") == FALSE) {
    log_dir <- paste0(log_dir, "/")
  }

  if (dir.exists(log_dir) == FALSE & log_dir != "") {
    dir.create(log_dir)
  }

  timestamp <- gsub(pattern = "[-: ]", replacement = "_", format(Sys.time(), "DTS%Y %m %d %X %Z"))

  log_file_name <- paste0(log_dir, log_file, "_", timestamp, ".log")

  futile.logger::flog.appender(
    futile.logger::appender.tee(log_file_name)
  ) # to log into a file instead of console

  futile.logger::flog.threshold(log_threshold) # TRACE, DEBUG, INFO, WARN, ERROR, FATAL
  futile.logger::flog.info(paste0("Setting up new log file: ", log_file_name))
  futile.logger::flog.info("Tracking Started")

  r_version <- R.Version()
  futile.logger::flog.info(paste0("Current R version: ", r_version$version.string))

  futile.logger::flog.info(paste0("Current branch: ", gcptools::get_current_branch()))

  return(log_file_name)
}

#' @title count_warnings_and_errors
#' @description Counts number of warnings and errors in a log file and appends it to the bottom of the log file
#' @param log_file_name the name of the log file
#'
#' @return invisible
#' @export

count_warnings_and_errors <- function(log_file_name) {
  log <- readLines(log_file_name)
  warnings <- length(grep(pattern = "^WARN\\b", x = log, perl = TRUE))
  errors <- length(grep(pattern = "^ERROR\\b", x = log, perl = TRUE))

  futile.logger::flog.info(paste0("The number of warning messages is: ", warnings))
  futile.logger::flog.info(paste0("The number of error messages is: ", errors))
  return(invisible())
}

#' filter_log_by_date
#'
#' @description filter log files by specific dates
#'
#' @param file_list data.frame all log files located in the bucket
#' @param data_run_from This is the earliest date (inclusive) you want logs for (set as NULL if you don't want to restrict logs to after a selected date.
#' @param data_run_to This is the latest date (inclusive) you want logs for (set as NULL if you don't want to restrict logs to before a selected date.
#'
#' @return files_to_download list of files that meet date criteria
#' @export

filter_log_by_date <- function(file_list,
                               data_run_to,
                               data_run_from) {
  files_to_download <- list()

  if (is.null(data_run_from) & is.null(data_run_to)) {
    files_to_download <- as.list(file_list)
    message("All logs will be included")

    return(files_to_download)
  } else if (is.null(data_run_to)) {
    assertthat::assert_that(nchar(data_run_from) == 8,
      msg = "data_run_from input is incorrect, it should only be 8 characters long"
    )

    # keep only files with selected dates
    for (filename_index in 1:length(file_list)) {
      individual_log_filename <- file_list[filename_index]

      file_data_run <- as.numeric(sapply(strsplit(individual_log_filename, "_"), "[[", 2))

      if (file_data_run >= data_run_from) {
        files_to_download[filename_index] <- individual_log_filename
      }
    }

    return(files_to_download)
  } else if (is.null(data_run_from)) {
    assertthat::assert_that(nchar(data_run_to) == 8,
      msg = "data_run_to input is incorrect, it should only be 8 characters long"
    )

    # keep only files with selected dates
    for (filename_index in 1:length(file_list)) {
      individual_log_filename <- file_list[filename_index]

      file_data_run <- as.numeric(sapply(strsplit(individual_log_filename, "_"), "[[", 2))

      if (file_data_run <= data_run_to) {
        files_to_download[filename_index] <- individual_log_filename
      }
    }

    return(files_to_download)
  } else {
    assertthat::assert_that(nchar(data_run_from) == 8,
      msg = "data_run_from input is incorrect, it should only be 8 characters long"
    )

    assertthat::assert_that(nchar(data_run_to) == 8,
      msg = "data_run_to input is incorrect, it should only be 8 characters long"
    )

    assertthat::assert_that(data_run_from <= data_run_to,
      msg = "data_run_from must be before data_run_to"
    )

    # keep only files with selected dates
    for (filename_index in 1:length(file_list)) {
      individual_log_filename <- file_list[filename_index]

      file_data_run <- as.numeric(sapply(strsplit(individual_log_filename, "_"), "[[", 2))

      if (file_data_run >= data_run_from & file_data_run <= data_run_to) {
        files_to_download[filename_index] <- individual_log_filename
      }
    }
  }

  return(files_to_download)
}

#' filter_log_by_pipeline
#'
#' @description filter log files by specific pipeline i.e. "mrp", "incidence", "inla", "age_contour", "all"
#'
#' @param file_list data.frame all log files located in the bucket
#' @param pipeline string set specific pipeline or all pipelines
#'
#' @return files_to_download list of files that meet date criteria
#' @export

filter_log_by_pipeline <- function(file_list,
                                   pipeline = c("mrp", "incidence", "inla", "age_contour", "all")) {
  pipeline <- match.arg(pipeline)

  files_to_download <- list()

  if (pipeline == "all") {
    files_to_download <- as.list(file_list)
    print("All logs will be included")
  } else {
    for (filename_index in 1:length(file_list)) {
      individual_log_filename <- file_list[filename_index]

      if (grepl(pipeline, individual_log_filename) == TRUE) {
        files_to_download[filename_index] <- individual_log_filename
      }
    }
  }

  # remove null elements from list
  files_to_download[sapply(files_to_download, is.null)] <- NULL

  return(files_to_download)
}

#' @title filter_log_level
#'
#' @description function to return specific levels
#'
#' @param log data frame
#' @param level Levels to be filtered should be a character vector of any number of the following strings "INFO", "TRACE", "DEBUG", "WARN", "ERROR", "FATAL"
#'
#' return filtered data frame
#'
#' @export

filter_log_level <- function(log, level) {
  assertthat::assert_that(all(level %in% c("INFO", "TRACE", "DEBUG", "WARN", "ERROR", "FATAL")),
    msg = "Incorrect level names, please check your levels!"
  )

  filtered_log <- log %>%
    dplyr::filter(.data$log_level %in% level)

  return(filtered_log)
}

#' @title convert_log_to_df
#'
#' @description function to convert a log file to a data.frame
#'
#' @param file_path string path of the raw log file
#'
#' @export
#'
#' @examples
#' # Load the library to ensure example data sets are available
#' library(gcptools)
#'
#' # activate authentication token
#' # authenticate_GCP()
#'
#' # Create dummy log file
#' # gcptools::initialise_log(log_file = "example",
#' #                         log_dir = "convert_log_to_df_example")
#'
#' # file_path <- list.files("convert_log_to_df_example", full.names = TRUE)
#'
#' # call function
#' # dummy_output <- convert_log_to_df(file_path)
#'
#' # view output
#' # dummy_output
#'
#' # remove folder/files used in the example
#' # unlink("convert_log_to_df_example/", recursive = TRUE)
convert_log_to_df <- function(file_path) {
  suppressWarnings(log_file <- readr::read_log(file_path))

  ncol_log_file <- ncol(log_file)

  # remove NAs
  log_file[is.na(log_file)] <- ""

  # merge columns from the 4th onwards
  log_file <- log_file %>% tidyr::unite("message", .data$X4:paste0("X", ncol_log_file), remove = TRUE)

  # rename columns
  colnames(log_file) <- c("log_level", "timestamp", "namespace", "message")

  log_file <- as.data.frame(log_file)

  # remove namespace column
  log_file$namespace <- NULL

  return(log_file)
}
