#### LOG GET FUNCTIONS MODULE ####
# 1 get_package_version
# 2 get_list_of_commits
# 3 get_list_of_branches
# 4 get_git_status
# 5 get_current_branch
# 6 get_memory_usage
# 7 get_cpu_usage
# 8 get_log_dataframe_list
#### End of contents ####

#' @title get_package_version
#'
#' @description checks the version of the package you are using
#'
#' @param package string input for the package you intend to check the version of
#'
#' @return outputs version number of package as string
#'
#' @export
get_package_version <- function(package) {
  version_output <- utils::packageVersion(package)
  return(version_output)
}

#' @title get_list_of_commits
#'
#' @description retrieve a list of commits for a specific repository
#'
#' @return list of commits
#'
#' @export

get_list_of_commits <- function() {
  if (is.null(git2r::discover_repository(getwd()))) {
    return("Not in a git repository")
  }

  return(git2r::commits())
}

#' @title get_list_of_branches
#'
#' @description retrieve a list of branches
#'
#' @return list of branches
#'
#' @export

get_list_of_branches <- function() {
  if (is.null(git2r::discover_repository(getwd()))) {
    return("Not in a git repository")
  }

  return(git2r::branches())
}

#' @title get_git_status
#'
#' @description retrieve git status
#'
#' @return git status
#'
#' @export

get_git_status <- function() {
  if (is.null(git2r::discover_repository(getwd()))) {
    return("Not in a git repository")
  }

  return(git2r::status())
}

#' @title get_current_branch
#'
#' @description retrieve the current branch
#'
#' @return a list containing the name, type and repo of the currently checked out branch
#'
#' @export

get_current_branch <- function() {
  if (is.null(git2r::discover_repository(getwd()))) {
    return("Not in a git repository")
  }

  return(git2r::repository_head()$name)
}

#' @title get_memory_usage
#'
#' @description retrieves memory usage
#'
#' @return list of memory usage
#'
#' @export

get_memory_usage <- function() {
  memory_usage <- paste(strsplit(system2("free", args = "-m", stdout = T)[2], split = "\\s+")[[1]][3], "Mb")

  return(memory_usage)
}

#' @title get_cpu_usage
#'
#' @description retrieves cpu usage
#'
#' @return list of cpu usage
#' @importFrom rlang .data
#' @export

get_cpu_usage <- function() {
  output <- NCmisc::top(Table = TRUE)
  cpu_usage <- output$Table %>%
    dplyr::filter(.data$COMMAND %in% "R")

  return(cpu_usage)
}

#' @title get_log_dataframe_list
#'
#' @description reading in list of log file names and downloads them into a list of dataframe objects
#'
#' @param files_to_download list names of the logs files we want to download
#' @param top_level_path string path to search for log files
#' @param bucket desired output bucket
#'
#' @return log_list list of dataframe objects
#'
#' @export
#'
#' @examples
#' # Load the library to ensure the example data sets are available
#' # library(gcptools)
#'
#' # Set up example variables to test the code
#' # files_to_download <- data.frame(filename = c("inla_20211213_processed_log.csv",
#' #                                              "mrp_20211213_processed_log.csv"))
#' top_level_path <- "logs/processed/"
#' # bucket <- "polestar-prod-process-wip"
#'
#' # After loading in the variables you can call the function
#' # log_list <- get_log_dataframe_list(files_to_download, top_level_path, bucket)
#'
#' # View the output
#' # log_list
#'
get_log_dataframe_list <- function(files_to_download,
                                   top_level_path,
                                   bucket) {
  filenames <- as.list(files_to_download$filename)

  log_list <- lapply(
    X = filenames,
    FUN = function(filename, top_level_path, bucket) {
      log <- googleCloudStorageR::gcs_get_object(
        object_name = paste0(top_level_path, filename),
        bucket = bucket
      )

      return(log)
    },
    top_level_path = top_level_path,
    bucket = bucket
  )

  return(log_list)
}
