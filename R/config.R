#### CONFIG MODULE ####
# 1 create_config
# 2 create_config_from_files
# 3 create_config_from_folder
# 4 get_column_types
#### End of contents ####

#' @title create_config
#'
#' @description A function that reads in configuration settings from either a folder or file(s).
#'
#' @param ... Either folder to read in, e.g. create_config("configuration", subfolder = "model")
#' or filepaths to read in, e.g. create_config("config.yaml", "config2.yaml")
#'
#' @return list
#'
#' @family Config Creation Functions
#'
#' @export
create_config <- function(...) {
  files <- list(...)

  if (length(files) == 2 & any(sapply(files, dir.exists))) {
    config <- create_config_from_folder(folder = files[[1]], subfolder = files[[2]])
  } else {
    config <- create_config_from_files(...)
  }

  return(config)
}

#' @title create_config_from_files
#'
#' @description function to append config files to a single list
#'
#' @param ... filepaths to read in, e.g. create_config("config.yaml", "config2.yaml")
#'
#' @return list
#'
#' @family Config Creation Functions
#'
#' @export
create_config_from_files <- function(...) {
  files <- list(...)
  contents <- purrr::map(files, yaml::read_yaml)
  names(contents) <- purrr::map(files, clean_file)
  return(contents)
}

#' @title create_config_from_folder
#'
#' @description A function to append config files to a single list from a folder.
#'
#' @param folder This is the path to the folder containing configuration settings
#'
#' @param subfolder This is a subfolder within folder.
#' The contents of this subfolder will be recursively read in and flattened
#' and added to the output list as element "subfolder".
#'
#' @return list
#'
#' @family Config Creation Functions
#'
#' @export
create_config_from_folder <- function(folder, subfolder = "models") {
  top_names <- list.files(folder, full.names = TRUE, pattern = ".yaml")

  top_contents <- purrr::map(top_names, yaml::read_yaml) %>%
    purrr::set_names(clean_file(top_names))

  recursive_file_names <- list.files(folder, full.names = TRUE, recursive = TRUE)

  model_names <- grep(pattern = glue::glue("/{subfolder}/"), x = recursive_file_names, value = TRUE)

  model_contents <- purrr::map(model_names, yaml::read_yaml) %>%
    purrr::set_names(clean_file(model_names))

  configuration <- c(
    top_contents,
    list(model_contents)
  )
  names(configuration)[length(configuration)] <- subfolder

  configuration <- purrr::compact(configuration)

  return(configuration)
}

#' @title get column types
#'
#' @description reading column types from config and returning an evaluated string
#'
#' @param config contains directory information on this occasion
#'
#' @returns evaluated string of column names and types
#'
#' @export
get_column_types <- function(config) {
  assertthat::assert_that("column_types" %in% names(config),
    msg = "column_types does not exist in config. Please check your config"
  )

  # Read in info on column names and specified types from config
  type_cols <- unlist(config$column_types)
  name_cols <- names(type_cols)

  # Stitching together the main part of string to be evaluated as syntax
  str_eval_main <- paste0(name_cols, " = readr::", type_cols)
  str_eval_main[-length(str_eval_main)] <- paste0(str_eval_main[-length(str_eval_main)], ", ")
  str_eval_main <- paste0(str_eval_main, collapse = "")

  # Adding remaining bits of string so that the evaulated string creates col_type_vec object
  # col_type_vec can then be used to specify column types for col_types argument within readr::read_csv()
  str_eval <- paste0(
    "col_type_vec <- readr::cols(",
    str_eval_main,
    ")"
  )

  eval(parse(text = str_eval))

  return(col_type_vec)
}
