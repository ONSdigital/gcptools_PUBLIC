#### DISCLOSURE CONTROL MODULE ####
# 1 name_pattern
# 2 column_names
# 3 column_types
# 4 number_of_rows
# 5 check_headers
# 6 disco_checks
#### End of contents ####

#' @title Check Name Pattern
#'
#' @description Takes a file name pattern to determine if it exists within config file.
#'
#' @param pattern A character string of the file name pattern to check. This takes the form of a file name with extension and any dates removed.
#'
#' @param config A list config file to check file names against. This file contains information about expected file metadata.
check_name_pattern <- function(pattern, config) {
  assertthat::assert_that(
    pattern %in% (config %>% names()),
    msg = "File name does not match"
  )
}

#' @title Check Column Names
#'
#' @description Takes a .csv file and name pattern to determine if column names match as expected.
#'
#' @param file A .csv file to extract column names from.
#'
#' @param pattern A character string of the file name pattern to check. This takes the form of a file name with extension and any dates removed.
#'
#' @param config A list config file to check column names against. This file contains information about expected file metadata.
check_column_names <- function(file, pattern, config) {
  file_col_names <- names(file)

  correct_col_names <- config[[pattern]]$names_and_types %>%
    names()

  assertthat::assert_that(
    setequal(file_col_names, correct_col_names),
    msg = "Column names do not match"
  )
}

#' @title Check Column Types
#'
#' @description Takes a .csv file and name pattern to determine if column types match as expected.
#'
#' @param file A .csv file to extract column types from.
#'
#' @param pattern A character string of the file name pattern to check. This takes the form of a file name with extension and any dates removed.
#'
#' @param config A list config file to check column types against. This file contains information about expected file metadata.
check_column_types <- function(file, pattern, config) {
  file_col_types <- sapply(file, class) %>%
    unname()

  correct_col_types <- config[[pattern]]$names_and_types %>%
    unlist() %>%
    unname()

  assertthat::assert_that(
    isTRUE(all.equal(file_col_types, correct_col_types)),
    msg = "Column types do not match"
  )
}

#' @title Check Number of Rows
#'
#' @description Takes a .csv file, name pattern and config file to determine if the number of rows match as expected.
#'
#' @param file A .csv file to extract number of rows from.
#'
#' @param pattern A character string of the file name pattern to check. This takes the form of a file name with extension and any dates removed.
#'
#' @param config A list config file to check number of rows against. This file contains information about expected file metadata.
check_number_of_rows <- function(file, pattern, config) {
  file_no_of_rows <- nrow(file)

  correct_no_of_rows <- config[[pattern]]$number_of_rows

  # If the file does not have a standard number of rows, check that the file's number of rows fits within a range.
  if (config[[pattern]]$fixed_number_of_rows == TRUE) {
    assertthat::assert_that(
      file_no_of_rows == correct_no_of_rows,
      msg = "Number of rows do not match"
    )
  } else {
    assertthat::assert_that(
      file_no_of_rows > min(correct_no_of_rows),
      msg = "Number of rows is below allowed lower limit"
    )
  }
}

#' @title Check CSV File
#'
#' @description Function to run .csv specific checks on a .csv file
#'
#' @param file A .csv file to check.
#'
#' @param pattern A character string of the file name pattern to check. This takes the form of a file name with extension and any dates removed.
#'
#' @param config A list config file to check file attributes against. This file contains information about expected file metadata.
check_csv_file <- function(file, pattern, config) {
  assertive.types::assert_is_data.frame(file)

  check_column_names(file = file, pattern = pattern, config = config)
  check_column_types(file = file, pattern = pattern, config = config)
  check_number_of_rows(file = file, pattern = pattern, config = config)
}

#' @title Check Headers
#'
#' @description Checking h1, h2 and h3 header elements match as expected.
#'
#' @param file An .html file to extract headers from.
#'
#' @param header A character string of header type - either h1, h2, or h3.
#'
#' @param pattern A character string of the file name pattern to check. This takes the form of a file name with extension and any dates removed.
#'
#' @param config A list config file to check headers against. This file contains information about expected file metadata.
check_headers <- function(file, header, pattern, config) {
  file_headers <- file %>%
    rvest::html_elements(header) %>%
    rvest::html_text() %>%
    stringr::str_replace("[0-9]{8}", "")

  correct_headers <- config[[pattern]][[header]]

  assertthat::assert_that(
    setequal(file_headers, correct_headers),
    msg = "Document headers do not match"
  )
}

#' @title Check Number of Tables
#'
#' @description Checking number of tables in a .html document match as expected.
#'
#' @param file An .html file to extract number of tables from.
#'
#' @param pattern A character string of the file name pattern to check. This takes the form of a file name with extension and any dates removed.
#'
#' @param config A list config file to check number of tables against. This file contains information about expected file metadata.
check_number_of_tables <- function(file, pattern, config) {
  file_no_of_tables <- file %>%
    rvest::html_elements("table") %>%
    length()

  correct_no_of_tables <- config[[pattern]]$number_of_tables

  assertthat::assert_that(
    file_no_of_tables == correct_no_of_tables,
    msg = "Number of tables do not match"
  )
}

#' @title Check Number of Charts
#'
#' @description Checking number of charts in a .html document match as expected.
#'
#' @param file An .html file to extract number of charts from.
#'
#' @param pattern A character string of the file name pattern to check. This takes the form of a file name with extension and any dates removed.
#'
#' @param config A list config file to check number of charts against. This file contains information about expected file metadata.
check_number_of_charts <- function(file, pattern, config) {
  file_no_of_charts <- file %>%
    rvest::html_elements("img") %>%
    length()

  correct_no_of_charts <- config[[pattern]]$number_of_charts

  assertthat::assert_that(
    file_no_of_charts == correct_no_of_charts,
    msg = "Number of charts do not match"
  )
}

#' @title Check HTML File
#'
#' @description Function to run .html specific checks on an .html file
#'
#' @param file An .html file to check.
#'
#' @param pattern A character string of the file name pattern to check. This takes the form of a file name with extension and any dates removed.
#'
#' @param config A list config file to check file attributes against. This file contains information about expected file metadata.
check_html_file <- function(file, pattern, config) {
  assertive.types::assert_is_list(file)

  check_headers(file = file, header = "h1", pattern = pattern, config = config)
  check_headers(file = file, header = "h2", pattern = pattern, config = config)
  check_headers(file = file, header = "h3", pattern = pattern, config = config)
  check_number_of_tables(file = file, pattern = pattern, config = config)
  check_number_of_charts(file = file, pattern = pattern, config = config)
}


#' @title Check YAML Names
#'
#' @description Function to check that names in .yaml file match as expected.
#'
#' @param file A .yaml file to extract names from.
#'
#' @param pattern A character string of the file name pattern to check. This takes the form of a file name with extension and any dates removed.
#'
#' @param config A list config file to check file attributes against. This file contains information about expected file metadata.
check_yaml_names <- function(file, pattern, config) {
  file_names <- file %>% names()

  correct_names <- config[[pattern]]$names

  assertthat::assert_that(
    isTRUE(all.equal(file_names, correct_names)),
    msg = "Names do not match",
    fixed = TRUE
  )
}

#' @title Check YAML File
#'
#' @description Function to run .yaml specific checks on a .yaml file
#'
#' @param file A .yaml file to check.
#'
#' @param pattern A character string of the file name pattern to check. This takes the form of a file name with extension and any dates removed.
#'
#' @param config A list config file to check file attributes against. This file contains information about expected file metadata.
check_yaml_file <- function(file, pattern, config) {
  assertive.types::assert_is_list(file)

  check_yaml_names(file = file, pattern = pattern, config = config)
}

#' @title Disco Checks
#'
#' @description Takes a file and file path and determines whether or not the file passes automated disclosure checks.
#'
#' @param path A character string file path of file to be checked.
#'
#' @param file File to be checked. Can take the form of a .csv, .html, .yaml or .jpeg.
#'
#' @param config A list config file to check file attributes against. This file contains information about expected file metadata.
#'
#' @param type A character string of the file type.
#'
#' @return Pass/fail messages
#'
#' @export
disco_checks <- function(path, file, config, type = c(".csv", ".jpeg|.jpg", ".yaml|.yml", ".html")) {
  assertive.types::assert_is_character(path)
  assertive.types::assert_is_list(config)

  type <- match.arg(type)

  # Regexp extracts portion of file path before date and after last forward slash (if present)
  # The result is a file pattern that is used to filter the config file.
  input_name_pattern <- stringr::str_extract(path, paste0("([^\\/]+)+$")) %>%
    stringr::str_remove_all("[0-9]{8}.+$") %>%
    stringr::str_remove_all(type)

  check_name_pattern(pattern = input_name_pattern, config = config)

  if (type == ".csv") {
    check_csv_file(file = file, pattern = input_name_pattern, config = config)
  } else if (type == ".yaml") {
    check_yaml_file(file = file, pattern = input_name_pattern, config = config)
  } else if (type == ".html") {
    check_html_file(file = file, pattern = input_name_pattern, config = config)
  }

  message("All checks pass")
}
