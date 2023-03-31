##################################################################################################
# 1 get_filepaths_from_project_name
# 2 explore_column
# 3 get_unique_values
# 4 reveal_low_counts
# 5 print_jpeg
# 6 retrieve_html_file
##################################################################################################


#' @title get_filepaths_from_project_name
#'
#' @description function to search all of the available filepaths witihin a given project_name and return it as a dataframe
#'
#' @param project_name a string containing the exact project name (case-sensitive) which you want to get the filepaths from
#'
#' @return dataframe containing the filepaths of all of the files within that project
#'
#' @export

get_filepaths_from_project_name <- function(project_name, file_type = c(".csv", ".jpeg|.jpg", ".yaml|.yml", ".html")) {
  file_type <- match.arg(file_type)

  project_pattern <- paste0("^", project_name)

  project_files <- grep(
    x = googleCloudStorageR::gcs_list_objects(gcp_paths$review_bucket)$name,
    pattern = project_pattern,
    value = TRUE
  )

  approved_file_patterns <- file_type

  project_files <- grep(
    x = project_files,
    pattern = approved_file_patterns,
    value = TRUE
  )

  as.data.frame(project_files)
}

#' @title explore_column
#'
#' @description function for use in disclosure control to explore the contents of a particular column
#'
#' @param data a dataframe containing the data you want to explore
#' @param column_name a string containing the exact name (case-sensitive) of the column you want to explore
#'
#' @return either unique values or a percentage of low counts under a given threshold (default 10)
#'
#' @export

explore_column <- function(data, column_name) {
  column_type <- typeof(data[[column_name]])

  if (column_type == "character") {
    get_unique_values(data, column_name)
  } else if (column_type %in% c("double", "integer")) {
    reveal_low_counts(data, column_name)
  }
}


#' @title get_unique_values
#'
#' @description function for use in disclosure control to explore the contents of a character or factor type column
#'
#' @param data a dataframe containing the data you want to explore
#' @param column_name a string containing the exact name (case-sensitive) of the column you want to explore
#'
#' @return a pretty list containing the unique values within that column
#'
#' @export

get_unique_values <- function(data, column_name) {
  unique_values <- data %>%
    dplyr::pull(column_name) %>%
    unique()

  cat(c("Unique values:", unique_values), sep = "\n   ")
}

#' @title reveal_low_counts
#'
#' @description function for use in disclosure control to explore the percentage of low counts within a given numeric column
#'
#' @param data a dataframe containing the data you want to explore
#' @param column_name a string containing the exact name (case-sensitive) of the column you want to explore
#' @param low_count_threshold the threshold at which counts are considered disclosive (default 10)
#'
#' @return a message to the console which states the percentage of low counts within the column.
#'
#' @export

reveal_low_counts <- function(data, column_name, low_count_threshold = 10) {
  perc_less_than_10 <- data %>%
    dplyr::filter(!!sym(column_name) < low_count_threshold) %>%
    dplyr::summarise(perc_less_than_10 = paste0(round(n() / nrow(data), 2) * 100, "%")) %>%
    dplyr::pull()

  paste(perc_less_than_10, "of rows contain values less than 10")
}

#' @title print_jpeg
#'
#' @description function to print jpegs to the console
#'
#' @param jpeg_filepath a string containing a jpeg file path (including folder name) that you want to print to the console
#' @param bucket the bucket which you want to read the jpeg from
#'
#' @return an image of the jpeg within the console
#'
#' @export

print_jpeg <- function(jpeg_filepath, bucket = gcp_paths$review_bucket) {
  jpeg <- suppressMessages(googleCloudStorageR::gcs_get_object(jpeg_filepath, bucket = bucket))

  options(repr.plot.width = 25, repr.plot.height = 25)

  plot(0:3000, type = "n")

  rasterImage(image = jpeg, 0, 0, 3000, 3000)
}


#' @title retrieve_html_file
#'
#' @description function to retrieve html file from the review bucket and put it in the QA_reports folder
#'
#' @param html_filepath a string containing a html file path (including folder name) that you want to save to the QA_reports folder
#'
#' @return a message saying where to find the html file
#'
#' @export

retrieve_html_file <- function(html_filepath) {
  main_directory <- "/home/jupyter"
  sub_directory <- "QA_reports"

  if (!file.exists(paste0(main_directory, "/", sub_directory))) {
    dir.create(file.path(main_directory, sub_directory))
    setwd(file.path(main_directory, sub_directory))
  }

  suppressMessages(gcptools::download_qa_report_to_notebook(
    file = html_filepath,
    bucket = gcp_paths$review_bucket
  ))

  print(paste0("The ", html_filepath, " file has been saved in the '", main_directory, "/", sub_directory, "' folder"))
}
