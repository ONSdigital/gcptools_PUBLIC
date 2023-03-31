dummy_config <- list()

dummy_config$prev_time_series_mrp_$expected_no_of_cols <- 8

dummy_config$prev_time_series_mrp_$names_and_types$X <- "integer"
dummy_config$prev_time_series_mrp_$names_and_types$region <- "character"
dummy_config$prev_time_series_mrp_$names_and_types$time <- "character"
dummy_config$prev_time_series_mrp_$names_and_types$mean <- "numeric"
dummy_config$prev_time_series_mrp_$names_and_types$ll <- "numeric"
dummy_config$prev_time_series_mrp_$names_and_types$ul <- "numeric"
dummy_config$prev_time_series_mrp_$names_and_types$country <- "character"
dummy_config$prev_time_series_mrp_$names_and_types$variant <- "character"

dummy_config$prev_time_series_mrp_$fixed_number_of_rows <- TRUE

dummy_config$prev_time_series_mrp_$number_of_rows <- 2632

dummy_config$configs_mrp_$names <- c(
  "run_settings",
  "paths",
  "GCP",
  "Northern_Ireland",
  "Wales",
  "Scotland",
  "England"
)

usethis::use_data(dummy_config, overwrite = TRUE)
