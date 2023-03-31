# creating empty list for main_config (config isn't used in this function directly)
main_config <- list()
temp_dir <- "temp/"

dummy_log <- gcptools::initialise_log(
  log_dir = temp_dir,
  log_file = "dummy_log",
  log_threshold = "TRACE"
)

log_model_info(main_config, "inla")

testthat::test_that("Username is logged correctly", {
  expect_true(any(grepl(x = readLines(dummy_log), pattern = "Username: ", Sys.info()["nodename"])))
})

testthat::test_that("An error message is produced if the pipeline argument doesn't match expected arguments", {
  expect_error(
    log_model_info(main_config, "not_a_pipeline"),
    "'arg' should be one of \"mrp\", \"age_contour\", \"incidence\", \"inla\""
  )
})


testthat::test_that("An error message is produced if pipeline is either mrp or incidence and country_config not supplied", {
  expect_error(
    log_model_info(main_config, "mrp"),
    "country_configs must be supplied if running log_model_info on the mrp or incidence pipelines"
  )

  expect_error(
    log_model_info(main_config, "incidence"),
    "country_configs must be supplied if running log_model_info on the mrp or incidence pipelines"
  )
})

unlink(temp_dir, recursive = TRUE, force = TRUE)
