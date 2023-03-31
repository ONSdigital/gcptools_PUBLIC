dir.create("temp/")
temp_dir <- "temp/"

# creating dummy_configs
# creating empty lists for each country config and main config
main_dummy_config <- list()
England_dummy_config <- list()
Wales_dummy_config <- list()
Northern_Ireland_dummy_config <- list()
Scotland_dummy_config <- list()

# main_dummy_config
main_dummy_config$run_settings$analyst <- "Wonder Woman"

# England_dummy_config
England_dummy_config$model_settings$n_days_to_model <- 100
England_dummy_config$model_settings$knots <- NULL
England_dummy_config$model_settings$adapt_delta <- 0.95
England_dummy_config$model_settings$iterations <- 1500
England_dummy_config$model_settings$seed <- 12345

Wales_dummy_config$model_settings$n_days_to_model <- 120
Wales_dummy_config$model_settings$knots <- NULL
Wales_dummy_config$model_settings$adapt_delta <- 0.9
Wales_dummy_config$model_settings$iterations <- 1750
Wales_dummy_config$model_settings$seed <- 1234

Northern_Ireland_dummy_config$model_settings$n_days_to_model <- 130
Northern_Ireland_dummy_config$model_settings$knots <- NULL
Northern_Ireland_dummy_config$model_settings$adapt_delta <- 0.85
Northern_Ireland_dummy_config$model_settings$iterations <- 2000
Northern_Ireland_dummy_config$model_settings$seed <- 123

Scotland_dummy_config$model_settings$n_days_to_model <- 140
Scotland_dummy_config$model_settings$knots <- NULL
Scotland_dummy_config$model_settings$adapt_delta <- 0.8
Scotland_dummy_config$model_settings$iterations <- 2500
Scotland_dummy_config$model_settings$seed <- 12

dummy_country_configs <- list(
  England = England_dummy_config,
  Wales = Wales_dummy_config,
  Northern_Ireland = Northern_Ireland_dummy_config,
  Scotland = Scotland_dummy_config
)

dummy_log <- gcptools::initialise_log(
  log_dir = temp_dir,
  log_file = "dummy_log",
  log_threshold = "TRACE"
)

gcptools::log_incidence_model_info(
  main_config = main_dummy_config,
  country_configs = dummy_country_configs
)

testthat::test_that("Analyst name is logged correctly", {
  expect_true(any(grepl(x = readLines(dummy_log), pattern = "Analyst name: Wonder Woman")))
})

# calculating no. knots from n_days_to_model
england_knots <- (round((England_dummy_config$model_settings$n_days_to_model / 7) - 1))
wales_knots <- (round((Wales_dummy_config$model_settings$n_days_to_model / 7) - 1))
scotland_knots <- (round((Scotland_dummy_config$model_settings$n_days_to_model / 7) - 1))
northern_ireland_knots <- (round((Northern_Ireland_dummy_config$model_settings$n_days_to_model / 7) - 1))

testthat::test_that("Model information is logged correctly from the England config", {
  expect_true(any(grepl(x = readLines(dummy_log), pattern = "Model information: England knots: ", england_knots)))
  expect_true(any(grepl(x = readLines(dummy_log), pattern = "Model information: England adapt_delta: 0.95")))
  expect_true(any(grepl(x = readLines(dummy_log), pattern = "Model information: England iterations: 1500")))
  expect_true(any(grepl(x = readLines(dummy_log), pattern = "Model information: England seed: 12345")))
})

testthat::test_that("Model information is logged correctly from the Wales config", {
  expect_true(any(grepl(x = readLines(dummy_log), pattern = "Model information: Wales knots: ", wales_knots)))
  expect_true(any(grepl(x = readLines(dummy_log), pattern = "Model information: Wales adapt_delta: 0.9")))
  expect_true(any(grepl(x = readLines(dummy_log), pattern = "Model information: Wales iterations: 1750")))
  expect_true(any(grepl(x = readLines(dummy_log), pattern = "Model information: Wales seed: 1234")))
})

testthat::test_that("Model information is logged correctly from the Northern_Ireland config", {
  expect_true(any(grepl(x = readLines(dummy_log), pattern = "Model information: Northern_Ireland knots: ", northern_ireland_knots)))
  expect_true(any(grepl(x = readLines(dummy_log), pattern = "Model information: Northern_Ireland adapt_delta: 0.85")))
  expect_true(any(grepl(x = readLines(dummy_log), pattern = "Model information: Northern_Ireland iterations: 2000")))
  expect_true(any(grepl(x = readLines(dummy_log), pattern = "Model information: Northern_Ireland seed: 123")))
})

testthat::test_that("Model information is logged correctly from the Scotland config", {
  expect_true(any(grepl(x = readLines(dummy_log), pattern = "Model information: Scotland knots: ", scotland_knots)))
  expect_true(any(grepl(x = readLines(dummy_log), pattern = "Model information: Scotland adapt_delta: 0.8")))
  expect_true(any(grepl(x = readLines(dummy_log), pattern = "Model information: Scotland iterations: 2500")))
  expect_true(any(grepl(x = readLines(dummy_log), pattern = "Model information: Scotland seed: 12")))
})

unlink(temp_dir, recursive = TRUE, force = TRUE)
