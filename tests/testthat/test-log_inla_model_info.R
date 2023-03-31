dir.create("temp/")
temp_dir <- "temp/"

dummy_config1 <- list(
  run_settings = list(analyst = "Aquaman"),
  models_values = list(
    model_names = c("GB_AB", "NI_AB", "GB_VAC", "NI_VAC"),
    vaccine_doses = c("one", "two", "three")
  ),
  model_settings = list(
    n_posterior_samples = 6000,
    seed = 1234
  )
)

dummy_config2 <- list(
  run_settings = list(analyst = "Batman"),
  models_values = list(
    model_names = c("GB_AB", "GB_VAC"),
    vaccine_doses = "one"
  ),
  model_settings = list(
    n_posterior_samples = 1337,
    seed = 42
  )
)

# creating a log
dummy_log1 <- gcptools::initialise_log(
  log_dir = temp_dir,
  log_file = "dummy_log1",
  log_threshold = "TRACE"
)

gcptools::log_inla_model_info(config = dummy_config1)

dummy_log2 <- gcptools::initialise_log(
  log_dir = temp_dir,
  log_file = "dummy_log2",
  log_threshold = "TRACE"
)

gcptools::log_inla_model_info(config = dummy_config2)

testthat::test_that("Analyst name is logged correctly", {
  expect_true(any(grepl(x = readLines(dummy_log1), pattern = "Analyst name: Aquaman")))
  expect_true(any(grepl(x = readLines(dummy_log2), pattern = "Analyst name: Batman")))
})

testthat::test_that("Models running is logged correctly", {
  expect_true(any(grepl(
    x = readLines(dummy_log1),
    pattern = "Running for models: GB_AB, NI_AB, GB_VAC, NI_VAC"
  )))
  expect_true(any(grepl(
    x = readLines(dummy_log2),
    pattern = "Running for models: GB_AB, GB_VAC"
  )))
})

testthat::test_that("Vaccine doses is logged correctly", {
  expect_true(any(grepl(
    x = readLines(dummy_log1),
    pattern = "Running for vaccine doses: one, two, three"
  )))
  expect_true(any(grepl(
    x = readLines(dummy_log2),
    pattern = "Running for vaccine doses: one"
  )))
})

testthat::test_that("Number of posterior samples is logged correctly", {
  expect_true(any(grepl(x = readLines(dummy_log1), pattern = "Model information: Number of posterior samples: 6000")))
  expect_true(any(grepl(x = readLines(dummy_log2), pattern = "Model information: Number of posterior samples: 1337")))
})

testthat::test_that("Seed is logged correctly", {
  expect_true(any(grepl(x = readLines(dummy_log1), pattern = "Model information: seed: 1234")))
  expect_true(any(grepl(x = readLines(dummy_log2), pattern = "Model information: seed: 42")))
})

unlink(temp_dir, recursive = TRUE, force = TRUE)
