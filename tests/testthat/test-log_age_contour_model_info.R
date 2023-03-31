dir.create("temp/")
temp_dir <- "temp/"

# load("/home/jupyter/CIS_age_contour/agecontour/tests/testdata/dummy_config.rda")
dummy_config_swabs <- list(run_settings = list(
  analyst = "Antman",
  swabs_or_antibodies = "swabs",
  swabs = list(model_settings = list(
    england = list(
      method = "REML",
      knots = list(
        study_day = 12,
        age = 17
      )
    ),
    da = list(
      method = "REML",
      knots = list(
        study_day = 19,
        age = 26
      )
    )
  ))
))

dummy_config_antibodies <- list(run_settings = list(
  analyst = "Spider-Man",
  swabs_or_antibodies = "antibodies",
  antibodies = list(model_settings = list(
    england = list(
      method = "UBRE",
      knots = list(
        study_day = 5,
        age = 25
      )
    ),
    da = list(
      method = "UBRE",
      knots = list(
        study_day = 50,
        age = 5
      )
    )
  ))
))

dummy_log1 <- gcptools::initialise_log(
  log_dir = temp_dir,
  log_file = "dummy_log1",
  log_threshold = "TRACE"
)

gcptools::log_age_contour_model_info(config = dummy_config_swabs)

dummy_log2 <- gcptools::initialise_log(
  log_dir = temp_dir,
  log_file = "dummy_log2",
  log_threshold = "TRACE"
)

gcptools::log_age_contour_model_info(config = dummy_config_antibodies)

testthat::test_that("Analyst name is logged correctly", {
  expect_true(any(grepl(x = readLines(dummy_log1), pattern = "Analyst name: Antman")))
  expect_true(any(grepl(x = readLines(dummy_log2), pattern = "Analyst name: Spider-Man")))
})

testthat::test_that("Swabs or antibodies is logged correctly", {
  expect_true(any(grepl(x = readLines(dummy_log1), pattern = "Swabs or antibodies: swabs")))
  expect_true(any(grepl(x = readLines(dummy_log2), pattern = "Swabs or antibodies: antibodies")))
})

testthat::test_that("England and DA methods are logged correctly", {
  expect_true(any(grepl(x = readLines(dummy_log1), pattern = "England method: REML")))
  expect_true(any(grepl(x = readLines(dummy_log1), pattern = "DA method: REML")))

  expect_true(any(grepl(x = readLines(dummy_log2), pattern = "England method: UBRE")))
  expect_true(any(grepl(x = readLines(dummy_log2), pattern = "DA method: UBRE")))
})

testthat::test_that("England knots are logged correctly", {
  expect_true(any(grepl(x = readLines(dummy_log1), pattern = "England knots: study_day: 12")))
  expect_true(any(grepl(x = readLines(dummy_log1), pattern = "England knots: age: 17")))

  expect_true(any(grepl(x = readLines(dummy_log2), pattern = "England knots: study_day: 5")))
  expect_true(any(grepl(x = readLines(dummy_log2), pattern = "England knots: age: 25")))
})

testthat::test_that("DA knots are logged correctly", {
  expect_true(any(grepl(x = readLines(dummy_log1), pattern = "DA knots: study_day: 19")))
  expect_true(any(grepl(x = readLines(dummy_log1), pattern = "DA knots: age: 26")))

  expect_true(any(grepl(x = readLines(dummy_log2), pattern = "DA knots: study_day: 50")))
  expect_true(any(grepl(x = readLines(dummy_log2), pattern = "DA knots: age: 5")))
})

unlink(temp_dir, recursive = TRUE, force = TRUE)
