dir.create("temp/")
temp_dir <- "temp/"

dummy_log_1e_2w <- initialise_log(
  log_dir = temp_dir,
  log_file = "dummy_log_1e_2w",
  log_threshold = "TRACE"
)

futile.logger::flog.info("WARN")
futile.logger::flog.info("ERROR")
futile.logger::flog.warn("WARN one")
futile.logger::flog.warn("WARN two")
futile.logger::flog.error("ERROR one")
count_warnings_and_errors(dummy_log_1e_2w)

dummy_log_2e_1w <- initialise_log(
  log_dir = temp_dir,
  log_file = "dummy_log_2e_1w",
  log_threshold = "TRACE"
)

futile.logger::flog.info("WARN")
futile.logger::flog.info("ERROR")
futile.logger::flog.warn("WARN one")
futile.logger::flog.error("ERROR one")
futile.logger::flog.error("ERROR two")
count_warnings_and_errors(dummy_log_2e_1w)

dummy_log_3e_3w <- initialise_log(
  log_dir = temp_dir,
  log_file = "dummy_log_3e_3w",
  log_threshold = "TRACE"
)

futile.logger::flog.info("WARN")
futile.logger::flog.info("ERROR")
futile.logger::flog.warn("WARN one")
futile.logger::flog.warn("WARN two")
futile.logger::flog.warn("WARN three")
futile.logger::flog.error("ERROR one")
futile.logger::flog.error("ERROR two")
futile.logger::flog.error("ERROR three")
count_warnings_and_errors(dummy_log_3e_3w)

testthat::test_that("number of warnings and number of errors matches what is expected", {
  expect_true(any(grepl(x = readLines(dummy_log_1e_2w), pattern = "error messages is: 1")))
  expect_true(any(grepl(x = readLines(dummy_log_1e_2w), pattern = "warning messages is: 2")))

  expect_true(any(grepl(x = readLines(dummy_log_2e_1w), pattern = "error messages is: 2")))
  expect_true(any(grepl(x = readLines(dummy_log_2e_1w), pattern = "warning messages is: 1")))

  expect_true(any(grepl(x = readLines(dummy_log_3e_3w), pattern = "error messages is: 3")))
  expect_true(any(grepl(x = readLines(dummy_log_3e_3w), pattern = "warning messages is: 3")))
})

unlink(temp_dir, recursive = TRUE, force = TRUE)
