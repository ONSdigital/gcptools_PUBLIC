testthat::test_that("File patterns match", {
  testthat::expect_silent(check_name_pattern("prev_time_series_mrp_", dummy_config))
  testthat::expect_error(check_name_pattern("a", dummy_config),
    pattern = "File name does not match",
    fixed = TRUE
  )
})
