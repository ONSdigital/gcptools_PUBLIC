testthat::test_that("Number of rows fails", {
  testthat::expect_error(
    check_number_of_rows(
      dummy_csv, "prev_time_series_mrp_",
      dummy_config
    ),
    pattern = "Number of rows do not match",
    fixed = TRUE
  )
})
