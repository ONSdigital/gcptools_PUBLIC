testthat::test_that("Column names match", {
  testthat::expect_silent(check_column_names(dummy_csv, "prev_time_series_mrp_", dummy_config))
  testthat::expect_error(check_column_names(
    dummy_csv %>% dplyr::select(-ll),
    "prev_time_series_mrp_",
    dummy_config
  ))
})
