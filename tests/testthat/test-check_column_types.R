testthat::test_that("Column types match", {
  testthat::expect_silent(check_column_types(dummy_csv, "prev_time_series_mrp_", dummy_config))
  testthat::expect_error(check_column_types(
    dummy_csv %>% dplyr::mutate(region = as.numeric(region)),
    "prev_time_series_mrp_",
    dummy_config
  ))
})
