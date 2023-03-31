testthat::test_that("Empty list returns error", {
  testthat::expect_error(check_number_of_charts(list(), "configs_mrp_", dummy_config))
})
