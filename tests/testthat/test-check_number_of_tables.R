testthat::test_that("Empty list returns error", {
  testthat::expect_error(check_number_of_tables(list(), "configs_mrp_", dummy_config))
})
