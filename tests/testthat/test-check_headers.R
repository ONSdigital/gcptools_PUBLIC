testthat::test_that("Empty list returns error", {
  testthat::expect_error(check_headers(list(), "h1", "configs_mrp_", dummy_config))
})
