testthat::test_that("Function stops if input is not a list", {
  testthat::expect_error(check_html_file(!list(), character(), dummy_config))
})
