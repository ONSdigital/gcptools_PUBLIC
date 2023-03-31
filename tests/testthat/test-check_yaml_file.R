testthat::test_that("Function stops if input is not a list", {
  testthat::expect_error(check_yaml_file(!list(), character(), config()))
})
