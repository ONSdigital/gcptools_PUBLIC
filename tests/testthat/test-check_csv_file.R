testthat::test_that("Function stops if input is not a data.frame", {
  testthat::expect_error(check_csv_file(!data.frame(), character(), list()))
})
