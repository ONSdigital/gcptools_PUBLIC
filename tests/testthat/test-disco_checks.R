testthat::test_that("Wrong data types return an error", {
  testthat::expect_error(disco_checks(!character(), dummy_csv, list()))
  testthat::expect_error(disco_checks(character(), data.frame(), !list()))
})

testthat::test_that("Incorrect file type input returns an error", {
  testthat::expect_error(disco_checks(character(), data.frame(), list(), ".cv"))
})
