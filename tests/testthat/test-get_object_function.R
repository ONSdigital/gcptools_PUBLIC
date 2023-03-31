testthat::test_that(
  "function returns a closure (i.e. a function)",
  {
    testthat::expect_equal(typeof(get_object_write_function("rdata")), "closure")
    testthat::expect_equal(typeof(get_object_write_function("rds")), "closure")
    testthat::expect_equal(typeof(get_object_write_function("csv")), "closure")
    testthat::expect_equal(typeof(get_object_write_function("xlsx")), "closure")
    testthat::expect_equal(typeof(get_object_write_function("yaml")), "closure")
    testthat::expect_equal(typeof(get_object_write_function("html")), "closure")
    testthat::expect_equal(typeof(get_object_write_function("jpeg")), "closure")
  }
)


testthat::test_that(
  "function returns an error if an incompatible file type is given",
  testthat::expect_error(get_object_write_function("qms"))
)
