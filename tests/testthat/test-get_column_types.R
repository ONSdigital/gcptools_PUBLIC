dummy_config <- list(
  column_types = list(
    from_date = "col_date()",
    to_date = "col_date()",
    geography_area = "col_factor()",
    geography_code = "col_factor()",
    sex_group = "col_factor()",
    ethnicity_group = "col_factor()",
    age_group = "col_factor()",
    measurement = "col_character()",
    positive_sample = "col_integer()",
    total_sample = "col_integer()",
    population_total = "col_number()",
    model = "col_character()"
  )
)

# "DDfffffciinc": Each letter coresponds to a column type i.e., D=Date, f=factor, c=character, i=integer, n=numeric

testthat::test_that("function returns type of column", {
  testthat::expect_equal(as.character(get_column_types(dummy_config)), "DDfffffciinc")
})

testthat::test_that("function returns correct type of output", {
  testthat::expect_equal(class(get_column_types(dummy_config)), "col_spec")
})

testthat::test_that("function returns an error if column_types does not exist in config", {
  dummy_config2 <- list("Number" = c(1, 2, 3))
  testthat::expect_error(
    get_column_types(dummy_config2),
    "column_types does not exist in config. Please check your config"
  )
})
