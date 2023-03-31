testthat::test_that(".yaml names match", {
  testthat::expect_silent(check_yaml_names(
    dummy_yaml,
    "configs_mrp_",
    dummy_config
  ))
})

testthat::test_that("error if names do not match", {
  testthat::expect_error(check_yaml_names(
    list(),
    "configs_mrp_",
    dummy_config
  ))
})
