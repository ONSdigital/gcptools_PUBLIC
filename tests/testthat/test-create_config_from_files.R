dir.create("temp_config/")
dir.create("temp_config/models/")

dummy_run_settings <- list(data_run = "20220102")
dummy_model_settings <- list(family = "binomial")

yaml::write_yaml(dummy_run_settings, "temp_config/dummy_run_settings.yaml")
yaml::write_yaml(dummy_model_settings, "temp_config/models/dummy_model_settings.yaml")

dummy_output <- create_config("temp_config/dummy_run_settings.yaml", "temp_config/models/dummy_model_settings.yaml")

test_that("function returns a list", {
  expect_true(is.list(dummy_output))
})

test_that("function returns a list with named elements dummy_run_settings and dummy_model_settings", {
  expect_named(dummy_output, c("dummy_run_settings", "dummy_model_settings"))
})

unlink("temp_config", recursive = TRUE, force = TRUE)
