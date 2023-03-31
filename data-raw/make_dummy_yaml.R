dummy_yaml <- list()

dummy_yaml$run_settings <- 1
dummy_yaml$paths <- 1
dummy_yaml$GCP <- 1
dummy_yaml$Northern_Ireland <- 1
dummy_yaml$Wales <- 1
dummy_yaml$Scotland <- 1
dummy_yaml$England <- 1

usethis::use_data(dummy_yaml, overwrite = TRUE)
