dummy_csv <- data.frame(
  X = as.integer(1),
  region = "1",
  time = "1",
  mean = as.numeric(1),
  ll = as.numeric(1),
  ul = as.numeric(1),
  country = "1",
  variant = "1"
)

usethis::use_data(dummy_csv, overwrite = TRUE)
