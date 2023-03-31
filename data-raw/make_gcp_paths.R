gcp_paths <- list(data_bucket = "ons-psplus-data-prod-psplus-cis-data",
                 review_bucket = "ons-psplus-analysis-prod-cis-review",
                 wip_bucket = "ons-psplus-analysis-prod-cis-wip")

usethis::use_data(gcp_paths, overwrite = TRUE)