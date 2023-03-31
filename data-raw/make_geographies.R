england_regions <- c(
  "North East", "North West", "Yorkshire and The Humber",
  "East Midlands", "West Midlands", "East of England",
  "London", "South East", "South West"
)
usethis::use_data(england_regions, overwrite = TRUE)


uk_countries <- c("England", "Northern Ireland", "Scotland", "Wales")
usethis::use_data(uk_countries, overwrite = TRUE)


nuts1_to_country_lookup <- c(
  "North East" = "England",
  "North West" = "England",
  "Yorkshire and The Humber" = "England",
  "East Midlands" = "England",
  "West Midlands" = "England",
  "East of England" = "England",
  "London" = "England",
  "South East" = "England",
  "South West" = "England",
  "Wales" = "Wales",
  "Northern Ireland" = "Northern Ireland",
  "Scotland" = "Scotland"
)
usethis::use_data(nuts1_to_country_lookup, overwrite = TRUE)
