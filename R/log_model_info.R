#### LOG MODEL INFO MODULE ####
# 1 log_age_contour_model
# 2 log_inla_model_info
# 3 log_mrp_model_info
# 4 log_incidence_model_info
# 5 log_model_info
#### End of contents ####

#' @title log_age_contour_model_info
#' @description Logs the username, analyst name and model information for the age contour pipeline
#' @param config the config variable (after it has been loaded in as a list using load_config)
#'
#' @return invisible
#' @export

log_age_contour_model_info <- function(config) {
  swabs_or_AB <- config$run_settings$swabs_or_antibodies
  model_settings <- config$run_settings[[swabs_or_AB]]$model_settings

  futile.logger::flog.info(paste0("Analyst name: ", config$run_settings$analyst))

  futile.logger::flog.info(paste0("Model information: Swabs or antibodies: ", swabs_or_AB))

  for (country in c("England", "DA")) {
    futile.logger::flog.info(paste0("Model information: ", country, " method: ", model_settings[[tolower(country)]]$method))
    futile.logger::flog.info(paste0("Model information: ", country, " knots: study_day: ", model_settings[[tolower(country)]]$knots$study_day))
    futile.logger::flog.info(paste0("Model information: ", country, " knots: age: ", model_settings[[tolower(country)]]$knots$age))
  }
  return(invisible())
}

#' @title log_inla_model_info
#' @description Logs the analyst name and model information for the INLA pipeline
#' @param config the config variable (after it has been loaded in as a list using load_config)
#'
#' @return invisible
#' @export

log_inla_model_info <- function(config) {
  futile.logger::flog.info(paste0("Analyst name: ", config$run_settings$analyst))

  futile.logger::flog.info(paste0("Running for models: ", toString(config$models_values$model_names)))
  futile.logger::flog.info(paste0("Running for vaccine doses: ", toString(config$models_values$vaccine_doses)))

  futile.logger::flog.info(paste0("Model information: Number of posterior samples: ", config$model_settings$n_posterior_samples))

  futile.logger::flog.info(paste0("Model information: seed: ", config$model_settings$seed))
  return(invisible())
}

#' @title log_mrp_model_info
#' @description Logs the analyst name and model information for the MRP pipeline
#' @param main_config the main config variable (after it has been loaded in as a list using load_config)
#' @param country_configs the country_configs list (after they have been loaded in using load_country_configs)
#'
#' @return invisible
#' @export
log_mrp_model_info <- function(main_config, country_configs) {
  futile.logger::flog.info(paste0("Analyst name: ", main_config$run_settings$analyst))

  for (country in names(country_configs)) {
    futile.logger::flog.info(paste0("Model information: ", country, " knots: ", country_configs[[country]]$model_settings$knots))

    futile.logger::flog.info(paste0("Model information: ", country, " adapt_delta: ", country_configs[[country]]$model_settings$adapt_delta))

    futile.logger::flog.info(paste0("Model information: ", country, " iterations: ", country_configs[[country]]$model_settings$iterations))

    futile.logger::flog.info(paste0("Model information: ", country, " seed: ", country_configs[[country]]$model_settings$seed))
  }
  return(invisible())
}

#' @title log_incidence_model_info
#' @description Logs the analyst name and model information for the incidence pipeline
#' @param main_config the main config variable (after it has been loaded in as a list using load_config)
#' @param country_configs the country_configs list (after they have been loaded in using load_country_configs)
#'
#' @return invisible
#' @export
log_incidence_model_info <- function(main_config, country_configs) {
  futile.logger::flog.info(paste0("Analyst name: ", main_config$run_settings$analyst))

  for (country in names(country_configs)) {
    n_days_to_model <- country_configs[[country]]$run_settings$n_days_to_model

    if (is.null(country_configs[[country]]$model_settings$knots)) {
      knots <- (round((n_days_to_model / 7) - 1))
    } else {
      knots <- country_configs[[country]]$model_settings$knots
    }

    futile.logger::flog.info(paste0("Model information: ", country, " knots: ", knots))

    futile.logger::flog.info(paste0("Model information: ", country, " adapt_delta: ", country_configs[[country]]$model_settings$adapt_delta))

    futile.logger::flog.info(paste0("Model information: ", country, " iterations: ", country_configs[[country]]$model_settings$iterations))

    futile.logger::flog.info(paste0("Model information: ", country, " seed: ", country_configs[[country]]$model_settings$seed))
  }
  return(invisible())
}

#' @title log_model_info
#' @description this function logs the username of the user and based on pipeline specified will run either the log_mrp_model_info, log_age_contour_model_info, log_incidence_model_info or log_inla_model_info functions, which will log model information for their respective models
#' @param main_config the main config variable (after it has been loaded in as a list using load_config)
#' @param country_configs the country_configs list (after they have been loaded in using load_country_configs) - this is required for the mrp and incidence pipelines but optional for other pipelines
#' @param pipeline the pipeline that you are running - this should be either mrp, age_contour, incidence or inla
#'
#' @return invisible
#' @export
log_model_info <- function(main_config, pipeline = c("mrp", "age_contour", "incidence", "inla"), country_configs = NULL) {
  match.arg(pipeline)

  if (missing(country_configs)) {
    assertthat::assert_that(!(pipeline %in% c("mrp", "incidence")), msg = "country_configs must be supplied if running log_model_info on the mrp or incidence pipelines")
  }

  futile.logger::flog.info(paste0("Username: ", Sys.info()["nodename"]))

  if (pipeline == "mrp") {
    log_mrp_model_info(main_config = main_config, country_configs = country_configs)
  }

  if (pipeline == "age_contour") {
    log_age_contour_model_info(config = main_config)
  }

  if (pipeline == "incidence") {
    log_incidence_model_info(main_config = main_config, country_configs = country_configs)
  }

  if (pipeline == "inla") {
    log_inla_model_info(config = main_config)
  }
}
