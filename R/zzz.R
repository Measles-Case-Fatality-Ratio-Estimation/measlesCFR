mrbrt <- NULL

.onLoad <- function(libname, pkgname) {
  ## Download pip packages for  dill and mrtool
  reticulate::virtualenv_create("measles_conda")
  reticulate::use_virtualenv("measles_conda", required = TRUE)
  env <- reticulate::py_config()$python
  ## reticulate::use_python(env, required = T)
  Sys.setenv("RETICULATE_PYTHON" = env)
  reticulate::py_config()
  reticulate::py_install("measles_conda", packages = c("mrtool==0.1.0"), pip = TRUE)
  reticulate::py_install("measles_conda", packages = c("dill==0.3.6"), pip = TRUE)
  mrbrt <<- reticulate::import("mrtool", delay_load = TRUE)

  ## Load baseline and vaccination covariate CSVs (used in predictCFR.R)
  baseline_covariates = system.file("extdata", "transformed_standardized_covaraite_set_with_incidence.csv", package = "measlesCFR", mustWork = TRUE)
  assign('baseline_covariates', baseline_covariates, envir = parent.env(environment()))

  no_vax_covariates = system.file("extdata", "transformed_standardized_covaraite_set_with_incidence_no_vax_revised_10march2023.csv", package = "measlesCFR", mustWork = TRUE)
  assign('no_vax_covariates', no_vax_covariates, envir = parent.env(environment()))

  no_vax_projection_covariates = system.file("extdata", "transformed_standardized_covaraite_set_with_incidence_no_vax_PROJECTION_revised_10march2023.csv", package = "measlesCFR", mustWork = TRUE)
  assign('no_vax_projection_covariates', no_vax_covariates, envir = parent.env(environment()))

  ## Load CSVs for createProjectionDF.R
  transformed_covariate_incidence_projections = system.file("extdata", "transformed_standardized_covaraite_set_with_incidence_PROJECTIONS.csv", package = "measlesCFR", mustWork = TRUE)
  assign('transformed_covariate_incidence_projections', transformed_covariate_incidence_projections, envir = parent.env(environment()))

  mcv_mean = system.file("extdata", "mcv_mean.csv", package = "measlesCFR", mustWork = TRUE)
  assign('mcv_mean', mcv_mean, envir = parent.env(environment()))

  mcv_sd = system.file("extdata", "mcv_sd.csv", package = "measlesCFR", mustWork = TRUE)
  assign('mcv_sd', mcv_sd, envir = parent.env(environment()))

  mcv_max = system.file("extdata", "mcv_max.csv", package = "measlesCFR", mustWork = TRUE)
  assign('mcv_max', mcv_max, envir = parent.env(environment()))

  mcv_min = system.file("extdata", "mcv_min.csv", package = "measlesCFR", mustWork = TRUE)
  assign('mcv_min', mcv_min, envir = parent.env(environment()))

  maternal_education_mean = system.file("extdata", "maternal_education_mean.csv", package = "measlesCFR", mustWork = TRUE)
  assign('maternal_education_mean', maternal_education_mean, envir = parent.env(environment()))

  maternal_education_sd = system.file("extdata", "maternal_education_sd.csv", package = "measlesCFR", mustWork = TRUE)
  assign('maternal_education_sd', maternal_education_sd, envir = parent.env(environment()))

  maternal_education_max = system.file("extdata", "maternal_education_max.csv", package = "measlesCFR", mustWork = TRUE)
  assign('maternal_education_max', maternal_education_max, envir = parent.env(environment()))

  maternal_education_min = system.file("extdata", "maternal_education_min.csv", package = "measlesCFR", mustWork = TRUE)
  assign('maternal_education_min', maternal_education_min, envir = parent.env(environment()))

  u5mr_mean = system.file("extdata", "u5mr_mean.csv", package = "measlesCFR", mustWork = TRUE)
  assign('u5mr_mean', u5mr_mean, envir = parent.env(environment()))

  u5mr_sd = system.file("extdata", "u5mr_sd.csv", package = "measlesCFR", mustWork = TRUE)
  assign('u5mr_sd', u5mr_sd, envir = parent.env(environment()))

  u5mr_max = system.file("extdata", "u5mr_max.csv", package = "measlesCFR", mustWork = TRUE)
  assign('u5mr_max', u5mr_max, envir = parent.env(environment()))

  u5mr_min = system.file("extdata", "u5mr_min.csv", package = "measlesCFR", mustWork = TRUE)
  assign('u5mr_min', u5mr_min, envir = parent.env(environment()))

  prop_urban_mean = system.file("extdata", "prop_urban_mean.csv", package = "measlesCFR", mustWork = TRUE)
  assign('prop_urban_mean', prop_urban_mean, envir = parent.env(environment()))

  prop_urban_sd = system.file("extdata", "prop_urban_sd.csv", package = "measlesCFR", mustWork = TRUE)
  assign('prop_urban_sd', prop_urban_sd, envir = parent.env(environment()))

  prop_urban_max = system.file("extdata", "prop_urban_max.csv", package = "measlesCFR", mustWork = TRUE)
  assign('prop_urban_max', prop_urban_max, envir = parent.env(environment()))

  prop_urban_min = system.file("extdata", "prop_urban_min.csv", package = "measlesCFR", mustWork = TRUE)
  assign('prop_urban_min', prop_urban_min, envir = parent.env(environment()))

  vitA_mean = system.file("extdata", "vitA_mean.csv", package = "measlesCFR", mustWork = TRUE)
  assign('vitA_mean', vitA_mean, envir = parent.env(environment()))

  vitA_sd = system.file("extdata", "vitA_sd.csv", package = "measlesCFR", mustWork = TRUE)
  assign('vitA_sd', vitA_sd, envir = parent.env(environment()))

  vitA_max = system.file("extdata", "vitA_max.csv", package = "measlesCFR", mustWork = TRUE)
  assign('vitA_max', vitA_max, envir = parent.env(environment()))

  vitA_min = system.file("extdata", "vitA_min.csv", package = "measlesCFR", mustWork = TRUE)
  assign('vitA_min', vitA_min, envir = parent.env(environment()))

  incidence_mean = system.file("extdata", "incidence_mean.csv", package = "measlesCFR", mustWork = TRUE)
  assign('incidence_mean', incidence_mean, envir = parent.env(environment()))

  incidence_sd = system.file("extdata", "incidence_sd.csv", package = "measlesCFR", mustWork = TRUE)
  assign('incidence_sd', incidence_sd, envir = parent.env(environment()))

  incidence_max = system.file("extdata", "incidence_max.csv", package = "measlesCFR", mustWork = TRUE)
  assign('incidence_max', incidence_max, envir = parent.env(environment()))

  incidence_min = system.file("extdata", "incidence_min.csv", package = "measlesCFR", mustWork = TRUE)
  assign('incidence_min', incidence_min, envir = parent.env(environment()))

  square_covariate_set = system.file("extdata", "square_covariate_set.csv", package = "measlesCFR", mustWork = TRUE)
  assign('square_covariate_set', square_covariate_set, envir = parent.env(environment()))
}
