#' Predict measles case fatality ratios.
#'
#' @description For specified years, age, and covariates, the function will
#' provide estimated measles case fatality ratios (CFRs) assuming vaccination
#' or no-vaccination, in community or hospital settings, with specified
#' uncertainty bounds, with or without sampled draws.
#'
#' @param country The three-digit ISO3 code for the country in which to
#' estimate measles CFRs. See Details for how to set this up as a loop
#' for multiple countries.
#' @param inputDF Data frame of input covariates from the ```createInputDF()```
#'  function that is long by calendar year and single year of age.
#' @param vaccination_scenario An indicator to specify whether to estimate
#' CFRs given vaccination (i.e. baseline) or no-vaccination scenario (options:
#' “baseline”, “no_vaccination”). Default set to “baseline”.
#' @param start_age The youngest single year of age at which to estimate
#' measles CFR. An integer value that is bounded between 0 and 100.
#' ```start_age``` cannot be greater than ```end_age```.
#' @param end_age The oldest single year of age at which to estimate measles
#' CFR. An integer value that is bounded between 0 and 100. ```end_age``` cannot
#' be less than ```start_age``.
#' @param year_start The start year to begin measles CFR estimation. An
#' integer class value that is bounded between 1980 and 2100. ```year_start```
#' cannot be greater than ```year_end```.
#' @param year_end The end year through which to estimate measles CFR. An
#' integer class value that is bounded between 1980 and 2100.
#' ```year_end``` cannot be less than ```year_start```.
#' @param community_indicator An indicator to specify whether to estimate
#' CFRs given a community or hospital setting (options: 1 for community,
#' 0 for hospital). Default set to 1.
#' @param get_draws Boolean value (TRUE/FALSE). If set to TRUE, the function
#' will output the 1000 draws used to estimate measles CFR uncertainty. If
#' set to FALSE, the function will output the mean, upper bound, and lower
#' bound measles CFR for the specified ages and years. Default set to FALSE.
#' @param upper_bound The upper bound percentile to return for the measles
#' CFR uncertainty interval. A numeric value bounded between 0 and 1.
#' ```upper_bound``` cannot be less than ```lower_bound```. Default set to
#' 0.975.
#' @param lower_bound The lower bound percentile to return for the measles
#' CFR uncertainty interval. A numeric value bounded between 0 and 1.
#' ```lower_bound``` cannot be greater than ```upper_bound```. Default set to
#' 0.025.
#'
#' @examples
#' # Predict CFRs with default covariates
#' eth_cfr_default <- predictCFR(country="ETH")
#' # Predict CFRs in the no-vaccination scenario with default covariates
#' eth_cfr_default_novax <- predictCFR(country="ETH", vaccination_scenario = "no_vaccination")
#' # Predict CFRs in a hospital setting with default covariates
#' eth_cfr_default <- predictCFR(country="ETH", community_indicator=0)
#' # Predict CFRs for years 2000 to 2030 with default covariates
#' eth_cfr_default <- predictCFR(country="ETH", year_start=2000, year_end=2030)
#' # Predict CFRs with user-specified covariates
#' eth_cfr <- predictCFR(country="ETH", inputDF=eth_inputDF)
#' # Predict CFRs with user-specified covariates for ages 0 to 14
#' eth_cfr <- predictCFR(country="ETH", inputDF=eth_inputDF, age_start=0, age_end=14)
#' # Predict CFRs with user-specified covariates including all parameter set draws
#' eth_cfr <- predictCFR(country="ETH", inputDF=eth_inputDF, get_draws=TRUE)
#'
#' @details The function returns estimated measles CFRs over time
#' (by calendar year) from start_year to end_year according to the supplied
#' input data frame created by ```createInputDF()```. The start_age must
#' be defined as an integer value between 0 and 100. The end_age must be
#' defined as an integer value between 0 and 100. If vaccination_scenario
#' is set to “vaccination” (default), the estimated measles CFRs include
#' the measles-containing-vaccine-first-dose (MCV1) coverage covariate,
#' which approximates vaccine coverage at the country level. If
#' vaccination_scenario is set to “no-vaccination,” the estimated measles
#' CFRs assume the MCV1 coverage covariate is set to 0% for all countries,
#' which approximates a counterfactual in which no vaccine program has been
#' implemented. If community_indicator is set to “community” (default), the
#' estimated measles CFRs are returned for the community-based setting (i.e.,
#' CFRs among cases that do not require hospitalization). If
#' community_indicator is set to “hospital,” the estimated measles CFRs are
#' returned for the hospital-based setting (i.e., CFRs among cases that
#' require hospitalization).
#'
#' If get_draws is set to “TRUE,” the function will output the 1000 draws
#' used to estimate measles CFR uncertainty. If get_draws is set to “FALSE”
#' (default), the function will output the mean, upper bound, and lower bound
#' measles CFR for the specified ages and years. The upper_bound percentile
#' must be supplied as a numeric value bounded between 0 and 1 (default set
#' to 0.975). The lower bound percentile must be supplied as a numeric value
#' bounded between 0 and 1 (default set to 0.025).
#'
#' The function will return estimated measles CFRs for a single country,
#' defined by the supplied three-digit ISO3 code for the country in which to
#' estimate measles CFRs. In order to set up a loop to return estimates across
#' multiple countries, you must first save a vector of ISO3 codes for your
#' countries of interest as a .CSV file, see Table A.
#'
#' **Table A: ISO3 codes for running a country loop for selected Southeast Asian countries.**
#' | ------------- |
#' | "BGD"         |
#' | "BTN"         |
#' | "IDN"         |
#' | "IND"         |
#' | "LKA"         |
#' | "MDV"         |
#' | "MMR"         |
#' | "NPL"         |
#' | "THA"         |
#' | "TLS"         |
#'
#'
#' From your working directory, load your data frame as a .CSV file into your
#' R environment:
#'
#' ```countries <- read.csv("LMIC_codes.csv",header=TRUE)```
#'
#' Run the measlesCFR package as a loop per the following code, revising your
#' inputs and arguments accordingly. This example loops through the above
#' countries for children ages 0 to 4 from years 2000 to 2030, assuming
#' community-based cases given a vaccination program, returning only the mean,
#' upper bound (0.975 percentile), and lower bound (0.025 percentile) estimates.
#'
#' ```
#' directory <- "C:\" ## your file path
#' setwd(directory)
#' covariates <- inputDF() ## your inputDF object
#'
#' for (i0 in 1:length(countries){
#'      i <- countries[i0]
#'      output <- predictCFR(country = i, inputDF = covariates,
#'                           vaccination_scenario = “baseline”,
#'                           start_age = 0, end_age = 4, start_year = 2000,
#'                           end_year = 2030, community_indicator=1,
#'                           get_draws = FALSE, upper_bound = 0.975,
#'                           lower_bound = 0.025)
#'      write.csv(output,paste0(directory,"output_",i,".csv"), row.names = FALSE)
#'}
#' ```
#'
#' @md
#'
#' @return A data frame of numeric class that is long by year and single
#' year of age.
#'
#' @import data.table
#' @import tidyverse
#' @import msm
#' @import arm
#' @import reticulate
#' @import stats
#' @export


predictCFR <- function(country, inputDF=NULL, vaccination_scenario = 'baseline', start_age = 0,
                       end_age = 34, start_year = 1990, end_year = 2019, community_indicator = 1,
                       get_draws = F, upper_bound = 0.975, lower_bound = 0.025) {

  ## ----------------------------------------------------------------------------------------------
  ## content for predictCFR()
  ## ----------------------------------------------------------------------------------------------

  setDTthreads(1)
  country_iso3 <- country
  reticulate::use_condaenv("mrtool-0.1.0")
  # -----------------------------------------------------------------------------------------------
  # read covariates for prediction frame ----------------------------------------------------------
  if (missing(inputDF)) {
    if(vaccination_scenario == "baseline"){
      covariates <- fread(measlesCFR:::baseline_covariates)
    }else if(vaccination_scenario == "no_vaccination"){
      covariates <- fread(measlesCFR:::no_vax_covariates)
    }
  }
  else {
    covariates <- inputDF
  }

  pred_frame <- subset(covariates, country == country_iso3 & year >= start_year & year <= end_year)

  # -----------------------------------------------------------------------------------------------
  # prepare prediction frame ----------------------------------------------------------------------
  pred_frame$Comm.ind <- community_indicator

  pred_frame2 <- cbind(pred_frame, i = rep(start_age:end_age, each = nrow(pred_frame)))
  pred_frame2$start_age <- pred_frame2$i
  pred_frame2$end_age <- pred_frame2$i

  # -----------------------------------------------------------------------------------------------
  # make MR-BRT prediction object -----------------------------------------------------------------
  data_pred <- mrtool$MRData()
  data_pred$load_df(data=pred_frame2,
                    col_covs=list( "incidence_standardized",
                                   "maternal_education_standardized",
                                   "start_age",
                                   "end_age",
                                   "Comm.ind",
                                   "mcv1_standardized",
                                   "u5mr_standardized",
                                   "prop_urban_standardized",
                                   "vitA_standardized"))

  # -----------------------------------------------------------------------------------------------
  # load MR-BRT model object and samples ----------------------------------------------------------
  data("mrbrt_samples")
  ##load_mrbrt_samples <- measlesCFR::mrbrt_samples
  pickle_filepath = system.file(package = "measlesCFR", "extdata", "mrbrt_mod_cfr_object.pkl")
  mod_cfr <- py_load_object(filename = pickle_filepath, pickle = "dill")

  # -----------------------------------------------------------------------------------------------
  # get point predictions from fixed effects ------------------------------------------------------
  data_pred$predictions <- mod_cfr$predict(data=data_pred, predict_for_study = F)
  pred_frame2$mod_cfr <- invlogit(data_pred$predictions)

  # -----------------------------------------------------------------------------------------------
  # create draws and uncertainty ------------------------------------------------------------------
  draws <- mod_cfr$create_draws(
    data = data_pred,
    beta_samples = samples1[[1]],
    gamma_samples = samples1[[2]],
    random_study = FALSE
  )

  draws2 <- invlogit(draws)
  draws2 <- data.table(draws2)

  pred_frame2 <- as.data.table(pred_frame2)
  pred_frame2[, `:=` (
    lower = apply(draws2, 1, function(x) quantile(x, lower_bound)),
    median = apply(draws2, 1, function(x) quantile(x, 0.5)),
    upper = apply(draws2, 1, function(x) quantile(x, upper_bound))
  )]

  draws2$country <- pred_frame2$country
  draws2$year <- pred_frame2$year
  draws2$age <- pred_frame2$start_age
  draws2$Comm.ind <- pred_frame2$Comm.ind
  draws2$predicted_cfr <- pred_frame2$mod_cfr
  draws2$predicted_cfr_ub <- pred_frame2$upper
  draws2$predicted_cfr_lb <- pred_frame2$lower
  draws2$care_setting <- ifelse(pred_frame2$Comm.ind == 1, "community", "hospital")

  pred_frame2$predicted_cfr <- pred_frame2$mod_cfr
  pred_frame2$predicted_cfr_ub <- pred_frame2$upper
  pred_frame2$predicted_cfr_lb <- pred_frame2$lower

  pred_frame2$care_setting <- ifelse(pred_frame2$Comm.ind == 1, "community", "hospital")
  pred_frame2$age <- pred_frame2$start_age

  # -----------------------------------------------------------------------------------------------
  # to be returned --------------------------------------------------------------------------
  pred_frame2 <- subset(pred_frame2, select=c('country','age','year','care_setting','predicted_cfr', 'predicted_cfr_lb', 'predicted_cfr_ub'))

  draw_cols <- names(draws2)[grepl("V[0-9]*", names(draws2))]
  draws2 <- subset(draws2, select=c('country','age','year','care_setting','predicted_cfr', 'predicted_cfr_lb', 'predicted_cfr_ub', draw_cols))

  if (get_draws == T) {
    if(nrow(draws2) == 0) {
      return("List returned is empty. You are possibly using an ISO3 country that is not low- or middle-income (you can see list of accepted countries with `listOfCountries()`.")
    }
    else {
      return(draws2)
    }
  } else {
    if(nrow(pred_frame2) == 0) {
      return("List returned is empty. You are possibly using an ISO3 country that is not low- or middle-income (you can see list of accepted countries with `listOfCountries()`.")
    }
    else {
      return(pred_frame2)
    }
  }
}
