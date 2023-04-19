#' Visually display covariates in individual plots.
#'
#' @description In a multi-panel plot object, each set of covariate values
#' is visually displayed over time (i.e., by calendar year) from the start
#' year to end year specified. Covariate values can be user-specified through
#' the use of the ```createHistoricDF()```, ```createProjectionDF()```, and
#' ```createInputDF()``` functions, or can rely on default covariate values.
#'
#' @param country The three-digit ISO3 code for the country in which to
#' estimate measles CFRs. See Details for how to set this up as a loop for
#' multiple countries.
#' @param inputDF Optional parameter. If supplied, the function will display
#' the combination of user-specified and default covariate values by year. Each
#' covariate will be displayed in its own plot (i.e., six plots). If no inputDF
#' is supplied, the function will rely on default covariate values.
#'
#' @details The function returns the default covariate values over time
#' (in calendar years) as a multi-panel plot object. If the user supplies an
#' input data frame created by ```createInputDF()```, the function returns the
#' user-specified covariate values over time (in calendar years) as a
#' multi-panel plot object.
#'
#' @return A multi-panel plot of each set of covariate values. The plots are
#' graphed over time (i.e., by calendar year).
#' @import data.table
#' @import ggplot2
#' @export
displayCovariates <- function(country, inputDF=NULL) {
  if (missing(inputDF)) {
    inputDF_original <- fread(measlesCFR::square_covariate_set)

    inputDF_original <- subset(inputDF_original, ihme_loc_id == country & year_id %in% c(1990:2019))
    inputDF_original$year <- inputDF_original$year_id

    incidence_df <- fread(measlesCFR::baseline_covariates)
    incidence_df <- subset(incidence_df, country == country & year %in% c(1990:2019))
    mean_incidence <- as.numeric(fread(measlesCFR::incidence_mean))
    sd_incidence <- as.numeric(fread(measlesCFR::incidence_sd))
    incidence_df$incidence <- exp((incidence_df$incidence_standardized * sd_incidence) + mean_incidence ) / (1 + exp((incidence_df$incidence_standardized * sd_incidence) + mean_incidence ))

    incidence_df <- subset(incidence_df, select=c("country", "year", "incidence"))

    inputDF_original <- merge(inputDF_original, incidence_df, by.x=c('ihme_loc_id', 'year_id'), by.y=c('country', 'year'))


  } else {
    inputDF_original <- inputDF

    mean_mcv1 <- as.numeric(fread(measlesCFR::mcv_mean))
    sd_mcv1 <- as.numeric(fread(measlesCFR::mcv_sd))
    inputDF_original$mcv1 <- (inputDF_original$mcv1_standardized * sd_mcv1) + mean_mcv1

    mean_maternal_education <- as.numeric(fread(measlesCFR::maternal_education_mean))
    sd_maternal_education <- as.numeric(fread(measlesCFR::maternal_education_sd))
    inputDF_original$maternal_education <- (inputDF_original$maternal_education_standardized * sd_maternal_education) + mean_maternal_education

    mean_incidence <- as.numeric(fread(measlesCFR::incidence_mean))
    sd_incidence <- as.numeric(fread(measlesCFR::incidence_sd))
    inputDF_original$incidence <- exp((inputDF_original$incidence_standardized * sd_incidence) + mean_incidence ) / (1 + exp((inputDF_original$incidence_standardized * sd_incidence) + mean_incidence ))

    mean_prop_urban <- as.numeric(fread(measlesCFR::prop_urban_mean))
    sd_prop_urban <- as.numeric(fread(measlesCFR::prop_urban_sd))
    inputDF_original$prop_urban <- (inputDF_original$prop_urban_standardized * sd_prop_urban) + mean_prop_urban

    mean_u5mr <- as.numeric(fread(measlesCFR::u5mr_mean))
    sd_u5mr <- as.numeric(fread(measlesCFR::u5mr_sd))
    inputDF_original$u5mr <- (inputDF_original$u5mr_standardized * sd_u5mr) + mean_u5mr

    mean_vitA <- as.numeric(fread(measlesCFR::vitA_mean))
    sd_vitA <- as.numeric(fread(measlesCFR::vitA_sd))
    inputDF_original$vitA_deficiency <- (inputDF_original$vitA_standardized * sd_vitA) + mean_vitA

  }

  # -----------------------------------------------------------------------------------------------
  # make diagnostic plots -------------------------------------------------------------------------
  ### everything above this should already be made in other functions / defaults

  country <- country

  d <- subset(inputDF_original, select=c('year', 'vitA_deficiency', 'maternal_education', 'u5mr', 'mcv1', 'prop_urban', 'incidence'))

  d <- melt(d, id = c('year'))

  ## set max limits for lines
  limits_df <- fread(measlesCFR::square_covariate_set)

  limits_df <- subset(limits_df, ihme_loc_id == country & year_id %in% c(1990:2019))
  limits_df$year <- limits_df$year_id

  incidence_df <- fread(measlesCFR::baseline_covariates)
  incidence_df <- subset(incidence_df, country == country & year %in% c(1990:2019))
  mean_incidence <- as.numeric(fread(measlesCFR::incidence_mean))
  sd_incidence <- as.numeric(fread(measlesCFR::incidence_sd))
  incidence_df$incidence <- exp((incidence_df$incidence_standardized * sd_incidence) + mean_incidence ) / (1 + exp((incidence_df$incidence_standardized * sd_incidence) + mean_incidence ))

  incidence_df <- subset(incidence_df, select=c("country", "year", "incidence"))

  limits_df <- merge(limits_df, incidence_df, by.x=c('ihme_loc_id', 'year_id'), by.y=c('country', 'year'))

  limits_df <- subset(limits_df, select=c('year', 'vitA_deficiency', 'maternal_education', 'u5mr', 'mcv1', 'prop_urban', 'incidence'))

  max_limits <- data.frame(variable = c("vitA_deficiency", "maternal_education", "mcv1", "u5mr", "prop_urban", "incidence"),
                           Z = c(max(limits_df$vitA_deficiency), max(limits_df$maternal_education), max(limits_df$mcv1), max(limits_df$u5mr), max(limits_df$prop_urban), max(limits_df$incidence, na.rm=T)))

  ## set min limits for lines
  min_limits <- data.frame(variable = c("vitA_deficiency", "maternal_education", "mcv1", "u5mr", "prop_urban", "incidence"),
                           Z = c(min(limits_df$vitA_deficiency), min(limits_df$maternal_education), min(limits_df$mcv1), min(limits_df$u5mr), min(limits_df$prop_urban), min(limits_df$incidence, na.rm=T)))

  gg_ctry <- ggplot(d, aes(x = year, y = value)) + geom_line(color='grey28') + theme_bw() +
    geom_point(color='dodgerblue4') + xlab("Year") + ylab("Value") +
    geom_hline(data = max_limits, aes(yintercept = Z)) +
    geom_hline(data = min_limits, aes(yintercept = Z)) + ggtitle(country) +
    facet_wrap(~ variable, scales = 'free') +
    guides(color=guide_legend(title=""))

  return(gg_ctry)
}
