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

    inputDF_original <- subset(inputDF_original, select=c('year', 'vitA_deficiency', 'maternal_education', 'u5mr', 'mcv1', 'prop_urban'))
    inputDF_original$vitA_deficiency_origin <- 'Default'
    inputDF_original$maternal_education_origin <- 'Default'
    inputDF_original$u5mr_origin <- 'Default'
    inputDF_original$mcv1_origin <- 'Default'
    inputDF_original$prop_urban_origin <- 'Default'

    fake_incidence <- seq(0.000001, 0.1, length=30)


    inputDF_original$incidence <- fake_incidence
    inputDF_original$incidence_origin <- 'User-specified'
  }
  else {
    inputDF_original <- inputDF
  }

  # -----------------------------------------------------------------------------------------------
  # make diagnostic plots -------------------------------------------------------------------------
  ### everything above this should already be made in other functions / defaults

  country <- country

  d <- subset(inputDF_original, select=c('year', 'vitA_deficiency', 'maternal_education', 'u5mr', 'mcv1', 'prop_urban', 'incidence',
                                         'vitA_deficiency_origin', 'maternal_education_origin', 'u5mr_origin', 'mcv1_origin', 'prop_urban_origin', 'incidence_origin'))

  d <- melt(d, id = c('year','vitA_deficiency_origin', 'maternal_education_origin', 'u5mr_origin', 'mcv1_origin', 'prop_urban_origin', 'incidence_origin'))

  d$value_origin <- ifelse(d$variable == "vitA_deficiency", d$vitA_deficiency_origin,
                           ifelse(d$variable == "maternal_education", d$maternal_education_origin,
                                  ifelse(d$variable == "mcv1", d$mcv1_origin,
                                         ifelse(d$variable == "u5mr", d$u5mr_origin,
                                                ifelse(d$variable == "prop_urban", d$prop_urban_origin,
                                                       ifelse(d$variable == "incidence", d$incidence_origin,NA))))))

  ## set max limits for lines
  limits_df <- fread(measlesCFR::square_covariate_set)

  limits_df <- subset(limits_df, ihme_loc_id == country & year_id %in% unique(inputDF_original$year))
  limits_df$year <- limits_df$year_id

  limits_df <- subset(limits_df, select=c('year', 'vitA_deficiency', 'maternal_education', 'u5mr', 'mcv1', 'prop_urban'))

  max_limits <- data.frame(variable = c("vitA_deficiency", "maternal_education", "mcv1", "u5mr", "prop_urban", "incidence"),
                           Z = c(max(limits_df$vitA_deficiency), max(limits_df$maternal_education), max(limits_df$mcv1), max(limits_df$u5mr), max(limits_df$prop_urban), 0.05))

  max_limits$origin <- ifelse(max_limits$variable == "vitA_deficiency", unique(d$vitA_deficiency_origin),
                              ifelse(max_limits$variable == "maternal_education", unique(d$maternal_education_origin),
                                     ifelse(max_limits$variable == "mcv1", unique(d$mcv1_origin),
                                            ifelse(max_limits$variable == "u5mr", unique(d$u5mr_origin),
                                                   ifelse(max_limits$variable == "prop_urban", unique(d$prop_urban_origin),
                                                          ifelse(max_limits$variable == "incidence", unique(d$incidence_origin), NA))))))

  max_limits <- subset(max_limits, origin == 'User-specified')

  ## set min limits for lines
  min_limits <- data.frame(variable = c("vitA_deficiency", "maternal_education", "mcv1", "u5mr", "prop_urban", "incidence"),
                           Z = c(min(limits_df$vitA_deficiency), min(limits_df$maternal_education), min(limits_df$mcv1), min(limits_df$u5mr), min(limits_df$prop_urban), 0.0005))

  min_limits$origin <- ifelse(min_limits$variable == "vitA_deficiency", unique(d$vitA_deficiency_origin),
                              ifelse(min_limits$variable == "maternal_education", unique(d$maternal_education_origin),
                                     ifelse(min_limits$variable == "mcv1", unique(d$mcv1_origin),
                                            ifelse(min_limits$variable == "u5mr", unique(d$u5mr_origin),
                                                   ifelse(min_limits$variable == "prop_urban", unique(d$prop_urban_origin),
                                                          ifelse(min_limits$variable == "incidence", unique(d$incidence_origin), NA))))))

  min_limits <- subset(min_limits, origin == 'User-specified')

  gg_ctry <- ggplot(d, aes(x = year, y = value)) + geom_line(color='grey28') + theme_bw() +
    geom_point(aes(color=as.factor(value_origin))) + xlab("Year") + ylab("Value") +
    geom_hline(data = max_limits, aes(yintercept = Z)) +
    geom_hline(data = min_limits, aes(yintercept = Z)) + ggtitle(country) +
    facet_wrap(~ variable, scales = 'free') + scale_color_manual(values=c("Default" = "#1B9E77",
                                                                          "User-specified" = "#D95F02")) +
    guides(color=guide_legend(title=""))


  return(gg_ctry)
}
