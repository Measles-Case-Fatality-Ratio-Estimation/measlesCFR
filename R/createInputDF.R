#' Create a combined input data frame.
#'
#' @description If historic and/or projection data frames are created by the
#' user, the function will combine user-specified values with default values
#' for the specified time period. If no user-specified data frames are created,
#' the function will rely on default values for the specified time period.
#'
#' @param country The three-digit ISO3 code for the country in which to
#' estimate measles CFRs. See Details for how to set this up as a loop for
#' multiple countries.
#' @param historic_include Boolean value (TRUE/FALSE). If set to TRUE,
#' will include default historic  Default set to TRUE.
#' @param projection_include Boolean value (TRUE/FALSE). Default set to TRUE.
#' @param historicDF Optional parameter. Output data frame created by
#' ```createHistoricDF()```.
#' @param projectionDF Optional parameter. Output data frame created by
#' ```createProjectionDF()```.
#' @param vaccination_scenario An indicator to specify whether to estimate
#' CFRs given vaccination (i.e. baseline) or no-vaccination scenario
#' (options: baseline, no_vaccination). Default set to baseline.
#' @param start_year The start year to begin measles CFR estimation. An
#' integer class value that is bounded between 1980 and 2100. start_year
#' cannot be greater than end_year. Default set to 1990.
#' @param end_year The end year through which to estimate measles CFR. An
#' integer class value that is bounded between 1980 and 2100. end_year
#' cannot be less than start_year.
#'
#' @details The function returns the default historic (if historic_include is
#' set to TRUE) and/or projection (if projection_include is set to TRUE) data
#' frames as a combined data frame. If the user supplies an historic data
#' frame created by ```createHistoricDF()``` and/or projection data frame
#' created by ```createProjectionDF()```, the function returns the combined
#' data frame of the user-specified data frame objects.
#'
#' @return A data frame of numeric class for the user-specified start to end
#' years. The data frame is long by year and wide by covariates.
#'
#' @import data.table
#'
#' @export
createInputDF <- function(country, historic_include=T, projection_include=F,
                          historicDF=NULL, projectionDF=NULL,
                          vaccination_scenario="baseline", start_year=1990,
                          end_year=NULL) {
    country_iso3 = country

    if(historic_include == T & missing(historicDF)){
      if(vaccination_scenario == "baseline"){
        historicDF <- fread(measlesCFR::baseline_covariates)
      }else if(vaccination_scenario == "no_vaccination"){
        historicDF <- fread(measlesCFR::no_vax_covariates)
      }
      historicDF <- subset(historicDF, country == country_iso3)
    }
    if(projection_include == T & missing(projectionDF)){
      if(vaccination_scenario == "baseline"){
        projectionDF <- fread(measlesCFR::transformed_covariate_incidence_projections)
      }else if(vaccination_scenario == "no_vaccination"){
        projectionDF <- fread(measlesCFR::no_vax_projection_covariates)
      }
      projectionDF <- subset(projectionDF, country == country_iso3)
    }

    inputDF <- data.frame()
    if(historic_include == T){
      inputDF <- rbind(inputDF, historicDF)
    }
    if(projection_include == T){
      inputDF <- rbind(inputDF, projectionDF)
    }
    if(!is.null(start_year)){
      inputDF <- subset(inputDF, year>=start_year)
    }
    if(!is.null(end_year)){
      inputDF <- subset(inputDF, year<=end_year)
    }

  return(inputDF)
}
