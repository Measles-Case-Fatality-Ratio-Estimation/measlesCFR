#' Create a projection data frame.
#'
#' @description User provides inputs of measles incidence, measles-containing
#' vaccine first-dose (MCV1) coverage, or other covariates for the projection
#' period. The projection period can be specified for any end year through the
#' year 2100. The function will standardize the provided covariates, rely on
#' default values for any covariates that are not user-specified, and return a
#' data frame that is long by year and wide by covariates.
#'
#' @param country The three-digit ISO3 code for the country in which to
#' estimate measles CFRs. See Details for how to set this up as a loop for
#' multiple countries.
#' @param df_incidence Optional parameter. A country-specific numeric vector of
#' measles incidence that is long by year. Values must be non-negative.
#' @param df_mcv1 Optional parameter. A country-specific numeric vector of
#' MCV1 coverage that is long by year. Values are bounded between 0 and 1.
#' @param df_maternal_education Optional parameter. A country-specific numeric
#' vector of maternal educational attainment that is long by year. Values must
#' be bounded between 0 and 18 years.
#' @param df_u5mr Optional parameter. A country-specific numeric vector of
#' under-five mortality rate (per 1000) that is long by year. Values must be
#' non-negative.
#' @param df_prop_urban Optional parameter. A country-specific numeric vector
#' of proportion of population living in urban settings that is long by year.
#' Values must be between 0 and 1.
#' @param df_vitA Optional parameter. A country-specific numeric vector of
#' vitamin A deficiency prevalence that is long by year. Values must be
#' between 0 and 1.
#' @param bound_sample_range Boolean value (TRUE/FALSE). If set to FALSE
#' (set as default), the package will provide measles CFR estimates with an
#' accompanying warning. The sample bounds will be applied to any
#' visualizations that display covariates in other package functions.
#' If TRUE, the package will restrict user-specified covariate values to the
#' sample range that the model was fit to. Please see below (Details section)
#' for more information.
#'
#' @details First, this function transforms each covariate according to the
#' defined model fit in Sbarra, et al. (2023).
#'
#' Second, this function standardizes the transformed covariates by subtracting
#' the mean of the covariate and dividing by the standard deviation. Third, the
#' function returns a data frame that is long by year and wide by transformed,
#' standardized covariates.
#'
#' Each covariate is held constant at its 2019 estimated value across the
#' projected time period. Across all countries, the sample range of each
#' untransformed, unstandardized covariate value is as described below.
#'
#' | **Covariate**                       | **Minimum Value**   | **Maximum Value**  |
#' | ----------------------------------- | -------------------:| ------------------:|
#' | Measles incidence                   | 0.0000              | 0.0549             |
#' | Maternal education                  | 1.5164              | 13.9316            |
#' | MCV1 coverage                       | 0.37                | 0.99               |
#' | Under-5 mortality rate (per 1000)   | 3.1                 | 118.3              |
#' | Proportion living in urban setting  | 0.1325              | 0.9120             |
#' | Vitamin A deficiency prevalence     | 0.0042              | 0.2644             |
#'
#' @md
#'
#' @return A data frame of numeric class for the projection (i.e., future)
#' period (projectionDF). The data frame is long by year and wide by covariates.
#' @import data.table
#' @export
#'
createProjectionDF <- function(country, df_incidence=NULL, df_mcv1=NULL,
                               df_maternal_education=NULL, df_u5mr=NULL,
                               df_prop_urban=NULL, df_vitA=NULL,
                               bound_sample_range=F) {
  # all arguments optional - except country
  country_iso3 <- country

  projectionDF <- fread(measlesCFR::transformed_covariate_incidence_projections)
  projectionDF <- subset(projectionDF, country == country_iso3)

  # Measles-containing-vaccine first-dose (mcv1)
  if(!missing(df_mcv1)){

    projectionDF$mcv1_standardized <- NULL

    mean_mcv1 <- as.numeric(fread(measlesCFR::mcv_mean))
    sd_mcv1 <- as.numeric(fread(measlesCFR::mcv_sd))
    df_mcv1$mcv1_standardized <- (df_mcv1$mcv1 - mean_mcv1 ) / sd_mcv1

    if(bound_sample_range == T){
      max_mcv1 <- as.numeric(fread(measlesCFR::mcv_max))
      min_mcv1 <- as.numeric(fread(measlesCFR::mcv_min))

      for(y in 1:length(unique(df_mcv1$year))){
        if(df_mcv1$mcv1_standardized[y] > max_mcv1){
          message("Bound sample range -- replacing ", unique(df_mcv1$year)[y], " with maximum observed MCV1 coverage value.")
          df_mcv1$mcv1_standardized[y] <- max_mcv1
        }
        if(df_mcv1$mcv1_standardized[y] < min_mcv1){
          message("Bound sample range --  replacing ", unique(df_mcv1$year)[y], " with minimum observed MCV1 coverage value.")
          df_mcv1$mcv1_standardized[y] <- min_mcv1
        }
      }
    }else if(bound_sample_range == F){
      message("WARNING: you are possibly including MCV1 coverage values outside
              the range used to fit the modeling object. Proceed with caution,
              or use bound_sample_range = T.")
    }

    df_mcv1$mcv1 <- NULL
    projectionDF <- merge(projectionDF, df_mcv1, by='year', all.x=T)
  }

  if(!missing(df_maternal_education)){
    projectionDF$maternal_education_standardized <- NULL
    mean_maternal_education <- as.numeric(fread(measlesCFR::maternal_education_mean))
    sd_maternal_education <- as.numeric(fread(measlesCFR::maternal_education_sd))
    df_maternal_education$maternal_education_standardized <- (df_maternal_education$maternal_education - mean_maternal_education ) / sd_maternal_education
    if(bound_sample_range == T){
      max_maternal_education <- as.numeric(fread(measlesCFR::maternal_education_max))
      min_maternal_education <- as.numeric(fread(measlesCFR::maternal_education_min))

      for(y in 1:length(unique(df_maternal_education$year))){
        if(df_maternal_education$maternal_education_standardized[y] > max_maternal_education){
          message("Bound sample range -- replacing ", unique(df_maternal_education$year)[y], " with maximum observed maternal education value.")
          df_maternal_education$maternal_education_standardized[y] <- max_maternal_education
        }
        if(df_maternal_education$maternal_education_standardized[y] < min_maternal_education){
          message("Bound sample range --  replacing ", unique(df_maternal_education$year)[y], " with minimum observed maternal education value.")
          df_maternal_education$maternal_education_standardized[y] <- min_maternal_education
        }
      }
    }else if(bound_sample_range == F){
      message("WARNING: you are possibly including maternal education values
              outside the range used to fit the modeling object. Proceed with
              caution, or use bound_sample_range = T.")
    }

    df_maternal_education$maternal_education <- NULL
    projectionDF <- merge(projectionDF, df_maternal_education, by='year', all.x=T)
  }

  if(!missing(df_u5mr)){
    projectionDF$u5mr_standardized <- NULL
    mean_u5mr <- as.numeric(fread(measlesCFR::u5mr_mean))
    sd_u5mr <- as.numeric(fread(measlesCFR::u5mr_sd))

    df_u5mr$u5mr_standardized <- (df_u5mr$u5mr - mean_u5mr ) / sd_u5mr

    if(bound_sample_range == T){
      max_u5mr <- as.numeric(fread(measlesCFR::u5mr_max))
      min_u5mr <- as.numeric(fread(measlesCFR::u5mr_min))

      for(y in 1:length(unique(df_u5mr$year))){
        if(df_u5mr$u5mr_standardized[y] > max_u5mr){
          message("Bound sample range -- replacing ", unique(df_u5mr$year)[y], " with maximum observed U5MR value.")
          df_u5mr$u5mr_standardized[y] <- max_u5mr
        }
        if(df_u5mr$u5mr_standardized[y] < min_u5mr){
          message("Bound sample range --  replacing ", unique(df_u5mr$year)[y], " with minimum observed U5MR value.")
          df_u5mr$u5mr_standardized[y] <- min_u5mr
        }
      }
    }else if(bound_sample_range == F){
      message("WARNING: you are possibly including U5MR values outside the
              range used to fit the modeling object. Proceed with caution, or
              use bound_sample_range = T.")
    }

    df_u5mr$u5mr <- NULL
    projectionDF <- merge(projectionDF, df_u5mr, by='year', all.x=T)
  }

  if(!missing(df_prop_urban)){
    projectionDF$prop_urban_standardized <- NULL
    mean_prop_urban <- as.numeric(fread(measlesCFR::prop_urban_mean))
    sd_prop_urban <- as.numeric(fread(measlesCFR::prop_urban_sd))

    df_prop_urban$prop_urban_standardized <- (df_prop_urban$prop_urban - mean_prop_urban) / sd_prop_urban

    if(bound_sample_range == T){
      max_prop_urban <- as.numeric(fread(measlesCFR::prop_urban_max))
      min_prop_urban <- as.numeric(fread(measlesCFR::prop_urban_min))

      for(y in 1:length(unique(df_prop_urban$year))){
        if(df_prop_urban$prop_urban_standardized[y] > max_prop_urban){
          message("Bound sample range -- replacing ", unique(df_prop_urban$year)[y], " with maximum observed proportion urban value.")
          df_prop_urban$prop_urban_standardized[y] <- max_prop_urban
        }
        if(df_prop_urban$prop_urban_standardized[y] < min_prop_urban){
          message("Bound sample range --  replacing ", unique(df_prop_urban$year)[y], " with minimum observed proportion urban value.")
          df_prop_urban$prop_urban_standardized[y] <- min_prop_urban
        }
      }
    }else if(bound_sample_range == F){
      message("WARNING: you are possibly including proportion urban values
              outside the range used to fit the modeling object. Proceed with
              caution, or use bound_sample_range = T.")
    }

    df_prop_urban$prop_urban <- NULL
    projectionDF <- merge(projectionDF, df_prop_urban, by='year', all.x=T)
  }

  if(!missing(df_vitA)){
    projectionDF$vitA_standardized <- NULL
    mean_vitA <- as.numeric(fread(measlesCFR::vitA_mean))
    sd_vitA <- as.numeric(fread(measlesCFR::vitA_sd))
    df_vitA$vitA_standardized <- (df_vitA$vitA - mean_vitA) / sd_vitA

    if(bound_sample_range == T){
      max_vitA <- as.numeric(fread(measlesCFR::vitA_max))
      min_vitA <- as.numeric(fread(measlesCFR::vitA_min))

      for(y in 1:length(unique(df_vitA$year))){
        if(df_vitA$vitA_standardized[y] > max_vitA){
          message("Bound sample range -- replacing ", unique(df_vitA$year)[y], " with maximum observed vitamin A deficiency prevalence value.")
          df_vitA$vitA_standardized[y] <- max_vitA
        }
        if(df_vitA$vitA_standardized[y] < min_vitA){
          message("Bound sample range --  replacing ", unique(df_vitA$year)[y], " with minimum observed vitamin A deficiency prevalence value.")
          df_vitA$vitA_standardized[y] <- min_vitA
        }
      }
    }else if(bound_sample_range == F){
      message("WARNING: you are possibly including vitamin A deficiency
              prevalence values outside the range used to fit the modeling
              object. Proceed with caution, or use bound_sample_range = T.")
    }

    df_vitA$vitA <- NULL
    projectionDF <- merge(projectionDF, df_vitA, by='year', all.x=T)
  }

  if(!missing(df_incidence)){
    projectionDF$incidence_standardized <- NULL
    mean_incidence <- as.numeric(fread(measlesCFR::incidence_mean))
    sd_incidence <- as.numeric(fread(measlesCFR::incidence_sd))
    df_incidence$incidence_standardized <- (logit(df_incidence$incidence) - mean_incidence) / sd_incidence

    if(bound_sample_range == T){

      max_incidence <- as.numeric(fread(measlesCFR::incidence_max))
      min_incidence <- as.numeric(fread(measlesCFR::incidence_min))

      for(y in 1:length(unique(df_incidence$year))){
        if(df_incidence$incidence_standardized[y] > max_incidence){
          message("Bound sample range -- replacing ", unique(df_incidence$year)[y], " with maximum observed incidence value.")
          df_incidence$incidence_standardized[y] <- max_incidence
        }
        if(df_incidence$incidence_standardized[y] < min_incidence){
          message("Bound sample range --  replacing ", unique(df_incidence$year)[y], " with minimum observed incidence value.")
          df_incidence$incidence_standardized[y] <- min_incidence
        }
      }
    }else if(bound_sample_range == F){
      message("WARNING: you are possibly including incidence values outside
              the range used to fit the modeling object. Proceed with caution,
              or use bound_sample_range = T.")
    }

    df_incidence$incidence <- NULL
    projectionDF <- merge(projectionDF, df_incidence, by='year', all.x=T)
  }

  # ensure projection data frame only goes as far as user specified covariates
  projectionDF <- projectionDF[complete.cases(projectionDF),]
  return(projectionDF)
}
