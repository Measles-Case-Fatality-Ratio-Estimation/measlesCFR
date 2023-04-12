#' Display plots of measles case fatality rate over time (i.e., by calendar
#' year).
#'
#' @description In a multi-panel plot, each calendar estimated with predictDF
#' is visually displayed by single year of age.
#'
#' @param predict_df Data frame object created by ```predictCFR()``` function.
#' @param list_of_ages A list of single-year ages obtained with unique values
#' from ```predictCFR()``` function, or a list specified by the user in order
#' to display a subset of ages
#'
#' @details The function returns a multi-panel plot in which each calendar
#' year from start_year to end_year included in the ```predictCFR()``` object
#' is graphed for single-year age groups. If no age_list is supplied, the
#' function will return plots with start_age and end_age defined according
#' to the ```predictCFR()``` object; otherwise, a list of single-year ages
#' can be passed to the function in order to display the relevant age groups.
#' If plot_uncertainty is set to “TRUE” (default), the multi-panel plots will
#' incorporate uncertainty intervals based on the defined upper_bound and
#' lower_bound percentiles in the ```predictCFR()``` object. If plot_uncertainty
#' is set to “FALSE,” the multi-panel plots will only include estimated mean
#' CFRs by age.
#'
#' @return A multi-panel plot of estimated measles CFR by calendar year. The
#' plots are graphed by single year of age for the specified ages.
#'
#' @import data.table
#' @import ggplot2
#'
#' @export
#'
displayTimeCFR <- function(predict_df, list_of_ages = c(0,1,2,3,4)) {
  age = NULL
  predicted_cfr = NULL
  predicted_cfr_lb = NULL
  predicted_cfr_ub = NULL
  by_time <- ggplot(subset(predict_df, age %in% list_of_ages),aes(x=year,y=predicted_cfr)) +
    geom_line(aes(x=year, y=predicted_cfr), color='#3E71A8') +
    geom_ribbon(data=subset(predict_df, age %in% list_of_ages), aes(x=year,ymin=predicted_cfr_lb, ymax=predicted_cfr_ub), fill='#3E71A8', alpha=0.3) +
    theme_bw() + facet_wrap(~age) + xlab("Year") + ylab("CFR")

  return(by_time)
}
