#' Display plots of measles case fatality rate by age.
#'
#' @description In a multi-panel plot, each single year of age estimated with
#' predictDF is visually displayed by calendar year.
#'
#' @param predict_df Data frame object created by ```predictCFR()``` function.
#' @param list_of_years A list of years obtained with unique values from
#' ```predictCFR()``` function, or a list specified by the user in order to
#' display a subset of years.
#'
#' @details The function returns a multi-panel plot in which each year of age
#' from start_age to end_age included in the ```predictCFR()``` object is
#' graphed over calendar year. If no year_list is supplied, the function will
#' return plots with start_year and end_year defined according to the
#' ```predictCFR()``` object; otherwise, a list of years can be passed to the
#' function in order to display the relevant calendar years. If
#' plot_uncertainty is set to “TRUE” (default), the multi-panel plots will
#' incorporate uncertainty intervals based on the defined upper_bound and
#' lower_bound percentiles in the ```predictCFR()``` object. If plot_uncertainty
#' is set to “FALSE,” the multi-panel plots will only include estimated mean
#' CFRs over time.
#'
#' @return A multi-panel plot of estimated measles CFR by single year of age.
#' The plots are graphed over time (i.e., by calendar year) for the specified
#' time period.
#'
#' @import data.table
#' @import ggplot2
#'
#' @export
#'
displayAgeCFR <- function(predict_df, list_of_years = c(2000, 2010, 2019)) {
  by_age <- ggplot(subset(predict_df, year %in% list_of_years)) +
    geom_line(aes(x=age, y=predicted_cfr), color='#870E75',) +
    geom_ribbon(data=subset(predict_df, year %in% list_of_years), aes(x=age,ymin=predicted_cfr_lb, ymax=predicted_cfr_ub), fill='#870E75', alpha=0.3) +
    theme_bw() + facet_wrap(~year) + xlab("Age") + ylab("CFR")

  return(by_age)
}
