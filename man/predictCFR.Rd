% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predictCFR.R
\name{predictCFR}
\alias{predictCFR}
\title{Predict measles case fatality ratios.}
\usage{
predictCFR(
  country,
  inputDF = NULL,
  vaccination_scenario = "baseline",
  start_age = 0,
  end_age = 34,
  start_year = 1990,
  end_year = 2019,
  community_indicator = 1,
  get_draws = F,
  upper_bound = 0.975,
  lower_bound = 0.025
)
}
\arguments{
\item{country}{The three-digit ISO3 code for the country in which to
estimate measles CFRs. See Details for how to set this up as a loop
for multiple countries.}

\item{inputDF}{Data frame of input covariates from the \code{createInputDF()}
function that is long by calendar year and single year of age.}

\item{vaccination_scenario}{An indicator to specify whether to estimate
CFRs given vaccination (i.e. baseline) or no-vaccination scenario (options:
“baseline”, “no_vaccination”). Default set to “baseline”.}

\item{start_age}{The youngest single year of age at which to estimate
measles CFR. An integer value that is bounded between 0 and 100.
\code{start_age} cannot be greater than \code{end_age}.}

\item{end_age}{The oldest single year of age at which to estimate measles
CFR. An integer value that is bounded between 0 and 100. \code{end_age} cannot
be less than ```start_age``.}

\item{start_year}{The start year to begin measles CFR estimation. An
integer class value that is bounded between 1980 and 2100. \code{start_year}
cannot be greater than \code{end_year}.}

\item{end_year}{The end year through which to estimate measles CFR. An
integer class value that is bounded between 1980 and 2100.
\code{end_year} cannot be less than \code{start_year}.}

\item{community_indicator}{An indicator to specify whether to estimate
CFRs given a community or hospital setting (options: 1 for community,
0 for hospital). Default set to 1.}

\item{get_draws}{Boolean value (TRUE/FALSE). If set to TRUE, the function
will output the 1000 draws used to estimate measles CFR uncertainty. If
set to FALSE, the function will output the mean, upper bound, and lower
bound measles CFR for the specified ages and years. Default set to FALSE.}

\item{upper_bound}{The upper bound percentile to return for the measles
CFR uncertainty interval. A numeric value bounded between 0 and 1.
\code{upper_bound} cannot be less than \code{lower_bound}. Default set to
0.975.}

\item{lower_bound}{The lower bound percentile to return for the measles
CFR uncertainty interval. A numeric value bounded between 0 and 1.
\code{lower_bound} cannot be greater than \code{upper_bound}. Default set to
0.025.}
}
\value{
A data frame of numeric class that is long by year and single
year of age.
}
\description{
For specified years, age, and covariates, the function will
provide estimated measles case fatality ratios (CFRs) assuming vaccination
or no-vaccination, in community or hospital settings, with specified
uncertainty bounds, with or without sampled draws.
}
\details{
The function returns estimated measles CFRs over time
(by calendar year) from start_year to end_year according to the supplied
input data frame created by \code{createInputDF()}. The start_age must
be defined as an integer value between 0 and 100. The end_age must be
defined as an integer value between 0 and 100. If vaccination_scenario
is set to “vaccination” (default), the estimated measles CFRs include
the measles-containing-vaccine-first-dose (MCV1) coverage covariate,
which approximates vaccine coverage at the country level. If
vaccination_scenario is set to “no-vaccination,” the estimated measles
CFRs assume the MCV1 coverage covariate is set to 0\% for all countries,
which approximates a counterfactual in which no vaccine program has been
implemented. If community_indicator is set to “community” (default), the
estimated measles CFRs are returned for the community-based setting (i.e.,
CFRs among cases that do not require hospitalization). If
community_indicator is set to “hospital,” the estimated measles CFRs are
returned for the hospital-based setting (i.e., CFRs among cases that
require hospitalization).

If get_draws is set to “TRUE,” the function will output the 1000 draws
used to estimate measles CFR uncertainty. If get_draws is set to “FALSE”
(default), the function will output the mean, upper bound, and lower bound
measles CFR for the specified ages and years. The upper_bound percentile
must be supplied as a numeric value bounded between 0 and 1 (default set
to 0.975). The lower bound percentile must be supplied as a numeric value
bounded between 0 and 1 (default set to 0.025).

The function will return estimated measles CFRs for a single country,
defined by the supplied three-digit ISO3 code for the country in which to
estimate measles CFRs. In order to set up a loop to return estimates across
multiple countries, you must first save a vector of ISO3 codes for your
countries of interest as a .CSV file, see Table A.\tabular{l}{
   \strong{Table A: ISO3 codes for running a country loop for selected Southeast Asian countries.} \cr
   "BGD" \cr
   "BTN" \cr
   "IDN" \cr
   "IND" \cr
   "LKA" \cr
   "MDV" \cr
   "MMR" \cr
   "NPL" \cr
   "THA" \cr
   "TLS" \cr
}


From your working directory, load your data frame as a .CSV file into your
R environment:

\code{countries <- read.csv("LMIC_codes.csv",header=TRUE)}

Run the measlesCFR package as a loop per the following code, revising your
inputs and arguments accordingly. This example loops through the above
countries for children ages 0 to 4 from years 2000 to 2030, assuming
community-based cases given a vaccination program, returning only the mean,
upper bound (0.975 percentile), and lower bound (0.025 percentile) estimates.

\if{html}{\out{<div class="sourceCode">}}\preformatted{directory <- "C:\\" ## your file path
setwd(directory)
covariates <- inputDF() ## your inputDF object

for (i0 in 1:length(countries)\{
     i <- countries[i0]
     output <- predictCFR(country = i, inputDF = covariates,
                          vaccination_scenario = “baseline”,
                          start_age = 0, end_age = 4, start_year = 2000,
                          end_year = 2030, community_indicator=1,
                          get_draws = FALSE, upper_bound = 0.975,
                          lower_bound = 0.025)
     write.csv(output,paste0(directory,"output_",i,".csv"), row.names = FALSE)
\}
}\if{html}{\out{</div>}}
}
\examples{
# Predict CFRs with default covariates
eth_cfr_default <- predictCFR(country="ETH")
# Predict CFRs in the no-vaccination scenario with default covariates
eth_cfr_default_novax <- predictCFR(country="ETH", vaccination_scenario = "no_vaccination")
# Predict CFRs in a hospital setting with default covariates
eth_cfr_default <- predictCFR(country="ETH", community_indicator=0)
# Predict CFRs for years 2000 to 2030 with default covariates
eth_cfr_default <- predictCFR(country="ETH", start_year=2000, end_year=2030)
# Predict CFRs with user-specified covariates
eth_cfr <- predictCFR(country="ETH", inputDF=eth_inputDF)
# Predict CFRs with user-specified covariates for ages 0 to 14
eth_cfr <- predictCFR(country="ETH", inputDF=eth_inputDF, start_age=0, end_age=14)
# Predict CFRs with user-specified covariates including all parameter set draws
eth_cfr <- predictCFR(country="ETH", inputDF=eth_inputDF, get_draws=TRUE)

}
