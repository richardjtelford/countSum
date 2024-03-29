% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/est2_n.R
\name{estimate_n}
\alias{estimate_n}
\title{Estimate count sum}
\usage{
estimate_n(
  x,
  percent_col = "percent",
  taxon_col = "taxon",
  ID_cols,
  digits = 2,
  nmin = 1L,
  nmax = 1000L
)
}
\arguments{
\item{x}{data.frame of percent data in long format}

\item{percent_col}{character giving name of column with percent data}

\item{taxon_col}{character giving name of column with taxon names}

\item{ID_cols}{character vector giving name of one or more columns with sample IDs}

\item{digits}{numeric; how many decimal places are the percent reported to}

\item{nmin}{integer; possible count sum to start direct search at}

\item{nmax}{integer; possible count sum to end direct search at}
}
\description{
Estimate count sum from percent data
}
\details{
Estimates the count sum with two methods.
The first estimate is 100/p_min where p_min is the minimum percent.
The second estimate is the value N within the range nmin:nmax for which all p/100 * N is an integer for all percent p. The proportion of taxa for which this is an integer is given by the score. A warning is given if the score is below 1. This can happen nmax is less than the count sum, or if the percent have been incorrectly calculated (perhaps an error in rounding) or percent are calculated from different count sums for different taxa (eg. upland trees shrubs and herbs vs aquatics).

@return A nested tibble
#' \itemize{
\item ID_cols - one or more user specified columns identifying the assemblages
\item data - nested raw data from each assemblage
\item direct_search - nested raw output of the direct_search method
\item direct_search_est - nested with the value(s) of \code{est_n_direct} with the highest \code{score}. If the score is <1 a warning is given. This can contain more than row, especially if the precision of the percent data is low and the estimated count sum is high.
\item minpc - the minimum percentage for each assemblage
\item est_n_minpc estimate count sum for the minimum percent method
\item est_min_minpc - lowest possible count sum given precision of the percentage data
\item est_max_minpc - ditto for highest possible count sum
\item n_taxa - number of taxa
}
}
\examples{
require("dplyr")
data(last_chance)
last_chance0  \%>\% 
  tidyr::pivot_longer(cols = -c(age_calBP, totcaps), names_to = "taxon", values_to = "percent") \%>\% 
  estimate_n(digits = 2, ID_cols = c("age_calBP", "totcaps"))

}
\references{
Telford (2019) Tools for identifying unexpectedly low microfossil count sums. Preprint.
}
