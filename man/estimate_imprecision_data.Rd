% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estimate_imprecision_data.R
\name{estimate_imprecision_data}
\alias{estimate_imprecision_data}
\title{All calculations regarding imprecision}
\usage{
estimate_imprecision_data(
  data,
  B = 2000L,
  type = "percentile",
  level = 0.95,
  invalid_NA = FALSE
)
}
\arguments{
\item{data}{\code{list}, \code{data frame} or \code{data table} that is grouped by \code{"comparison"}. Furthermore, must contain the following columns: \code{comparison}, \code{SampleID}, \code{ReplicateID}, \code{MP_A} and \code{MP_B}.}

\item{B}{\code{Integer} - Number of bootstrap replicates used to estimate bootstrap confidence intervals. The default is 2000, which is the typical for bootstrap confidence intervals. Note that if you have more than five unique IVD-MDs in \code{data}, it may take approximately two seconds to run the resampling.}

\item{type}{\code{Character} - Type of bootstrap confidence interval. There are four options:
\itemize{
  \item{\code{normal}: }{Standard normal bootstrap confidence intervals}
  \item{\code{basic}: }{Basic bootstrap confidence intervals}
  \item{\code{percentile}: }{Percentile bootstrap confidence intervals}
  \item{\code{BCa}: }{Bias- and skewness-corrected bootstrap confidence intervals}
}}

\item{level}{\code{Numeric} - Confidence level of the bootstrap confidence intervals. A 95 percent confidence level is the default.}

\item{invalid_NA}{A \code{logical} value. If set to \code{TRUE}, the function will return \code{NA} for zeta when encountering invalid input or computation errors instead of raising an error. While this is not generally recommended due to the risk of masking potential issues, it can be useful in certain scenarios where uninterrupted execution is a priority.}
}
\value{
A \code{data table} with entries \code{CV_A}, \code{CV_A_lwr}, \code{CV_A_upr}, \code{CV_B}, \code{CV_B_lwr}, \code{CV_B_upr}, \code{lambda}, \code{lambda_lwr}, \code{lambda_upr}, \code{Var_A}, \code{Var_A_lwr}, \code{Var_A_upr}, \code{Var_B}, \code{Var_B_lwr}, \code{Var_B_upr}
}
\description{
Obtain all necessary information on imprecision data for each unique pair of IVD-MDs in your data. The output is on the form required by \code{merge_results}(), making it very useful.
}
\examples{
print(1)
}
