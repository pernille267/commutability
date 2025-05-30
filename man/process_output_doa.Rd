% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/do_outlier_analysis.R
\name{process_output_doa}
\alias{process_output_doa}
\title{Transform Raw Outlier Test Summary to Desired Format}
\usage{
process_output_doa(raw_output, method, output = "visual")
}
\arguments{
\item{raw_output}{A \code{data.table}. Output from \code{outlier_tests()}.}

\item{method}{A \code{character} string. See \code{?do_outlier_analysis()}.}

\item{output}{A \code{character} string. See \code{?do_outlier_analysis()}.}
}
\value{
A \code{data.table}. The processed output with the desired format.
}
\description{
This function is used within \code{do_outlier_analysis()} to process raw
output to the desired format. This function should not be used by end-users.
}
\keyword{internal}
