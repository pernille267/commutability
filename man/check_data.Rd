% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_data.R
\name{check_data}
\alias{check_data}
\title{check data}
\usage{
check_data(data, silence = 1, type = "cs")
}
\arguments{
\item{data}{\code{list} or \code{data table} that is the data that we wish to validate}

\item{silence}{\code{integer} - results be printed. Default is \code{1}, which signifies no}

\item{type}{\code{character} - Is the data clinical sample data or external quality assessment material data. Set \code{type = "cs"} (default) for the former and \code{type = "eqam"} for the latter}
}
\value{
A \code{list} of validation results
}
\description{
check data
}
\examples{
print(1)
}
