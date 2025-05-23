% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/validation_checks.R
\name{validate_character}
\alias{validate_character}
\title{Validates and Converts a \code{character} Vector}
\usage{
validate_character(
  x,
  argument_name,
  defaults_to,
  valid_inputs = NULL,
  valid_length = 1L,
  NA_valid = FALSE
)
}
\arguments{
\item{x}{A \code{character} vector. The vector to check and convert.}

\item{argument_name}{A \code{character} string. The name of the argument to
check.}

\item{defaults_to}{A \code{character} vector. The fallback vector if
\code{x} is not among \code{valid_inputs}.}

\item{valid_inputs}{A \code{character} vector. Valid inputs for \code{x}.
If \code{NULL}, all \code{character} vectors with
\code{valid_length} are accepted.}

\item{valid_length}{An \code{integer}. The expected length of \code{x}.}

\item{NA_valid}{A \code{logical} value. If \code{TRUE}, \code{NA}-values in
\code{x} is acceptable, and will not result in an error.}
}
\value{
Validated and converted \code{x}.
}
\description{
Validates \code{x}. \code{x} is expected to be a \code{character} vector of
length \code{valid_length} and a subset of \code{valid_inputs}.
}
\details{
When \code{x} fails a particular validation test, an error is thrown
that describes why \code{x} does not pass as valid \code{character} vector.
}
\examples{
test_x <- 1L

# Validate test_x

# Will result in error because test_x is an integer.

tryCatch(
  expr = {
    validate_character(
      x = test_x,
      argument_name = "test_x",
      defaults_to = "hello",
      valid_inputs = c("hello", "bye", "cya"),
      valid_length = 1L,
      NA_valid = FALSE
    )
  },
  error = function(e) "GOT AN ERROR",
  warning = function(w) "GOT A WARNING"
)

test_x <- c("bye", "cya", "bye", "bye")

# Will result in a warning where only the first element is kept
validate_character(x = test_x,
                   argument_name = "test_x",
                   defaults_to = "hello",
                   valid_inputs = c("hello", "bye", "cya"),
                   valid_length = 1L,
                   NA_valid = FALSE)



}
