#' @title
#' Validates and Converts A \code{data} Argument
#'
#' @param data A \code{data.table}, \code{list} or \code{data.frame}. Must
#'             contain \code{required_columns}.
#' @param argument_name A \code{character} string. The name of the argument.
#' @param required_columns A \code{character} vector. The required columns in
#'                         \code{data}.
#'
#' @description
#' Validates and converts \code{data} to a \code{data.table} object.
#'
#' @details
#' When \code{data} fail a particular validation test, an error is thrown that
#' describes why \code{data} does not pass as a valid \code{data} argument.
#'
#' @returns A \code{data.table}. The validated and converted \code{data}.
#' @export
#'
#' @examples print(1)
validate_data <- function(data,
                          argument_name = "data",
                          required_columns = c('comparison',
                                               'SampleID',
                                               'ReplicateID',
                                               'MP_A',
                                               'MP_B')) {

  # Binding of global variables
  SampleID <- ReplicateID <- NULL

  # Avoid modifying the original data
  data <- copy(data)

  # Checks if 'data' is of correct class
  if(!is.data.table(data)){
    if(is.data.frame(data) || is.list(data)){
      setDT(data)
    }
    else{
      stop(
        paste0("The input '", argument_name, "' is neither a data.table, "),
        "list, nor data.frame. Registered input class of '",
        paste0(argument_name, "': '", class(data)[1], "'."),
        " Please provide an input of type data.table, list, or data.frame to proceed.",
        " The calculations cannot continue with the current input type."
      )
    }
  }

  # Checks if 'data' have the required columns
  missing_columns <- setdiff(required_columns, names(data))
  if(length(missing_columns) > 0){
    stop(
      "Some required columns are missing from '",
      argument_name,
      "'.",
      "Missing column(s): [",
      paste(missing_columns, collapse=", "),
      "] The '",
      argument_name,
      "' argument must include: ",
      paste0(required_columns, collapse = ", "),
      "."
    )
  }

  return(data)

}

#' @title
#' Validates and Converts a \code{character} Vector
#'
#' @param x A \code{character} vector. The vector to check and convert.
#' @param argument_name A \code{character} string. The name of the argument to
#'                      check.
#' @param defaults_to A \code{character} vector. The fallback vector if
#'                    \code{x} is not among \code{valid_inputs}.
#' @param valid_inputs A \code{character} vector. Valid inputs for \code{x}.
#'                     If \code{NULL}, all \code{character} vectors with
#'                     \code{valid_length} are accepted.
#' @param valid_length An \code{integer}. The expected length of \code{x}.
#' @param NA_valid A \code{logical} value. If \code{TRUE}, \code{NA}-values in
#'                 \code{x} is acceptable, and will not result in an error.
#'
#' @description
#' Validates \code{x}. \code{x} is expected to be a \code{character} vector of
#' length \code{valid_length} and a subset of \code{valid_inputs}.
#'
#' @details
#' When \code{x} fails a particular validation test, an error is thrown
#' that describes why \code{x} does not pass as valid \code{character} vector.
#'
#' @returns
#' Validated and converted \code{x}.
#' @export
#'
#' @examples
#' test_x <- 1L
#'
#' # Validate test_x
#'
#' # Will result in error because test_x is an integer.
#'
#' tryCatch(
#'   expr = {
#'     validate_character(
#'       x = test_x,
#'       argument_name = "test_x",
#'       defaults_to = "hello",
#'       valid_inputs = c("hello", "bye", "cya"),
#'       valid_length = 1L,
#'       NA_valid = FALSE
#'     )
#'   },
#'   error = function(e) "GOT AN ERROR",
#'   warning = function(w) "GOT A WARNING"
#' )
#'
#' test_x <- c("bye", "cya", "bye", "bye")
#'
#' # Will result in a warning where only the first element is kept
#' validate_character(x = test_x,
#'                    argument_name = "test_x",
#'                    defaults_to = "hello",
#'                    valid_inputs = c("hello", "bye", "cya"),
#'                    valid_length = 1L,
#'                    NA_valid = FALSE)
#'
#'
#'
validate_character <- function(x,
                               argument_name,
                               defaults_to,
                               valid_inputs = NULL,
                               valid_length = 1L,
                               NA_valid = FALSE) {

  # Checks if not character
  if (!is.character(x)) {
    stop(
      paste0("Expected a character for '", argument_name, "', but got a: "),
      class(x)[1],
      paste0(". Ensure that '", argument_name, "' is a character "),
      "and try again."
    )
  }

  # Check if length is correct
  if (length(x) != valid_length) {
    if (length(x) == 0) {
      stop(
        paste0("Expected a character vector for '", argument_name, "', "),
        "but got an empty vector.",
        paste0("Ensure that '", argument_name, "' is a character vector of "),
        "length ", valid_length, "L and try again."
      )
    }
    else if (length(x) < valid_length) {
      stop(
        paste0("Expected a character vector of length ", valid_length, "L"),
        paste0("for '", argument_name, "', but got a character vector of "),
        paste0("length ", length(x), "L. "),
        paste0("Ensure that '", argument_name, "' is a character vector of "),
        "length ", valid_length, "L and try again."
      )
    }
    else if (length(x) > valid_length) {
      warning(
        paste0("Expected a character vector of length ", valid_length, "L "),
        paste0("for '", argument_name, "', but got a character vector of "),
        paste0("length ", length(x), "L."),
        paste0("Only the first ", valid_length, " elements are used.")
      )
      x <- x[1:valid_length]
    }
  }

  # Checks if any NA
  if (any(is.na(x)) && (!NA_valid)) {
    stop(
      paste0("Expected a character vector of length ", valid_length, "L "),
      paste0("for '", argument_name, "', but at least one NA value were found."),
      paste0("Ensure that '", argument_name, "' does not contain any NA "),
      "values and try again."
    )
  }

  # Checks if there are a set of valid inputs for x
  if (is.null(valid_inputs)) {
    return(x)
  }

  # Checks if input is among the valid
  else if (!any(x %in% valid_inputs)) {
    warning(
      "Expected a character vector that is a subset of: ",
      paste0(valid_inputs, collapse = ", "),
      ".",
      "But got: ",
      paste0(x, collapse = ", "),
      ". Default for '",
      argument_name,
      "' is used instead (",
      paste0(defaults_to, collapse = ", "),
      ")."
    )
    x <- defaults_to
  }

  return(x)
}
