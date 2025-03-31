#' Validates and Converts The \code{data} Argument
#'
#' @description
#' This function is used to validate and convert the \code{data} argument
#' in \code{transform_data}. Not to be used by end-users.
#'
#' @param data An object to be validated and converted to \code{data.table}.
#' @param special_transformation A \code{logical} value. If \code{TRUE},
#'                               \code{data} is checked according to
#'                               requirements necessary for one of the
#'                               special transformations.
#' @return
#' A \code{data.table}. Processed and validated \code{data}.
#' @keywords internal
validate_date_td <- function(data, special_transformation = FALSE) {

  # Avoid modifying the original data
  data <- copy(data)

  # Checks if 'data' is of correct class
  if(!is.data.table(data)){
    if(is.data.frame(data) || is.list(data)){
      setDT(data)
    }
    else{
      stop(
        "The input 'data' is neither a data.table, list, nor data.frame.",
        " Registered input class of 'data': '",
        class(data)[1], "'.",
        " Please provide an input of type data.table, list, or data.frame to proceed.",
        " The calculations cannot continue with the current input type."
      )

    }
  }

  if (special_transformation) {
    # Checks if 'data' have the required columns
    required_columns <- c('comparison',
                          'SampleID',
                          'MP_A',
                          'MP_B')

    missing_columns <- setdiff(required_columns, names(data))
    if (length(missing_columns) > 0) {
      stop(
        "Some required columns are missing from 'data': ",
        "Missing column(s): ",
        paste(missing_columns, collapse = ", "),
        ". ",
        "When transformation is 'unit', 'ba', or 'bal', these columns are ",
        "required: 'comparison', 'SampleID', 'MP_A' and 'MP_B'."
      )
    }
    return(data)
  }

  # Checks if 'data' have the required columns
  required_columns <- c('MP_A', 'MP_B')

  missing_columns <- setdiff(required_columns, names(data))
  if (length(missing_columns) > 0) {
    stop(
      "Some required columns are missing from 'data': ",
      "Missing column(s): ",
      paste(missing_columns, collapse = ", "),
      ". ",
      "The data argument must include 'MP_A' and 'MP_B' for all transformations."
    )
  }

  return(data)
}

#' Extract Relevant Information From the \code{transformation} Argument
#'
#' @description
#' This function is used to validate and extract relevant information from the
#' \code{transformation} argument. Not to be used by end-users.
#'
#' @param transformation
#'
#' @return
#' A \code{list} of length four. Relevant extract information from the
#' character string \code{transformation}.
#' @keywords internal
extract_transformation_information <- function(transformation) {

  # Initialize
  no_transformation <- FALSE
  special_transformation <- FALSE
  transformation_type <- NULL
  transformation_parameter <- NULL

  # Special transformations
  special_transformations <- c("unit",
                               "ba",
                               "bal")

  # Valid transformation types
  valid_transformation_types <- c("log",
                                  "root",
                                  "pow",
                                  "boxcox")

  # Valid custom transformations
  valid_custom_transformations <- c("log",
                                    "ln",
                                    "log#e",
                                    "log10",
                                    "sqrt",
                                    "cuberoot",
                                    "square",
                                    "cube")

  if (!is.character(transformation)) {
    stop(
      "transformation is expected to be a character string, but recieved a ",
      class(transformation),
      ". Ensure that transformation is a character string with format ",
      "'transformation#parameter' and try again."
    )
  }

  if (length(transformation) != 1) {
    warning(
      "transformation is expected to have length 1L, but transformation has ",
      "length ",
      length(transformation),
      ". Only the first element is used."
    )
    transformation <- transformation[1]
  }

  if (is.na(transformation)) {
    stop(
      "transformation is expected to be a character string, but recieved a ",
      "NA value",
      ". Ensure that transformation is a character string with format ",
      "'transformation#parameter' and try again."
    )
  }

  if (transformation == "identity") {
    no_transformation <- TRUE
    output <- list("no_transformation" = no_transformation,
                   "special_transformation" = special_transformation,
                   "transformation_type" = transformation_type,
                   "transformation_parameter" = transformation_parameter)
    return(output)
  }

  else if (any(transformation == special_transformations)) {
    special_transformation <- TRUE
    output <- list("no_transformation" = no_transformation,
                   "special_transformation" = special_transformation,
                   "transformation_type" = transformation_type,
                   "transformation_parameter" = transformation_parameter)
    return(output)

  }

  else if (any(transformation == valid_custom_transformations)) {
    transformation <- switch(transformation,
                             "log" = paste0("log#", exp(1), collapse = ""),
                             "ln" = paste0("log#", exp(1), collapse = ""),
                             "log#e" = paste0("log#", exp(1), collapse = ""),
                             "log10" = "log#10",
                             "sqrt" = "root#2",
                             "cuberoot" = "root#3",
                             "square" = "pow#2",
                             "cube" = "pow#3")
  }

  # Check if format have necessary seperator ([:punct:])
  have_punct <- stri_detect_regex(str = transformation,
                                  pattern = "[:punct:]")

  if (!have_punct) {
    stop(
      "transformation is not formatted in the expected way: ",
      transformation,
      ". Ensure that it is on the correct format: ",
      "'transformation#parameter' and try again."
    )
  }

  # Extract information from string if it has necessary format
  transformation_split <- stri_split(str = transformation,
                                     regex = "[[:punct:]&&[^.]&&[^-]]")[[1]]
  transformation_split <- transformation_split[transformation_split != ""]

  # Check if split results in two values
  if (length(transformation_split) != 2) {
    stop(
      "transformation is not formatted in the expected way: ",
      transformation,
      ". Expected to extract two components from transformation, but got ",
      length(transformation_split),
      "L."
    )
  }

  # Get transformation_type
  transformation_type <- transformation_split[1]

  # Validate transformation type
  if (!any(transformation_type == valid_transformation_types)) {
    stop(
      "The transformation type found in transformation, '",
      transformation_type,
      "' is not amoung the accepted ones: ",
      paste(valid_transformation_types, collapse = ", "),
      "."
    )
  }

  # Get transformation_parameter
  transformation_parameter <- tryCatch(
    expr = {
      as.numeric(
        stri_replace_all(
          str = transformation_split[2],
          replacement = "",
          regex = "[^0-9.-]"
        )
      )
    },
    warning = NA_real_,
    error = NA_real_
  )

  # Validate transformation_parameter
  if (is.na(transformation_parameter)) {
    stop(
      "The transformation parameter found in transformation, ",
      transformation_split[2],
      " is not a valid number."
    )
  }

  # Check validity of transformation_parameter given transformation_type
  if(any(transformation_type == c("log", "root", "boxcox"))) {
    if (transformation_parameter <= 0) {
      stop(
        "Expected a positive transformation parameter because ",
        "the transformation type is '",
        transformation_type,
        "'. But, got a negative value: ",
        transformation_parameter,
        "."

      )
    }
  }

  output <- list("no_transformation" = no_transformation,
                 "special_transformation" = special_transformation,
                 "transformation_type" = transformation_type,
                 "transformation_parameter" = transformation_parameter)

  return(output)

}

#' Apply a Special Transformation to One IVD-MD comparison
#'
#' @description
#' This function is used to apply one of the special transformations to each
#' IVD-MD comparison separately. Used internally in \code{transform_data}
#' if one of the special transformations are desired.
#' Not to be used by end-users.
#'
#' @param data A \code{data.table}. The IVD-MD comparison results for a
#'             particular IVD-MD comparison.
#'
#' @return
#' A \code{data.table}. The transformed data for a particular IVD-MD
#' comparison.
#' @keywords internal
transform_comparison_data <- function(data, transformation) {

  # Bind global variables
  MP_A <- MP_B <- NULL

  unit_trans <- function(x) {
    u <- (x - min(x, na.rm = TRUE)) / diff(range(x, na.rm = TRUE))
    return(u)
  }

  ba_trans <- function(SampleID, ReplicateID = NULL, x, y, ba_log = FALSE) {
    x_ba <- (x + y) / 2
    y_ba <- if (ba_log) {log(y) - log(x)} else {y - x}
    if (!is.null(ReplicateID)) {
      output <- list("SampleID" = SampleID,
                     "ReplicateID" = ReplicateID,
                     "MP_A" = y_ba,
                     "MP_B" = x_ba)
      return(output)
    }
    output <- list("SampleID" = SampleID,
                   "MP_A" = y_ba,
                   "MP_B" = x_ba)

  }

  # Avoid modifying original data
  transformed_comparison_data <- copy(data)

  # Unit-transformation
  if (transformation == "unit") {
    transformed_comparison_data[, MP_B := unit_trans(MP_B)]
    transformed_comparison_data[, MP_A := unit_trans(MP_A)]
  }

  else if(any(transformation %in% c("ba", "bal"))) {
    if (!any("SampleID" == names(transformed_comparison_data))) {
      stop(
        "SampleID is expected in data if transformation = 'bal' or ",
        "transformation = 'ba'. Could not find SampleID among: ",
        paste(names(transformed_comparison_data), collapse = ", "),
        "."
      )
    }

    if (any("ReplicateID" == names(transformed_comparison_data))) {
      transformed_comparison_data <- ba_trans(
        SampleID = transformed_comparison_data$SampleID,
        ReplicateID = transformed_comparison_data$ReplicateID,
        x = transformed_comparison_data$MP_B,
        y = transformed_comparison_data$MP_A,
        ba_log = transformation == "bal"
      )
    }

    else {
      transformed_comparison_data <- ba_trans(
        SampleID = transformed_comparison_data$SampleID,
        ReplicateID = NULL,
        x = transformed_comparison_data$MP_B,
        y = transformed_comparison_data$MP_A,
        ba_log = transformation == "bal"
      )
    }



  }

  return(transformed_comparison_data)

}


#' Apply a Univariate Transformation to \code{MP_A} and \code{MP_B} in \code{data}
#'
#' @description
#' This function is internally to apply a univariate transformation to each
#' numeric column (\code{MP_A} and \code{MP_B}) in \code{data}. Depends on the
#' last two list elements of the output of
#' \code{extract_transformation_information()}. Not to be used by end-users.
#'
#' @param data A \code{data.table}. The IVD-MD comparison results for a
#'             particular IVD-MD comparison.
#'
#' @return
#' A \code{data.table}. The transformed data for a particular IVD-MD
#' comparison.
#' @keywords internal
apply_transformation <- function(data, type, parameter) {

  # Avoid modifying original data
  data <- copy(data)

  # Log-transformations
  if (type == "log") {
    data$MP_A <- ifelse(data$MP_A <= 0,
                        NA_real_,
                        log(data$MP_A,
                            base = parameter))
    data$MP_B <- ifelse(data$MP_B <= 0,
                        NA_real_,
                        log(data$MP_B,
                            base = parameter))
  }

  # Power-transformations
  else if (type == "pow") {

    # Check if parameter is in [1, infty) or (-infty, -1]
    if (parameter < 1 && parameter > -1) {

      if (parameter > 0) {
        warning(
          "The transformation parameter is expected to be larger than or equal ",
          "to 1, or smaller than or equal to -1 in power transformations, but got: ",
          parameter,
          ". Uses 'pow#1' instead (no transformation)."
        )
      }

      else {
        warning(
          "The transformation parameter is expected to be larger than or equal ",
          "to 1, or smaller than or equal to -1 in power transformations, but got: ",
          parameter,
          ". Uses 'pow#-1' instead."
        )
        parameter <- -1
      }


    }
    data$MP_A <- data$MP_A ** parameter
    data$MP_B <- data$MP_B ** parameter
  }

  # Root-transformations
  else if (type == "root") {
    data$MP_A <- ifelse(data$MP_A < 0,
                        NA_real_,
                        data$MP_A ** (1 / parameter))
    data$MP_B <- ifelse(data$MP_B < 0,
                        NA_real_,
                        data$MP_B ** (1 / parameter))
  }

  # Box-Cox-transformations
  else if (type == "boxcox") {
    data$MP_A <- ifelse(data$MP_A < 0,
                        NA_real_,
                        ((data$MP_A ** parameter) - 1) / parameter)
    data$MP_B <- ifelse(data$MP_B < 0,
                        NA_real_,
                        ((data$MP_B ** parameter) - 1) / parameter)
  }

  else {
    stop(
      "Expected a transformation type among 'log', 'pow', 'root' and 'boxcox',",
      " but got: '",
      type,
      "'."
    )
  }

  return(data)

}

#' @title
#' Transformation of IVD-MD Comparison Data
#'
#' @param data A \code{data.table}, \code{list} or \code{data.frame}. Must
#'             contain \code{MP_A} and \code{MP_B}. For special transformations
#'             it must also contain \code{comparison} and \code{SampleID}.
#'             See details on information on special transformations.
#' @param transformation A \code{character} string. A \code{character}
#'                       formatted like this: \code{'transformation#parameter'}.
#'                       See details.
#'
#' @description
#' Apply a particular transformation to \code{MP_A} and \code{MP_B} in
#' \code{data}. Not necessarily univariate transformations. See details.
#'
#' @details
#' To transform \code{MP_A} and \code{MP_B} in \code{data}, one need to specify
#' a particular transformation to apply. There are five different
#' transformations one can apply:
#' \itemize{
#'    \item \code{log: } Log-transformation.
#'          Use \code{transformation = 'log#base'}, where \code{'base'} is the
#'          desired base (positive \code{double}) to use in the
#'          log-transformation. For example, \code{'log#e'} transform the data
#'          using natural logs.
#'    \item \code{root: } Root-transformation. Use
#'          \code{transformation = 'root#n'}, where \code{'n'} (an integer) is
#'          the desired root. For example, \code{'root#2'} is the square root.
#'    \item \code{pow: } Power-transformation. Use
#'          \code{transformation = 'pow#exponent'}, where \code{'exponent'}
#'          (a \code{double}) is the desired power. For example \code{'pow#3'}
#'          applies cube transformation.
#'    \item \code{boxcox: } Box-Cox transformation. Use
#'          \code{transformation = 'boxcox#lambda'}, where \code{'lambda'}
#'          (a positive \code{double}) is the desired Box-Cox parameter.
#'    \item \code{identity: } No transformation.
#' }
#' Alternatively, one can apply custom transformations among the allowed
#' variants:
#' \itemize{
#'    \item \code{'log': } Natural log-transformation. Same as \code{'log#e'}.
#'    \item \code{'ln': }  Natural log-transformation. Same as \code{'log#e'}.
#'    \item \code{'log10': } log-transformation with base \code{10}. Same as
#'          \code{'log#10': }.
#'    \item \code{'sqrt': } Square-root transformation. Same as
#'          \code{'root#2'}.
#'    \item \code{'cuberoot': } Cube-root transformation. Same as
#'          \code{'root#3'}.
#'    \item \code{'square': } Square transformation. Same as \code{'pow#2'}
#'    \item \code{'cube': } Cube transformation. Same as \code{'pow#3'}
#'    \item \code{'unit': } Unit-transformation. Transforms to unit interval.
#'    \item \code{'ba': } Bland-Altman transformation. Two-dimensional
#'          transformation. Using aritmetic differences.
#'    \item \code{'bal': } Bland-Altman transformation. Two-dimensional
#'          transformation. Using log-differences.
#' }
#' The latter three transformations are considered special transformations.
#'
#' Note: using log- or root- type transformations are not allowed if
#' \code{data} contains negative values. Negative values will be replaced
#' with NA-values prior to transformation if unsuitable transformaton types
#' are desired. You have been warned.
#'
#' @return
#' A \code{data.table}. The transformed \code{data}.
#'
#' @export
#'
#' @examples
#' # Required packages
#' library(smooth.commutability)
#' library(data.table)
#'
#' # Use example data from the smooth.commutability package
#' test_cs_data <- copy(crp_cs_data)
#'
#' # Apply log-transformation
#' test_cs_data_log_transformed <- transform_data(data = test_cs_data,
#'                                                transformation = "log")
#'
#' # Output
#' print(test_cs_data_log_transformed)
#'
#' # Apply Bland-Altman transformation (w. logarithmic differences)
#' test_cs_data_bal_transformed <- transform_data(data = test_cs_data,
#'                                                transformation = "bal")
#'
#' # Output
#' print(test_cs_data_bal_transformed)
#'
#' # Apply Box-Cox transformation (w. parameter = 0.5)
#' test_cs_data_boxcox_0.5_transformed <- transform_data(data = test_cs_data,
#'                                                       transformation = "boxcox#0.5")
#'
#' # Output
#' print(test_cs_data_boxcox_0.5_transformed)

transform_data <- function(data, transformation = "log"){

  # Validate and extract information from transformation
  transformation_information <- extract_transformation_information(transformation)

  # Return data if transformation = "identity"
  if (transformation_information$no_transformation) {
    return(data)
  }

  # Validate and convert data
  data <- validate_date_td(
    data = data,
    special_transformation = transformation_information$special_transformation
  )

  # Avoid modifying the orignal data
  data <- copy(data)

  # If a special transformation is to be used.
  if (transformation_information$special_transformation) {
    data <- data[, transform_comparison_data(data = .SD,
                                             transformation = transformation),
                 by = "comparison"]

    return(data)
  }

  # If a transformation with valid format is given:
  transformation_type <- transformation_information$transformation_type
  transformation_parameter <- transformation_information$transformation_parameter

  data <- apply_transformation(data = data,
                               type = transformation_type,
                               parameter = transformation_parameter)

  return(data)
}


