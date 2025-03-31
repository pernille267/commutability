#' Validates \code{data} and \code{new_data}
#'
#' @description
#' This function is used to validate and convert the \code{data} or
#' \code{new_data} arguments in \code{do_commutability_evaluation}.
#'
#' @param data A object to be validated.
#' @param na_rm A \code{logical} value. If \code{TRUE}, \code{NA} values are
#'              removed from the validated \code{data}.
#' @return Processed \code{data} or \code{new_data}
#' @keywords internal
validate_data_dce <- function(data, na_rm = TRUE){

  # Avoid modifying original input
  data <- copy(data)

  # Convert data iff possible. Error otherwise
  if (!is.data.table(data)) {
    if (is.data.frame(data) || is.list(data)) {
      setDT(data)
    }
    else {
      stop(sprintf("Invalid class '%s'. Expected data.table, data.frame, or list.",
                   class(data)[1]))

    }
  }

  # Req. columns in data.
  required_columns <- c('comparison', 'SampleID', 'ReplicateID', 'MP_A', 'MP_B')

  # Misng. Columns in data
  missing_columns <- setdiff(required_columns, names(data))

  # Throws an error if some column names are missing of 'data'
  if(length(missing_columns) > 0){
    stop(sprintf("Missing required columns in data: [%s]",
                 paste(missing_columns, collapse=", ")))
  }

  # Removes NA values from 'data' if 'na_rm' = TRUE
  if(isTRUE(na_rm)){
    data <- na.omit(data)
  }

  return(data)

}

#' Validates Names of \code{data} and \code{new_data}
#'
#' @description
#' This function is used to check if \code{data} and \code{new_data} have the
#' same names and the same order. This is a helper function used in
#' \code{estimate_prediction_data}
#'
#' @param data_list Object to be checked.
#' @param new_data_list Object to be checked.
#'
#' @return
#' Returns nothing if check is successful. Otherwise, an error is thrown.
#' @keywords internal
check_names_dce <- function(data_list, new_data_list){

  # Checks if 'data_list' and 'new_data_list' have the same names and in the same order
  if(!identical(names(data_list), names(new_data_list))) {
    missing_comparisons_new_data <- setdiff(names(data_list), names(new_data_list))
    missing_comparisons_data <- setdiff(names(new_data_list), names(data_list))
    if(any(length(missing_comparisons_data) > 0, length(missing_comparisons_new_data) > 0)){
      stop(sprintf("The elements of the 'comparison' column of 'data' does not match that of 'new_data'.
                   \nComparisons in 'data' but not in 'new_data': [%s]
                   \nComparisons in 'new_data' but not in 'data': [%s]",
                   paste(missing_comparisons_new_data, collapse = ", "),
                   paste(missing_comparisons_data, collapse = ", ")))
    }
    else{
      stop(sprintf("The order of the elements of the 'comparison' column of 'data' does not match that of 'new_data'.
                   \nOrder in 'data': [%s]
                   \nOrder in 'new_data': [%s]",
                   paste(names(data_list), collapse = ", "),
                   paste(names(new_data_list), collapse = ", ")))
    }
  }
}

#' Attempt to Fix Upper Zeta
#'
#' @description
#' Try to fix upper zeta
#'
#' @param upper_zeta Object to be checked.
#' @param avoid_simulations Object to be checked.
#'
#' @return
#' Returns nothing if check is successful. Otherwise, an error is thrown.
#' @keywords internal
attempt_fix_upper_zeta <- function(upper_zeta, avoid_simulations) {

  # Converted upper_zeta
  converted_upper_zeta <- NULL

  # Check if a number [digits.digits] if detected among noise
  number_detected <- stri_detect_regex(str = upper_zeta, pattern = "^\\d+\\.\\d+$")

  if (!number_detected) {

    # Remove all non-numeric components from string
    upper_zeta_without_noise <- stri_replace_all_regex(str = upper_zeta,
                                                       pattern = "[^[:digit:].]",
                                                       replacement = "")
    converted_upper_zeta <- stri_replace_all_regex(str = upper_zeta_without_noise,
                                                   pattern = "\\.{2,}",
                                                   replacement = ".")
    upper_zeta_start_with_dot <- stri_startswith(str = converted_upper_zeta,
                                                 fixed = ".")
    upper_zeta_ends_with_dot <- stri_endswith(str = converted_upper_zeta,
                                              fixed = ".")
    if (upper_zeta_start_with_dot) {
      converted_upper_zeta <- stri_replace_first(str = converted_upper_zeta,
                                                 fixed = ".",
                                                 replacement = "")
    }

    if (upper_zeta_ends_with_dot) {
      converted_upper_zeta <- stri_replace_first(str = converted_upper_zeta,
                                                 fixed = ".",
                                                 replacement = "")
    }

    # Count the number of dots in the string. Should be only one!
    current_number_of_dots_in_converted_upper_zeta <- stri_count_fixed(
      str = converted_upper_zeta,
      pattern = "."
    )

    if (current_number_of_dots_in_converted_upper_zeta > 1) {

      converted_upper_zeta <- tryCatch(
        expr = {
          as.numeric(converted_upper_zeta)
        },
        warning = function(w) NA_real_,
        error = function(e) NA_real_
      )

      if (is.na(converted_upper_zeta) && avoid_simulations) {
        stop(
          sprintf(
            paste(
              "The upper_zeta %s argument is a character string and it was",
              "not possible to convert it to a double.",
              "Because avoid_simulations = TRUE, upper_zeta will not be",
              "approximated using Monte Carlo simulations"
            ),
            upper_zeta
          )
        )
      }

      return(list(upper_zeta = converted_upper_zeta,
                  simulate_upper_zeta = (is.na(converted_upper_zeta)) && (!avoid_simulations)))

    }

    converted_upper_zeta <- tryCatch(
      expr = {
        as.numeric(converted_upper_zeta)
      },
      warning = function(w) NA_real_,
      error = function(e) NA_real_
    )

    if (is.na(converted_upper_zeta) && avoid_simulations) {
      stop(
        sprintf(
          paste(
            "The upper_zeta %s argument is a character string and it was",
            "not possible to convert it to a double.",
            "Because avoid_simulations = TRUE, upper_zeta will not be",
            "approximated using Monte Carlo simulations"
          ),
          upper_zeta
        )
      )
    }

    return(list(upper_zeta = converted_upper_zeta,
                simulate_upper_zeta = (is.na(converted_upper_zeta)) && (!avoid_simulations)))

  }

  converted_upper_zeta <- tryCatch(
    expr = {
      as.numeric(upper_zeta)
    },
    warning = function(w) NA_real_,
    error = function(e) NA_real_
  )

  return(list(upper_zeta = converted_upper_zeta,
              simulate_upper_zeta = FALSE))

}


#' @title
#' Run a Complete Commutability Evaluation Analysis Based on Clinical Sample
#' Data and Evaluated Material Data
#'
#' @param data A \code{list}, \code{data.table} or \code{data.frame}. Must
#'             contain the measurements of clinical samples grouped by
#'             \code{comparison}, \code{SampleID} and \code{ReplicateID}.
#' @param new_data A \code{list}, \code{data.table}, or \code{data.frame}. Must
#'                 contain the measurements of either external quality
#'                 assessment materials or reference materials grouped by
#'                 \code{comparison}, \code{SampleID} and \code{ReplicateID}.
#' @param B An \code{integer} \eqn{geq} 100 that represents the number of bootstrap
#'          replicates utilized for estimating bootstrap confidence intervals.
#'          The default is set to \code{2000}, a common choice for such
#'          calculations. Please be aware that if your \code{data} contains
#'          more than five unique IVD-MDs, the resampling process might take a
#'          couple of seconds to complete due to the computational complexity.
#' @param N An \code{integer} \eqn{geq} 100 that specifies the number of
#'          bootstrap resamples used for estimating the inside rates
#'          (commutability conclusion consistency) for the evaluated material.
#'          Set to \code{NULL} to exclude estimation of inside rates.
#' @param method_pi A \code{character} string referring to the method to employ
#'                  for estimating the Deming prediction intervals. The default
#'                  is \code{'fg'}, standing for the Fuller and Gillard
#'                  approach. Alternatively, the approach derived by CLSI EP14
#'                  can be used by setting \code{'clsi'}.
#' @param method_bs A \code{character} string. Determines the bootstrap
#'                  method to apply for estimating the confidence intervals
#'                  of \eqn{\zeta} and repeatability statistics for each IVD-MD
#'                  comparison.
#' @param level_pi A \code{double}. Must be between \code{0} and \code{1}.
#'                 This value is the desired nominal confidence level for the
#'                 estimated prediction intervals. The default is \code{0.99}
#'                 (\eqn{99\%}).
#' @param level_bs A \code{double}. Must be between 0 and 1. The desired
#'                 nominal confidence level for the bootstrap estimated
#'                 confidence intervals. The default is \code{0.95}
#'                 (\eqn{95\%}).
#' @param M A \code{double} \eqn{\geq 0}. If \code{upper_zeta} is either
#'          \code{NULL}, \code{NA}, or an improperly formatted \code{character}
#'          string (see details), and \code{avoid_simulations = FALSE}, this
#'          parameter will signify the acceptable level of differences in
#'          non-selectivity between all IVD-MD pairs within \code{data} and
#'          \code{new_data}. This tolerance level is expressed as an average
#'          relative increase in the widths of pointwise prediction intervals.
#' @param upper_zeta A \code{double} \eqn{\geq 0}. The maximum value \eqn{\hat{\zeta}}
#'                   can take that correspond with an acceptable magnitude of
#'                   differences in non-selectivity.
#' @param output A \code{character} string. The output structure:
#'               \itemize{
#'                  \item \code{'sufficient': } Only return essential results.
#'                  \item \code{'complete': } Return all results.
#'               }
#' @param avoid_simulations A \code{logical} value. Ensures that an error is
#'                          thrown if \code{upper_zeta} is a \code{character}
#'                          string with invalid syntax (see details) instead of
#'                          approximating \code{upper_zeta} values utilizing
#'                          tedious Monte Carlo simulations. Defaults to
#'                          \code{FALSE}.
#'
#' @return
#' A \code{list} of length two. Contains:
#' \itemize{
#'    \item \code{merged_pb_data: } Comprises pointwise prediction intervals
#'          (prediction band data). Intended solely for plotting purposes
#'    \item \code{merged_ce_data: } A thorough, tabulated summary of the
#'          commutability evaluation data analysis.
#' }
#' @export
#'
#' @examples print(1)

do_commutability_evaluation <- function(data,
                                        new_data,
                                        B = 2e3L,
                                        N = 1e3L,
                                        method_pi = "fg",
                                        method_bs = "percentile",
                                        level_pi = 0.99,
                                        level_bs = 0.95,
                                        M = 0,
                                        upper_zeta = 2.25,
                                        output = "sufficient",
                                        avoid_simulations = FALSE){

  # Initialization
  simulate_zeta_upper <- FALSE

  # Checks if data is list, data.table or data.frame
  data <- validate_data_dce(data)

  # Checks if new_data is list, data.table or data.frame
  new_data <- validate_data_dce(new_data)

  # Split by comparison
  data_list <- split(x = data, by = "comparison", keep.by = FALSE)
  new_data_list <- split(x = new_data, by = "comparison", keep.by = FALSE)

  # Check if comparions are equal in data and new_data
  check_names_dce(data_list, new_data_list)

  # Calculate the number of replicates
  R_cs_list <- lapply(X = data_list,
                      FUN = function(x) {
                        count_samplewise_replicates(x, "ceiling")$R_i
                      })

  R_eq_list <- lapply(X = new_data_list,
                      FUN = function(x) {
                        count_samplewise_replicates(x, "ceiling")$R_i
                      })

  R_ratio_list <- mapply(function(R_cs, R_eq) R_cs / R_eq,
                         R_cs_list,
                         R_eq_list,
                         SIMPLIFY = F)

  # Calculate pb_data component
  pb_data <- estimate_prediction_data(data = data,
                                      new_data = "gen_250",
                                      method = method_pi,
                                      level = level_pi,
                                      rounding = 3L,
                                      override_R_ratio = R_ratio_list)

  # Calculate ce_data component
  ce_data <- estimate_prediction_data(data = data,
                                      new_data = new_data,
                                      method = method_pi,
                                      level = level_pi,
                                      rounding = 3L,
                                      B = N)

  # Calculate imprecision_data component
  impr_data <- estimate_imprecision_data(data = data,
                                         type = method_bs,
                                         level = level_bs,
                                         B = B)


  # Checks if upper_zeta is a vector of length > 1
  if(length(upper_zeta) > 1){
    upper_zeta <- upper_zeta[1]
  }

  if(is.null(upper_zeta) || length(upper_zeta) == 0 || is.na(upper_zeta)){
    simulate_zeta_upper <- TRUE
  }

  else if(is.character(upper_zeta)){
    attempted_fix_of_upper_zeta <- attempt_fix_upper_zeta(upper_zeta = upper_zeta,
                                                          avoid_simulations = avoid_simulations)
    upper_zeta <- attempted_fix_of_upper_zeta$upper_zeta
    simulate_zeta_upper <- attempted_fix_of_upper_zeta$simulate_upper_zeta
  }

  zeta_method <- switch(method_pi,
                        "ols" = "ols",
                        "fg" = "ols",
                        "clsi" = "ols",
                        "ss" = "ss",
                        "ssw" = "ssw")

  if(simulate_zeta_upper){

    # Check if M < 0 (negative). If so, set M = 0
    if(M < 0){
      M <- 0
    }

    zeta_data <- estimate_zeta_data(data = data,
                                    B = B,
                                    type = method_bs,
                                    level = level_bs,
                                    M = M,
                                    N = 1e4,
                                    method = zeta_method,
                                    zeta_critical = NULL)

  }
  else {
    zeta_data <- estimate_zeta_data(data = data,
                                    B = B,
                                    type = method_bs,
                                    level = level_bs,
                                    M = M,
                                    N = 1e4,
                                    method = zeta_method,
                                    zeta_critical = upper_zeta)
  }

  include_impr_data <- switch(output,
                              "complete" = TRUE,
                              "sufficient" = FALSE,
                              FALSE)

  merged_results <- merge_results(pb_data = pb_data,
                                  ce_data = ce_data,
                                  imprecision_data = impr_data,
                                  zeta_data = zeta_data,
                                  rounding = 3L,
                                  include_imprecision_estimates = include_impr_data)

  merged_results$merged_ce_data <- merge(x = merged_results$merged_ce_data,
                                         y = ce_data[,c("comparison",
                                                        "SampleID",
                                                        "extrapolate",
                                                        "inside_rate")],
                                         by = c("comparison", "SampleID"))

  if(include_impr_data) {
    setcolorder(x = merged_results$merged_ce_data,
                neworder = c(names(merged_results$merged_ce_data)[1:13],
                             "extrapolate",
                             "inside_rate",
                             "CV_A",
                             "CV_A_lwr",
                             "CV_A_upr",
                             "CV_B",
                             "CV_B_lwr",
                             "CV_B_upr",
                             "lambda",
                             "lambda_lwr",
                             "lambda_upr"))
  }


  setDT(merged_results$merged_pb_data)
  setDT(merged_results$merged_ce_data)

  return(merged_results)

}
