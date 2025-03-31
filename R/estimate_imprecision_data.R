#' Validates \code{data}
#'
#' @description
#' This function is used to validate and convert the \code{data} argument
#' in \code{estimate_imprecision_data}.
#'
#' @param data A object to be validated.
#'
#' @return Processed \code{data}
#' @keywords internal
validate_data_eid <- function(data){

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
    stop(
      sprintf(
        "Missing required columns in data: [%s]",
        paste(missing_columns, collapse = ", ")
      )
    )
  }

  data$SampleID <- as.character(data$SampleID)
  data$ReplicateID <- as.character(data$ReplicateID)


  return(data)
}

#' Validates \code{B} parameter
#'
#' @description
#' This function validates the \code{B} parameter (bootstrap iterations)
#' in \code{estimate_imprecision_data}.
#'
#' @param B A numeric value representing bootstrap iterations.
#' @param invalid_NA A \code{logical} value. If \code{TRUE}, \code{NA} is
#'                   returned instead of throwing an error. Default is \code{FALSE}.
#'
#' @return
#' A \code{list}. The processed \code{B} and which action should be taken
#' based on the value of \code{B}.
#' @keywords internal
validate_B_eid <- function(B, invalid_NA = FALSE) {

  # Initialize return values
  B_action <- NULL
  B_message <- NULL
  B_message_type <- NULL

  # Check if B is NULL or empty
  if (is.null(B) || length(B) == 0) {
    B_action <- "return_orig_imps"
    return(list(B = NULL,
                B_action = B_action,
                B_message = B_message,
                B_message_type = B_message_type))
  }

  # Check if B is not numeric
  if (!is.numeric(B)) {
    B_action <- "throw_error"
    B_message_type <- "error"
    B_message <- "B is expected to be an integer larger than or equal to 50L."

    if (invalid_NA) {
      B_message_type <- "warning"
      B_action <- "return_NA_imps"
      B_message <- paste(B_message, "NA returned.")
    } else {
      B_message <- paste(B_message, "Try again with a valid B.")
    }

    return(list(B = NULL,
                B_action = B_action,
                B_message = B_message,
                B_message_type = B_message_type))
  }

  # Check if B has multiple values
  if (length(B) > 1) {
    B_action <- "throw_warning"
    B_message <- paste0("B was of length ", length(B),
                        ". Only the first element is used.")
    B_message_type <- "warning"
    B <- B[1]
  }

  # Check if B is NA
  if (is.na(B)) {
    B_action <- "return_orig_imps"
    B_message <- paste("The first element of B is a NA value.",
                       "Bootstrap confidence intervals are not estimated.")
    B_message_type <- "warning"
    return(list(B = NULL,
                B_action = B_action,
                B_message = B_message,
                B_message_type = B_message_type))
  }

  # Round up B to nearest integer
  B <- as.integer(ceiling(B))

  # Check the value of B
  if (B < 0) {
    B_message <- paste("B is negative. It must be a positive integer",
                       "larger than or equal to 50L.")

    if (invalid_NA) {
      B_action <- "return_orig_imps"
      B_message <- paste(B_message,
                         "Bootstrap confidence intervals are not estimated.")
      B_message_type <- "warning"
    } else {
      B_action <- "throw_error"
      B_message_type <- "error"
    }

    return(list(B = NULL,
                B_action = B_action,
                B_message = B_message,
                B_message_type = B_message_type))
  }
  else if (B < 50) {
    B_action <- "return_orig_imps"
    B_message <- paste("B is too small. It must be a positive integer",
                       "larger than or equal to 50L.",
                       "Bootstrap confidence intervals are not estimated.")
    B_message_type <- "warning"
    return(list(B = NULL,
                B_action = B_action,
                B_message = B_message,
                B_message_type = B_message_type))
  }
  else if (B > 1e6) {
    B_action <- "use_default_B"
    B_message <- "B is too large. The default (B = 2e3L) is used instead."
    B_message_type <- "warning"
    return(list(B = 2e3L,
                B_action = B_action,
                B_message = B_message,
                B_message_type = B_message_type))
  }

  # B is valid
  B_action <- "use_B"

  return(list(B = B,
              B_action = B_action,
              B_message = B_message,
              B_message_type = B_message_type))
}

#' @title
#' Estimates Imprecision Bootstrap Confidence Intervals
#'
#' @param B An \code{integer}. The desired number of bootstrap replicates used
#'          to estimate bootstrap confidence intervals. See
#'          \code{?estimate_imprecision_data()} for more information.
#' @param data A \code{data.table} or \code{list}. The clinical sample data.
#'             See \code{?estimate_imprecision_data()} for more information.
#' @param type An \code{integer}. The type of bootstrap confidence interval
#'             desired. Can be \code{1-4}.
#' @param level A \code{double}. The desired nominal confidence level of the
#'              estimated boostrap confidence intervals.
#'
#' @details
#' Not to be used by end-users. Used internally in
#' \code{estimate_imprecision_data()}.
#'
#'
#' @returns
#' A \code{list}. The estimated boostrap confidence intervals.
#' @keywords internal
estimate_imp_boot_ci <- function(B, data, type, level) {

  # Avoid to modify original data
  data <- copy(data)

  # Original imprecision estimates
  orig_imps <- data[, global_precision_estimates(.SD)]

  # Resampled imprecision estimates
  resampled_imps <- replicate(
    n = B,
    expr = resample_global_precision_estimates(data),
    simplify = FALSE
  )

  # Leave-One-Out imprecision estimates
  loo_imps <- sapply(
    X = seq_len(length.out = length(unique(data$SampleID))),
    FUN = function(smpl){
      loo_global_precision_estimates(
        data = data,
        loo_id = smpl
      )
    },
    simplify = FALSE
  )

  # Convert to data.table
  resampled_imps <- rbindlist(resampled_imps)
  loo_imps <- rbindlist(loo_imps)

  # Estimate bootstrap intervals
  boot_ci_Var_A <- bootstrap_ci(
    bootstrapped_parameter_estimates = resampled_imps$Var_A,
    jackknife_parameter_estimates = loo_imps$Var_A,
    original_parameter_estimate = orig_imps$Var_A,
    type = type,
    level = level
  )
  boot_ci_Var_B <- bootstrap_ci(
    bootstrapped_parameter_estimates = resampled_imps$Var_B,
    jackknife_parameter_estimates = loo_imps$Var_B,
    original_parameter_estimate = orig_imps$Var_B,
    type = type,
    level = level
  )
  boot_ci_CV_A <- bootstrap_ci(
    bootstrapped_parameter_estimates = resampled_imps$CV_A,
    jackknife_parameter_estimates = loo_imps$CV_A,
    original_parameter_estimate = orig_imps$CV_A,
    type = type,
    level = level
  )
  boot_ci_CV_B <- bootstrap_ci(
    bootstrapped_parameter_estimates = resampled_imps$CV_B,
    jackknife_parameter_estimates = loo_imps$CV_B,
    original_parameter_estimate = orig_imps$CV_B,
    type = type,
    level = level
  )
  boot_ci_lambda <- bootstrap_ci(
    bootstrapped_parameter_estimates = resampled_imps$lambda,
    jackknife_parameter_estimates = loo_imps$lambda,
    original_parameter_estimate = orig_imps$lambda,
    type = type,
    level = level
  )

  out <- list("Var_A" = orig_imps$Var_A,
              "Var_A_lwr" = boot_ci_Var_A[1],
              "Var_A_upr" = boot_ci_Var_A[2],
              "Var_B" = orig_imps$Var_B,
              "Var_B_lwr" = boot_ci_Var_B[1],
              "Var_B_upr" = boot_ci_Var_B[2],
              "CV_A" = orig_imps$CV_A,
              "CV_A_lwr" = boot_ci_CV_A[1],
              "CV_A_upr" = boot_ci_CV_A[2],
              "CV_B" = orig_imps$CV_B,
              "CV_B_lwr" = boot_ci_CV_B[1],
              "CV_B_upr" = boot_ci_CV_B[2],
              "lambda" = orig_imps$lambda,
              "lambda_lwr" = boot_ci_lambda[1],
              "lambda_upr" = boot_ci_lambda[2])

  out <- lapply(
    X = out,
    FUN = function(stat){
      if (!isTRUE(stat >= 0)) {
        return(NA_real_)
      }
      else{
        return(stat)
      }
    }
  )
  return(out)
}

#' @title
#' All calculations regarding imprecision
#'
#' @param data A \code{list}, \code{data.table} or \code{data.frame}. The
#'             CS data. Must contain the following variables:
#'             \itemize{
#'                \item \code{comparison: } A \code{character} vector. The
#'                      comparison identifiers. Typically on the form
#'                      \code{'MP_A - MP_B'}.
#'                \item \code{SampleID: } A \code{character} vector. The sample
#'                      identifiers for the clinical samples.
#'                \item \code{ReplicateID: }A \code{character} vector. The
#'                      replicated measurement identifiers.
#'                \item \code{MP_A: } A \code{numeric} vector. The observed
#'                      measurement results from IVD-MD \code{MP_A} (response).
#'                \item \code{MP_B: } A \code{numeric} vector. The observed
#'                      measurement results from IVD-MD \code{MP_A} (predictor).
#'             }
#' @param B An \code{integer}. Must be larger than or equal to \code{50}. The
#'          Number of bootstrap replicates used to estimate bootstrap
#'          confidence intervals for each of the repeatability imprecision
#'          statistics. Defaults to \code{2e3L} (2,000). If \code{NULL},
#'          estimated confidence intervals are not calculated.
#' @param type A \code{character} string. Determines the bootstrap method to
#'             apply for estimating the confidence intervals:
#'             \itemize{
#'                \item \code{normal: } Standard normal bootstrap confidence
#'                                      interval.
#'                \item \code{basic: } Basic bootstrap confidence interval.
#'                \item \code{percentile: } Percentile bootstrap confidence
#'                                          interval.
#'                \item \code{BCa: } Bias- and skewness-corrected bootstrap
#'                                   confidence interval.
#'            }
#' @param level A \code{double}. Must be between \code{0} and \code{1}. The
#'              desired nominal confidence level for the bootstrap estimated
#'              confidence intervals. The default is \code{0.95} (\eqn{95\%}).
#' @param invalid_NA A \code{logical} value. If \code{TRUE} (not recommended),
#'                   return \code{NA} rather than throwing an error if
#'                   something goes wrong.
#' @description
#' Obtains all necessary information on repeatability imprecision for each
#' unique IVD-MD pair (\code{comparison}) in \code{data}.
#'
#' @details
#' Estimate \code{Var_A}, \code{Var_B}, \code{CV_A}, \code{CV_B} and
#' \code{lambda}, with corresponding bootstrap confidence intervals for each
#' unique \code{comparison} element in \code{data}.
#'
#' This function is generally not recommended to be used by end-users. It is
#' however an extremely important helping function that is used in the
#' \code{do_commutability_evaluation()} wrapper function.
#'
#'
#' @return
#' A \code{data.table} with \code{unique(data$comparison)} rows and \code{5} or
#' \code{15} columns. The columns are named: \code{CV_A}, \code{CV_A_lwr},
#' \code{CV_A_upr}, \code{CV_B}, \code{CV_B_lwr}, \code{CV_B_upr},
#' \code{lambda}, \code{lambda_lwr}, \code{lambda_upr}, \code{Var_A},
#' \code{Var_A_lwr}, \code{Var_A_upr}, \code{Var_B}, \code{Var_B_lwr},
#' \code{Var_B_upr}.
#' @export
#'
#' @examples print(1)

estimate_imprecision_data <- function(data,
                                      B = 2e3L,
                                      type = "percentile",
                                      level = 0.95,
                                      invalid_NA = FALSE){

  # Bind global variables
  bootstrap_ci <- BCa_bootstrap_ci <- global_precision_estimates <- NULL
  leave_one_out <- resample_samples <- SampleID <- NULL
  ReplicateID <- comparison <- NULL

  # Validate 'data'
  data <- validate_data_eid(data)

  # Convert type to integer (because it is used in bootstrap_ci)
  type_numeric <- switch(type, "percentile" = 3, "basic" = 2, "normal" = 1, "BCa" = 4, "bca" = 4, 3)

  # Calculate original imprecision estimates
  orig_imps <- data[, global_precision_estimates(.SD), by = comparison]

  # Validate 'B'
  B_validation <- validate_B_eid(B, invalid_NA)

  if(!is.null(B_validation$B_message_type) && B_validation$B_message_type == "error"){
    stop(B_validation$B_message)
  }
  else if(!is.null(B_validation$B_message_type) && B_validation$B_message_type == "warning"){
    warning(B_validation$B_message)
    if(!is.null(B_validation$B_action) && B_validation$B_action == "return_orig_imps"){
      return(orig_imps)
    }
    else if(!is.null(B_validation$B_action) && B_validation$B_action == "return_NA_imps"){
      orig_imps$Var_A <- NA_real_
      orig_imps$Var_B <- NA_real_
      orig_imps$CV_A <- NA_real_
      orig_imps$CV_B <- NA_real_
      orig_imps$lambda <- NA_real_
      return(orig_imps)
    }
  }

  # A valid value of B
  B <- B_validation$B

  # The output
  out <- data[, estimate_imp_boot_ci(B, .SD, type_numeric, level),
              by = comparison]
  setcolorder(x = out, neworder = c("comparison","CV_A", "CV_A_lwr", "CV_A_upr", "CV_B", "CV_B_lwr", "CV_B_upr", "lambda", "lambda_lwr", "lambda_upr", "Var_A", "Var_A_lwr", "Var_A_upr", "Var_B", "Var_B_lwr", "Var_B_upr"))

  return(out)
}

