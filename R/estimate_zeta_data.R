#' Internal Function for Checking Input Data
#'
#' This function is used internally for checking and possibly converting the
#' input of \code{estimate_zeta_data} to correct format.
#' @keywords internal
check_estimate_zeta_data <- function(data,
                                     B = 2e3L,
                                     type = "percentile",
                                     level = 0.95,
                                     M = 0,
                                     N = 1e4L,
                                     zeta_critical = NA,
                                     method = c("ols", "ss", "ssw"),
                                     invalid_NA = FALSE){

  # Bind global variables
  SampleID <- ReplicateID <- NULL

  corrected_inputs <- list(data = data,
                           B = B,
                           type = type,
                           level = level,
                           M = M,
                           N = N,
                           zeta_critical = zeta_critical,
                           method = method,
                           invalid_NA = invalid_NA)

  # Check 'data':

  # Checks if 'data' is of correct class
  if(!is.data.table(data)){
    if(is.data.frame(data) | is.list(data)){
      setDT(data)
    }
    else{
      stop("The input 'data' is neither a data.table, list, nor data.frame.
            \nRegistered input class of 'data': '", class(data)[1], "'.
            \nPlease provide an input of type data.table, list, or data.frame to proceed. The calculations cannot continue with the current input type.")

    }
  }

  # Checks if 'data' have the required columns
  required_columns <- c('comparison', 'SampleID', 'ReplicateID', 'MP_A', 'MP_B')
  missing_columns <- setdiff(required_columns, names(data))
  if(length(missing_columns) > 0){
    stop(paste0("Some required columns are missing from 'data':", "\n",
                "* Missing column(s): [",
                paste(missing_columns, collapse=", "),
                "]", "\n",
                "* The argument 'data' must include 'comparison', 'SampleID', 'ReplicateID', 'MP_A' and 'MP_B'."))
  }

  # Checks if 'SampleID' and 'ReplicateID' are character vectors
  # If they are not, they will be converted to character vectors
  if(isFALSE(is.character(data$SampleID))){
    data[, SampleID := as.character(SampleID)]
  }
  if(isFALSE(is.character(data$ReplicateID))){
    data[, ReplicateID := as.character(ReplicateID)]
  }

  # Input data is OK
  corrected_inputs$data <- data

  # Check 'level'
  if(is.null(level) || (!is.numeric(level)) || is.na(level) || level > 1 || level < 0){
    corrected_inputs$level <- 0.95
  }

  # Check 'type'
  if(is.null(type) || (!is.character(type))){
    corrected_inputs$type <- "percentile"
  }

  else if(length(type) > 1){
    corrected_inputs$type <- type[1]
    if(is.na(corrected_inputs$type)){
      corrected_inputs$type <- "percentile"
    }
  }

  # Check 'method'
  if(is.null(method) || (!is.character(method))){
    corrected_inputs$method <- "ols"
  }

  else if(length(method) > 1){
    corrected_inputs$method <- method[1]
    if(is.na(corrected_inputs$method)){
      corrected_inputs$method <- "ols"
    }
  }

  # Check 'invalid_NA':

  # Checks if 'invalid_NA' is NULL
  if(is.null(invalid_NA)){
    stop(sprintf("'invalid_NA' must be a non-missing logical value or an integer (0L for FALSE, 1L for TRUE). Current value and type of 'invalid_NA' is: [%s] '%s'",
                 invalid_NA,
                 typeof(invalid_NA)))
  }

  # Checks if 'invalid_NA' is a missing value
  else if(is.na(invalid_NA)){
    stop(sprintf("'invalid_NA' must be a non-missing logical value or an integer (0L for FALSE, 1L for TRUE). Current value and type of 'invalid_NA' is: [%s] '%s'",
                 invalid_NA,
                 typeof(invalid_NA)))
  }

  # Checks if 'invalid_NA' is a non-missing logical value or an integer representation of a logical value
  else if(!is.logical(invalid_NA)){
    if(is.integer(invalid_NA) && (invalid_NA == 0L || invalid_NA == 1L)){
      invalid_NA <- as.logical(invalid_NA)
      corrected_inputs$invalid_NA <- invalid_NA
    }
    else{
      stop(sprintf("'invalid_NA' must be a non-missing logical value or an integer (0L for FALSE, 1L for TRUE). Current value and type of 'invalid_NA' is: [%s] '%s'",
                   invalid_NA,
                   typeof(invalid_NA)))
    }
  }

  # Input 'invalid_NA' is OK
  corrected_inputs$invalid_NA <- invalid_NA

  # Check 'B'

  # Checks if 'B' is 'NULL' or 'NA'. If this is the case, we do not return estimated bootstrap confidence intervals
  if(is.null(B) || length(B) == 0 || is.na(B)){
    corrected_inputs$B <- "return_original_zetas"
  }

  else if(!is.numeric(B)){
    # Checks if 'B' is a character string. If this is the case, we return NA or throw an error depending on the 'invalid_NA' input
    if(isTRUE(is.character(B))){
      if(isTRUE(invalid_NA)){
        corrected_inputs$B <- "return_NA_zetas"
      }
      else{
        stop(sprintf("'B' ['%s'] is a character string. Make sure it is a integer >= 50 and try again.
                 \nIf you do not wish to estimate bootstrap confidence intervals for zeta, set 'B' to either 'NULL' or 'NA'.",
                     B))
      }
    }
    else{
      if(isTRUE(invalid_NA)){
        corrected_inputs$B <- "return_NA_zetas"
      }
      else{
        stop(sprintf("'B' is not an integer. Make sure it is a integer >= 50 and try again. If you do not wish to estimate bootstrap confidence intervals for zeta, set 'B' to either 'NULL' or 'NA'."))
      }
    }
  }

  else {
    # Checks if 'B' is positive. If this is not the case, return NA or throw an error depending on the 'invalid_NA' input
    B_is_positive <- B >= 0
    if(isFALSE(B_is_positive)){
      if(isTRUE(invalid_NA)){
        corrected_inputs$B <- "return_NA_zetas"
      }
      else{
        stop(sprintf("'B' [%d] is a negative value. Make sure it is a integer >= 50 and try again.
                 \nIf you do not wish to estimate bootstrap confidence intervals for zeta, set 'B' to either 'NULL' or 'NA'.",
                     B))
      }
    }

    # Checks if 'B' is an integer. If this is not the case, round it to the nearest integer
    B_is_integer <- abs(B - round(B)) < .Machine$double.eps ** 0.5
    if(isFALSE(B_is_integer)){
      B <- round(B)
      corrected_inputs$B <- B
    }

    if(B < 50L && B >= 1L){
      warning(sprintf("Ideally, 'B' should exceed 2,000 to ensure stable bootstrap confidence intervals for zeta in IVD-MD comparisons.
                 \nWith the current 'B' value [%d], which is less than 50, the confidence intervals might be quite unstable. Due to the low 'B' value, no bootstrap confidence intervals are estimated. Increase 'B' above 50 to enable confidence interval estimation.",
                      B), immediate. = TRUE)

      corrected_inputs$B <- "return_original_zetas"

    }
    else if(B < 1L){
      corrected_inputs$B <- "return_original_zetas"
    }
  }



  # Check 'zeta_critical':

  simulate_zeta_critical <- FALSE

  # Check if NULL / Not numeric / NA
  if(is.null(zeta_critical) || (!is.numeric(zeta_critical)) || is.na(zeta_critical)){
    corrected_inputs$zeta_critical <- "simulate_zeta_critical"
    simulate_zeta_critical <- TRUE
  }

  # 'N' and 'M' are only checked if simulate_zeta_critical == TRUE
  if(simulate_zeta_critical){

    # Check 'N'

    # Checks if 'N' is 'NULL' or 'NA', which is not allowed if 'zeta_critical' is 'NULL' or 'NA'
    if(is.null(N) || length(N) == 0 || is.na(N)){
      if(isTRUE(invalid_NA)){
        corrected_inputs$N <- "return_NA_zetas"
      }
      else{
        stop(sprintf("The input 'N' [%s] must be a positive non-zero integer, but a '%s' was provided. Please ensure 'N' is an integer equal to or greater than 1 and retry.
                     \nIf there is no need to simulate upper zeta values, assign 'zeta_critical' a positive numeric value, not '%s'.",
                     ifelse(is.null(N) || length(N) == 0, "NULL", N),
                     class(N),
                     ifelse(is.null(zeta_critical) || length(zeta_critical) == 0, "NULL", zeta_critical)))

      }
    }

    else if(!is.numeric(N)){

      # Checks if 'N' is a characer string, which is not allowed if 'zeta_critical' is 'NULL' or 'NA'
      if(isTRUE(is.character(N))){
        if(isTRUE(invalid_NA)){
          corrected_inputs$N <- "return_NA_zetas"
        }
        else{
          stop(sprintf("'N' ['%s'] is a character string. Make sure it is a integer equal to or larger than 1 and try again.
                 \nIf there is no need to simulate upper zeta values, assign 'zeta_critical' a positive numeric value, not '%s'.",
                       N,
                       ifelse(is.null(zeta_critical) || length(zeta_critical) == 0, "NULL", zeta_critical)))
        }
      }

      else {
        if(isTRUE(invalid_NA)){
          corrected_inputs$N <- "return_NA_zetas"
        }
        else{
          stop(sprintf("'N' is not an integer. Make sure it is a integer equal to or larger than 1 and try again.
                 \nIf there is no need to simulate upper zeta values, assign 'zeta_critical' a positive numeric value.'."))
        }
      }
    }

    else {

      # Checks if 'N' is positive. If this is not the case, return 'NA' or throw an error depending on the 'invalid_NA' input
      N_is_positive <- N > 0
      if(isFALSE(N_is_positive) && !is.logical(N)){
        if(isTRUE(invalid_NA)){
          corrected_inputs$N <- "return_NA_zetas"
        }
        else{
          stop(sprintf("Invalid input: 'N' [%d] is a negative value or zero. Please ensure 'N' is an integer equal to or greater than 1, then try again.
              \nIf simulation of upper zeta values is not required, assign a positive numeric value to 'zeta_critical', not '%s'.",
                       N,
                       zeta_critical))

        }
      }

      # Checks if 'N' is an integer. If this is not the case, round it to the nearest integer
      N_is_integer <- abs(N - round(N)) < .Machine$double.eps ** 0.5
      if(isFALSE(N_is_integer)){
        if(N < 0.5){
          warning(sprintf("The value of 'N' [%s] was provided as a decimal less than 0.5 and was rounded to the nearest integer: %s.
                \nPlease note, 'N' should represent the count of zeta simulations for each IVD-MD comparison, used to estimate the specific upper zeta values.
                \nThe rounded 'N' value [%s] is not suitable for this purpose. Thus, 'N' will be set to the closest valid integer: 1L.",
                          N,
                          round(N),
                          round(N)), noBreaks. = TRUE, immediate. = TRUE)
          N <- 1L
          corrected_inputs$N <- N
        }
        else if(N == 0.5){
          N <- 1L
          corrected_inputs$N <- N
        }
        else{
          N <- round(N)
          corrected_inputs$N <- N
        }
      }

      # Checks if 'N' is either TRUE or FALSE
      N_is_logical <- is.logical(N)
      if(isTRUE(N_is_logical)){
        if(isTRUE(invalid_NA)){
          corrected_inputs$N <- "return_NA_zetas"
        }
        else{
          stop(sprintf("Invalid input: 'N' [%s] is a logical value. Please ensure 'N' is an integer equal to or greater than 1, then try again.
              \nIf simulation of upper zeta values is not required, assign a positive numeric value to 'zeta_critical', not '%s'.",
                       as.character(N),
                       zeta_critical))

        }
      }
    }





    # Checks if 'M' is a vector. If so, keep the first element of the vector
    if(length(M) > 1L){
      M <- M[1]
      corrected_inputs$M <- M
    }

    # Checks if 'M' is a numeric value equal to or larger than 0.
    M_is_valid <- !(!is.numeric(M) || is.na(M) || is.null(M) || M <= 0)
    if(isFALSE(M_is_valid)){
      if(invalid_NA){
        corrected_inputs$N <- "return_NA_zetas"
      }
      else{
        stop(sprintf("Invalid input for 'M'. You entered a(n) '%s' [%s]. Please provide a numeric value equal to or larger than 0.",
                     class(M),
                     deparse(substitute(M))))
      }
    }
  }
  else{
    corrected_inputs$N <- "ignore"
    corrected_inputs$M <- "ignore"
  }

  return(corrected_inputs)


}

#' Internal Function for Calculating Boostrap Confidence Intervals
#'
#' This function is used internally for Calculating Boostrap Confidence
#' Intervals in \code{estimate_zeta_data}
#' @keywords internal
estimate_zeta_boot_ci <- function(data_list,
                                  original_zetas,
                                  original_dfs = NULL,
                                  B = B,
                                  method,
                                  type,
                                  level,
                                  buffer = 0.05) {

  # Get comparison names
  comparisons <- names(original_zetas)

  # Convert data.table zeta data to list zeta
  original_zetas_list <- split(original_zetas,
                               by = "comparison",
                               keep.by = FALSE)
  original_zetas_list <- lapply(original_zetas_list,
                                FUN = function(x) x$zeta)

  # Resample zetas with extra security
  bootstrapped_zetas <- sapply(X = seq_len(length(data_list)),
                               FUN = function(comp) {
                                 if (method %in% c("ss", "ssw")) {
                                   comp_data <- data_list[[comp]]
                                   comp_df <- ifelse(original_dfs < 2.2, 2, original_dfs)[comp]
                                   resampled_zetas <- replicate(
                                     n = ceiling(B * (1 + buffer)),
                                     expr = tryCatch(
                                       expr = {
                                         estimate_zeta_ss(data = resample_samples(data = comp_data),
                                                          df = comp_df,
                                                          weighted = method == "ssw",
                                                          mor = FALSE,
                                                          na_rm = TRUE)$zeta
                                       },
                                       warning = function(w) NA_real_,
                                       error = function(e) NA_real_
                                     ),
                                     simplify = TRUE
                                    )
                                   resampled_zetas <- resampled_zetas[!is.na(resampled_zetas)]
                                   if(length(resampled_zetas) > B){
                                     resampled_zetas <- resampled_zetas[seq_len(B)]
                                   }
                                   return(resampled_zetas)
                                 }
                                 else {
                                   comp_data <- data_list[[comp]]
                                   resampled_zetas <- replicate(
                                     n = ceiling(B * (1 + buffer)),
                                     expr = tryCatch(
                                       expr = {
                                         resample_zeta_estimates(data = comp_data)$zeta
                                       },
                                       warning = function(w) NA_real_,
                                       error = function(e) NA_real_
                                     ),
                                     simplify = TRUE
                                   )
                                   resampled_zetas <- resampled_zetas[!is.na(resampled_zetas)]
                                   if(length(resampled_zetas) > B){
                                     resampled_zetas <- resampled_zetas[seq_len(B)]
                                   }
                                   return(resampled_zetas)
                                 }
                               },
                               simplify = FALSE)

  # Get leave-one-out zetas
  loo_zetas <- sapply(X = seq_len(length(data_list)),
                      FUN = function(comp) {
                        SampleID_integer_IDs <- seq_len(length(unique(data_list[[comp]]$SampleID)))
                        if (method %in% c("ss", "ssw")) {
                          comp_loo_zetas <- sapply(X = SampleID_integer_IDs,
                                                   FUN = function(sampl) {
                                                     loo_zeta <- tryCatch(
                                                       expr = {
                                                         estimate_zeta_ss(
                                                           data = leave_one_out(data = data_list[[comp]],
                                                                                loo_id = sampl),
                                                           df = ifelse(original_dfs < 2.2, 2, original_dfs)[comp],
                                                           weighted = method == "ssw",
                                                           mor = FALSE,
                                                           na_rm = TRUE)$zeta
                                                       },
                                                       warning = function(w) NA_real_,
                                                       error = function(e) NA_real_
                                                     )
                                                     return(loo_zeta)
                                                   },
                                                   simplify = TRUE)

                          comp_loo_zetas <- comp_loo_zetas[!is.na(comp_loo_zetas)]
                          return(comp_loo_zetas)
                        }
                        else {
                          comp_loo_zetas <- sapply(X = SampleID_integer_IDs,
                                                   FUN = function(sampl) {
                                                     loo_zeta <- tryCatch(
                                                       expr = {
                                                         estimate_zeta_ols(
                                                           data = leave_one_out(data = data_list[[comp]],
                                                                                loo_id = sampl))$zeta
                                                       },
                                                       warning = function(w) NA_real_,
                                                       error = function(e) NA_real_
                                                     )
                                                     return(loo_zeta)
                                                   },
                                                   simplify = TRUE)

                          comp_loo_zetas <- comp_loo_zetas[!is.na(comp_loo_zetas)]
                          return(comp_loo_zetas)
                        }
                      },
                      simplify = FALSE)

  if(length(type) > 1) {
    type <- type[1]
  }

  type_integer <- switch(type,
                         "normal" = 1L,
                         "basic" = 2L,
                         "percentile" = 3L,
                         "BCa" = 4L,
                         5L)

  if(type_integer < 5L) {
    bootstrap_cis <- sapply(
      X = seq_len(length(data_list)),
      FUN = function(comp) {
        comp_bootstrap_ci <- tryCatch(
          expr = {
            bootstrap_ci(
              bootstrapped_parameter_estimates = bootstrapped_zetas[[comp]],
              jackknife_parameter_estimates = loo_zetas[[comp]],
              original_parameter_estimate = original_zetas_list[[comp]],
              type = type_integer,
              level = level)
          },
          warning = function(w) c(NA_real_, NA_real_),
          error = function(w) c(NA_real_, NA_real_)
        )
      },
      simplify = FALSE
    )
  }
  else {
    warning(
      "Input type is invalid. Uses 'percentile' as fallback."
    )
    bootstrap_cis <- sapply(
      X = seq_len(length(data_list)),
      FUN = function(comp) {
        comp_bootstrap_ci <- tryCatch(
          expr = {
            bootstrap_ci(
              bootstrapped_parameter_estimates = bootstrapped_zetas[[comp]],
              jackknife_parameter_estimates = loo_zetas[[comp]],
              original_parameter_estimate = original_zetas_list[[comp]],
              type = 3L,
              level = level)
          },
          warning = function(w) c(NA_real_, NA_real_),
          error = function(w) c(NA_real_, NA_real_)
        )
      },
      simplify = FALSE
    )
  }

  names(bootstrap_cis) <- comparisons

  return(bootstrap_cis)

}

#' Internal Function for Approximating Critical Values of \eqn{\hat{\zeta}}
#'
#' This function is used internally for approximating critical values of
#' \eqn{\hat{\zeta}}, using Monte Carlo simulations. Not meant to be used by
#' end-users.
#' @keywords internal
approximate_upper_zeta <- function(data_list, N, M) {

  # Get (n, R, cvx, cvy, cil, ciu) for each IVD-MD comparison
  approximated_upper_zetas <- lapply(X = data_list,
                                     FUN = function(comp) {
                                       impr_estimates <- global_precision_estimates(comp)
                                       pseudo_parameters <- list(
                                         n = length(unique(comp$SampleID[!is.na(comp$SampleID)])),
                                         R = count_samplewise_replicates(comp, "round")$R_i,
                                         cvx = impr_estimates$CV_B,
                                         cvy = impr_estimates$CV_A,
                                         cil = min(0.5 * comp$MP_A + 0.5 * comp$MP_B, na.rm = TRUE),
                                         ciu = max(0.5 * comp$MP_A + 0.5 * comp$MP_B, na.rm = TRUE),
                                         b0 = 0,
                                         b1 = 1
                                       )
                                       comp_simulated_zetas <- replicate(
                                         n = N,
                                         expr = {
                                           estimate_zeta_ols(
                                             data = sim_eqa_data(
                                               parameters = pseudo_parameters,
                                               type = 0L,
                                               AR = TRUE,
                                               include_parameters = FALSE
                                             )
                                           )$zeta
                                         },
                                         simplify = TRUE
                                       )
                                       comp_99th_percentile_zeta0 <- quantile(
                                         x = comp_simulated_zetas,
                                         probs = 0.99,
                                         names = FALSE,
                                         na.rm = TRUE
                                       )

                                       comp_99th_percentile_zeta <- round(
                                         x = comp_99th_percentile_zeta0 * (1 + M) ** 2,
                                         digits = 2L
                                       )

                                       return(comp_99th_percentile_zeta)
                                     })

  # Define zeta_criticals
  return(approximated_upper_zetas)
}


#' @title
#' All Necessary Inference On \eqn{\hat{\zeta}}
#'
#' @param data A \code{data.table}, \code{data.frame}, or \code{list}.
#'             The CS data. Must contain the following variables:
#'             \itemize{
#'                \item \code{comparison: } A \code{character} vector. The
#'                      comparison identifiers. Typically on the form
#'                      \code{'MP_A - MP_B'}.
#'                \item \code{SampleID: } A \code{character} vector. The sample
#'                      identifiers for the clinical samples.
#'                \item \code{ReplicateID: } A \code{character} vector. The
#'                      replicated measurement identifiers.
#'                \item \code{MP_A: } A \code{numeric} vector. The observed
#'                      measurement results from IVD-MD \code{MP_A} (response).
#'                \item \code{MP_B: } A \code{numeric} vector. The observed
#'                      measurement results from IVD-MD \code{MP_A} (predictor).
#'             }
#' @param B An \code{integer}. Must be larger than or equal to \code{50}. The
#'          Number of bootstrap replicates used to estimate bootstrap
#'          confidence intervals for \eqn{\zeta}. Defaults to \code{2e3L}
#'          (2,000). If \code{NULL}, estimated confidence intervals are not
#'          calculated.
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
#' @param M A \code{double}. Must be larger than \code{0}. The maximum
#'          acceptable relative increase in pointwise prediction interval width
#'          attribute to differences in non-selectivity. Relevant if
#'          \code{zeta_critical} is \code{NA} or \code{NULL}. Defaults to
#'          \code{0}.
#' @param N An \code{integer}. Must be larger than \code{100L}. The number of
#'          simulated \eqn{\hat{\zeta}} values used to approximate
#'          \code{zeta_critical} values. Only relevant if \code{zeta_critical}
#'          is \code{NA} or \code{NULL}. Defaults to \code{1e4L}
#' @param zeta_critical A \code{double}. The maximum value \eqn{\hat{\zeta}}
#'                      can take before an IVD-MD is considered to have
#'                      excessive differences in non-selectivity. If \code{NA}
#'                      (default) or \code{NULL}, \code{zeta_critical} is
#'                      approximated using Monte Carlo simulations.
#' @param method A \code{character} string. The regression method used to
#'               estimate \eqn{\zeta}. Can be \code{'ols'}, \code{'ss'} or
#'               \code{'ssw'}.
#' @param invalid_NA \code{logical} value. If \code{TRUE} (not recommended),
#'                   return \code{NA} rather than throwing an error if
#'                   something goes wrong.
#'
#' @description
#' Obtains all necessary information on \eqn{\hat{\zeta}} for each
#' unique IVD-MD pair (\code{comparison}) in \code{data}.
#'
#' @details
#' A wrapper function for calculating relevant statistics on \eqn{\zeta}.
#' This function is an important part of \code{do_commutability_evaluation()}.
#'
#' @return
#' A \code{data.table}. Contains the following columns:
#' \itemize{
#'  \item \code{comparison: } See \code{data}.
#'  \item \code{zeta: } A \code{double}. Point estimate of \eqn{\zeta}.
#'  \item \code{lwr: } A \code{double}. Lower boundary of calculated bootstrap
#'        confidence interval of \eqn{\zeta}.
#'  \item \code{upr: } A \code{double}. Upper boundary of calculated bootstrap
#'        confidence interval of \eqn{\zeta}.
#'  \item \code{zeta_critical: } A \code{double}. Always rounded to \code{2}
#'        decimals. Either the value given in \code{zeta_critical} or
#'        approximated values. The utilized maximum value \eqn{\hat{\zeta}} can
#'        take before a IVD-MD pair is deemed to have excessive differences in
#'        non-selectivity.
#'  \item \code{zeta_conclusion: } An \code{integer}. Equivalent to
#'        \code{dins_conclusion} used elsewhere. It is \code{1} if
#'        \code{zeta} > \code{zeta_critical}. Otherwise, \code{0}.
#' }
#'
#' @export
#'
#' @examples
#' # Required packages
#' library(smooth.commutability)
#' library(fasteqa)
#' library(data.table)
#'
#' # Reproducibility
#' set.seed(99)
#'
#' # Get example data from smooth.commutability package
#' test_crp_cs_data <- copy(crp_cs_data)
#'
#' # Calculate inference data on zeta
#' estimated_zeta_data <- estimate_zeta_data(data = test_crp_cs_data,
#'                                           B = 500L,
#'                                           type = "BCa",
#'                                           level = 0.95,
#'                                           zeta_critical = 3.24,
#'                                           method = "ols",
#'                                           invalid_NA = FALSE)
#' # Round results
#' estimated_zeta_data$zeta <- round(estimated_zeta_data$zeta, 2L)
#' estimated_zeta_data$lwr <- round(estimated_zeta_data$lwr, 2L)
#' estimated_zeta_data$upr <- round(estimated_zeta_data$upr, 2L)
#'
#' # Present results
#' print(estimated_zeta_data)
#'

estimate_zeta_data <- function(data,
                               B = 2e3L,
                               type = "percentile",
                               level = 0.95,
                               M = 0,
                               N = 1e4L,
                               zeta_critical = NA,
                               method = c("ols", "ss", "ssw"),
                               invalid_NA = FALSE){

  # Binding global variables
  comparison <- SampleID <- ReplicateID <- NULL
  resample_samples <- estimate_zeta_ols <- bootstrap_ci <- NULL
  leave_one_out <- BCa_bootstrap_ci <- simulate_eqa_data <- NULL

  # Check and get repaired input
  validity_checks_and_repaired_input <- check_estimate_zeta_data(data = data,
                                                                 B = B,
                                                                 type = type,
                                                                 level = level,
                                                                 M = M,
                                                                 N = N,
                                                                 zeta_critical = zeta_critical,
                                                                 method = method,
                                                                 invalid_NA = invalid_NA)

  # Extract repaired input
  data <- validity_checks_and_repaired_input$data
  B <- validity_checks_and_repaired_input$B
  type <- validity_checks_and_repaired_input$type
  level <- validity_checks_and_repaired_input$level
  M <- validity_checks_and_repaired_input$M
  N <- validity_checks_and_repaired_input$N
  zeta_critical <- validity_checks_and_repaired_input$zeta_critical
  method <- validity_checks_and_repaired_input$method
  invalid_NA <- validity_checks_and_repaired_input$invalid_NA

  # Split 'data' into a list grouped by 'comparison'
  data_list <- split(x = data,
                     by = "comparison",
                     keep.by = FALSE)

  if(method == "ols"){
    original_zetas <- data[, estimate_zeta_ols(.SD), by = comparison]
  }
  else if(method %in% c("ss", "ssw")){
    weights_action <- switch(method, "ss" = 1, "ssw" = "estimate", 1)
    weighted <- switch(method, "ss" = FALSE, "ssw" = TRUE, FALSE)
    original_dfs <- sapply(X = data_list,
                           FUN = function(dataset){
                             smoothing_spline(data = dataset[, fun_of_replicates(.SD)],
                                              weights = weights_action,
                                              df = NULL,
                                              df_max = 7.5,
                                              attempt_fast = FALSE,
                                              na_rm = TRUE)$df

                           },
                           simplify = TRUE)
    original_zetas <- estimate_zetas_ss(data = data,
                                        df = ifelse(original_dfs < 2.2, 2, original_dfs),
                                        weighted = weighted,
                                        na_rm = TRUE)

  }
  else {
    warning(
      "Input method was not among the valid options ('ols', 'ss', 'ssw').",
      "Using estimate_zeta_ols() as fallback."
    )
    original_zetas <- data[, estimate_zeta_ols(.SD), by = comparison]
  }


  # Check B, N and M for validity before moving on to bootstrapping
  if(B == "return_NA_zetas" || N == "return_NA_zetas" || M == "return_NA_zetas"){
    original_zetas$zeta <- NA_real_
    return(original_zetas)
  }
  else if(B == "return_original_zetas" || N == "return_original_zetas" || M == "return_original_zetas"){
    return(original_zetas)
  }

  bootstrap_cis <- estimate_zeta_boot_ci(data_list = data_list,
                                         original_zetas = original_zetas,
                                         original_dfs = original_dfs,
                                         B = B,
                                         method = method,
                                         type = type,
                                         level = level,
                                         buffer = 0.05)

  if(zeta_critical == "simulate_zeta_critical"){
   zeta_criticals <- approximate_upper_zeta(data_list = data_list,
                                            N = N,
                                            M = M)


  }
  else{
    zeta_criticals <- lapply(X = data_list,
                             FUN = function(x) zeta_critical)
  }

  # Get DINS conclusions
  zeta_conclusion <- sapply(X = seq_len(length(zeta_criticals)),
                            FUN = function(comp){
                              as.integer(original_zetas$zeta[comp] > zeta_criticals[[comp]])
                            },
                            simplify = FALSE)

  # Gather all results into one object
  out <- sapply(X = seq_len(length(zeta_criticals)),
                FUN = function(comp){
                  list("zeta" = original_zetas$zeta[comp],
                       "lwr" = max(0, bootstrap_cis[[comp]][1]),
                       "upr" = max(0, bootstrap_cis[[comp]][2]),
                       "zeta_critical" = zeta_criticals[[comp]],
                       "zeta_conclusion" = zeta_conclusion[[comp]])
                },
                simplify = FALSE)
  names(out) <- names(data_list)
  out <- rbindlist(l = out, idcol = "comparison")


  return(out)

}

