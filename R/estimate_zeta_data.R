#' All calculations regarding zeta
#'
#' @param data A \code{data.table}, \code{data.frame}, or \code{list}. It represents clinical sample data, grouped by 'comparison' (IVD-MD comparisons). The data structure should be arranged by 'comparison' and must include the following columns: \code{comparison}, \code{SampleID}, \code{ReplicateID}, \code{MP_A}, and \code{MP_B}.
#' @param B An \code{integer} value greater than or equal to 50. It represents the number of bootstrap replicates for estimating bootstrap confidence intervals. The default value is set at 2e3L (i.e., 2,000), a typical setting for bootstrap confidence intervals. Assign \code{B} to \code{NULL} or \code{NA} to skip estimating bootstrap confidence intervals and return only the point estimates of zeta.
#' @param type A \code{character} string that specifies the type of bootstrap confidence interval. Four types are supported:
#' \itemize{
#'   \item{\code{normal}: }{Standard normal bootstrap confidence interval.}
#'   \item{\code{basic}: }{Basic bootstrap confidence interval.}
#'   \item{\code{percentile}: }{Percentile bootstrap confidence interval.}
#'   \item{\code{BCa}: }{Bias and skewness-corrected bootstrap confidence interval embedding the Jackknife (leave-one-out) method.}
#' }
#' @param level A \code{double} value between 0 and 1 representing the confidence level for the bootstrap confidence interval. The default level is set at 0.95 (95 percent).
#' @param M A \code{double} value equal to or larger than 0 that indicates what value of \code{M} (acceptable average relative increase in pointwise prediction interval widths due to DINS) should be used to simulate an upper zeta value when \code{zeta_critical} is not specified (i.e., is \code{NULL}, \code{NA}, or not numeric).
#' @param N A \code{integer} value equal to or larger than 1 that indicates the number of IVD-MD comparison-wise zeta values to be simulated to approximate \code{zeta_critical} when it is not specified (i.e., is \code{NULL}, \code{NA}, or not numeric). The default value is 1e4L (i.e., 10,000).
#' @param zeta_critical A \code{double} value. Its default value is \code{NA}, which triggers a Monte Carlo simulation based on your average study design across all IVD-MD comparisons to obtain a suitable upper zeta value. Both \code{M} and \code{N} must be specified for a valid upper limit determination.
#' @param invalid_NA A \code{logical} value. If set to TRUE, the function will return \code{NA} for zeta when encountering invalid input or computation errors instead of raising an error. While this is not generally recommended due to the risk of masking potential issues, it can be useful in certain scenarios where uninterrupted execution is a priority.
#'
#' @description This function processes the zeta data for each unique pair of IVD-MDs in the provided \code{data}. The output format is compatible with the \code{merge_results()} function of the \code{fasteqa} package, facilitating downstream operations.
#'
#' @return A \code{data.table} that contains the following columns: \code{comparison}, \code{zeta}, \code{lwr}, \code{upr}, \code{zeta_critical}, and \code{zeta_conclusion}.
#' @export
#'
#' @examples \dontrun{
#'   library(commutability)
#'   print(1)
#' }

estimate_zeta_data <- function(data, B = 2e3L, type = "percentile", level = 0.95, M = 0, N = 1e4L, zeta_critical = NA, invalid_NA = FALSE){

  # Binding global variables
  comparison <- SampleID <- ReplicateID <- NULL
  resample_samples <- estimate_zeta <- bootstrap_ci <- leave_one_out <- BCa_bootstrap_ci <- simulate_eqa_data <- NULL

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
    }
    else{
      stop(sprintf("'invalid_NA' must be a non-missing logical value or an integer (0L for FALSE, 1L for TRUE). Current value and type of 'invalid_NA' is: [%s] '%s'",
                   invalid_NA,
                   typeof(invalid_NA)))
    }
  }

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

  data_list <- split(data, by = "comparison")
  original_zetas <- data[, estimate_zeta(.SD), by = comparison]

  # Checks if 'B' is 'NULL' or 'NA'. If this is the case, we do not return estimated bootstrap confidence intervals
  if(is.null(B) || length(B) == 0 || is.na(B)){
    return(original_zetas)
  }

  # Checks if 'B' is a character string. If this is the case, we return NA or throw an error depending on the 'invalid_NA' input
  if(isTRUE(is.character(B))){
    if(isTRUE(invalid_NA)){
      original_zetas$zeta <- NA_real_
      return(original_zetas)
    }
    else{
      stop(sprintf("'B' ['%s'] is a character string. Make sure it is a integer >= 50 and try again.
                 \nIf you do not wish to estimate bootstrap confidence intervals for zeta, set 'B' to either 'NULL' or 'NA'.",
                 B))
    }
  }

  # Checks if 'B' is positive. If this is not the case, return NA or throw an error depending on the 'invalid_NA' input
  B_is_positive <- B >= 0
  if(isFALSE(B_is_positive)){
    if(isTRUE(invalid_NA)){
      original_zetas$zeta <- NA_real_
      return(original_zetas)
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
  }

  if(B < 50L && B >= 1L){
    warning(sprintf("Ideally, 'B' should exceed 2,000 to ensure stable bootstrap confidence intervals for zeta in IVD-MD comparisons.
                 \nWith the current 'B' value [%d], which is less than 50, the confidence intervals might be quite unstable. Due to the low 'B' value, no bootstrap confidence intervals are estimated. Increase 'B' above 50 to enable confidence interval estimation.",
                 B), immediate. = TRUE)

    return(original_zetas)

  }
  else if(B < 1L){
    return(original_zetas)
  }

  # Estimation of confidence intervals of zeta depending on the input of 'type' and 'level'
  original_zetas_list <- split(original_zetas, by = "comparison", keep.by = FALSE)
  original_zetas_list <- lapply(original_zetas_list, FUN = function(x) x$zeta)
  resampled_data <- lapply(X = data_list, FUN = function(x) replicate(n = B, expr = resample_samples(data = x, silence = 1), simplify = FALSE))
  bootstrapped_zetas <- lapply(X = resampled_data, FUN = function(x) unlist(lapply(x, function(y) estimate_zeta(y)$zeta)))
  if(type == "normal"){
    bootstrap_cis <- mapply(FUN = function(x, y) bootstrap_ci(x, y, 1, level), bootstrapped_zetas, original_zetas_list, SIMPLIFY = FALSE)
  }
  else if(type == "basic"){
    bootstrap_cis <- mapply(FUN = function(x, y) bootstrap_ci(x, y, 2, level), bootstrapped_zetas, original_zetas_list, SIMPLIFY = FALSE)
  }
  else if(type == "percentile"){
    bootstrap_cis <- mapply(FUN = function(x, y) bootstrap_ci(x, y, 3, level), bootstrapped_zetas, original_zetas_list, SIMPLIFY = FALSE)
  }
  else if(type == "BCa"){
    loo_data <- lapply(X = data_list, FUN = function(x) sapply(1:length(unique(x$SampleID)), FUN = function(y) leave_one_out(x, y), simplify = FALSE))
    loo_zetas <- lapply(X = loo_data, FUN = function(x) unlist(lapply(x, function(y) estimate_zeta(y)$zeta)))
    bootstrap_cis <- mapply(FUN = function(x, y, z) BCa_bootstrap_ci(x, y, z, level), bootstrapped_zetas, loo_zetas, original_zetas_list, SIMPLIFY = FALSE)
  }
  else{
    bootstrap_cis <- mapply(FUN = function(x, y) bootstrap_ci(x, y, 3, level), bootstrapped_zetas, original_zetas_list, SIMPLIFY = FALSE)
  }

  if(is.null(zeta_critical) || (!is.numeric(zeta_critical)) || is.na(zeta_critical)){

    # Checks if 'N' is 'NULL' or 'NA', which is not allowed if 'zeta_critical' is 'NULL' or 'NA'
    if(is.null(N) || length(N) == 0 || is.na(N)){
      if(isTRUE(invalid_NA)){
        original_zetas$zeta <- NA_real_
        return(original_zetas)
      }
      else{
        stop(sprintf("The input 'N' [%s] must be a positive non-zero integer, but a '%s' was provided. Please ensure 'N' is an integer equal to or greater than 1 and retry.
                     \nIf there is no need to simulate upper zeta values, assign 'zeta_critical' a positive numeric value, not '%s'.",
                     ifelse(is.null(N) || length(N) == 0, "NULL", N),
                     class(N),
                     ifelse(is.null(zeta_critical) || length(zeta_critical) == 0, "NULL", zeta_critical)))

      }
    }

    # Checks if 'N' is a characer string, which is not allowed if 'zeta_critical' is 'NULL' or 'NA'
    if(isTRUE(is.character(N))){
      if(isTRUE(invalid_NA)){
        original_zetas$zeta <- NA_real_
        return(original_zetas)
      }
      else{
        stop(sprintf("'N' ['%s'] is a character string. Make sure it is a integer equal to or larger than 1 and try again.
                 \nIf there is no need to simulate upper zeta values, assign 'zeta_critical' a positive numeric value, not '%s'.",
                 N,
                 ifelse(is.null(zeta_critical) || length(zeta_critical) == 0, "NULL", zeta_critical)))
      }
    }

    # Checks if 'N' is positive. If this is not the case, return 'NA' or throw an error depending on the 'invalid_NA' input
    N_is_positive <- N > 0
    if(isFALSE(N_is_positive) && !is.logical(N)){
      if(isTRUE(invalid_NA)){
        original_zetas$zeta <- NA_real_
        return(original_zetas)
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
      }
      else if(N == 0.5){
        N <- 1L
      }
      else{
        N <- round(N)
      }
    }

    # Checks if 'N' is either TRUE or FALSE
    N_is_logical <- is.logical(N)
    if(isTRUE(N_is_logical)){
      if(isTRUE(invalid_NA)){
        original_zetas$zeta <- NA_real_
        return(original_zetas)
      }
      else{
        stop(sprintf("Invalid input: 'N' [%s] is a logical value. Please ensure 'N' is an integer equal to or greater than 1, then try again.
              \nIf simulation of upper zeta values is not required, assign a positive numeric value to 'zeta_critical', not '%s'.",
              as.character(N),
              zeta_critical))

      }
    }

    # Checks if 'M' is a vector. If so, keep the first element of the vector
    if(length(M) > 1L){
      M <- M[1]
    }
    # Checks if 'M' is a numeric value equal to or larger than 0.
    M_is_valid <- !(!is.numeric(M) || is.na(M) || is.null(M) || M < 0)
    if(isFALSE(M_is_valid)){
      if(invalid_NA){
        original_zetas$zeta <- NA_real_
        return(original_zetas)
      }
      else{
        stop(sprintf("Invalid input for 'M'. You entered a(n) '%s' [%s]. Please provide a numeric value equal to or larger than 0.",
                     class(M),
                     deparse(substitute(M))))
      }
    }

    # At a later stage we could also pool the simulated zeta values for those IVD-MD comparisons having the same values of n and R, but not for those
    # IVD-MD comparisons having different n and R. This is not prioritized to do at the moment, but should be considered in a later stage

    simulation_parameters <- lapply(X = data_list, FUN = function(x) list(n = length(unique(x$SampleID[!is.na(x$SampleID)])), R = count_samplewise_replicates(x, "round")$R_i))
    simulated_zetas <- lapply(X = simulation_parameters, FUN = function(x) replicate(n = N, expr = estimate_zeta(simulate_eqa_data(x))$zeta, simplify = TRUE))
    simulated_zetas_scaled <- lapply(X = simulated_zetas, FUN = function(x) x * (1 + M) ** 2)
    identical_simulation_parameters <- all(sapply(simulation_parameters, function(i) identical(i, simulation_parameters[[1]])))
    if(isTRUE(identical_simulation_parameters)){
      zeta_critical <- round(quantile(unlist(simulated_zetas_scaled, use.names = FALSE), na.rm = TRUE, probs = 0.99, names = FALSE), 2L)
      zeta_criticals <- lapply(X = data_list, FUN = function(x) zeta_critical)
    }
    else{
      simulated_zeta_criticals <- lapply(X = simulated_zetas_scaled, FUN = function(x) round(quantile(x, na.rm = TRUE, probs = 0.99, names = FALSE), 2L))
      zeta_criticals <- simulated_zeta_criticals
    }
  }
  else{
    zeta_criticals <- lapply(X = data_list, FUN = function(x) zeta_critical)
  }
  zeta_conclusion <- mapply(FUN = function(x, y) as.integer(x >= y), original_zetas_list, zeta_criticals, SIMPLIFY = FALSE)
  out <- mapply(FUN = function(x, y, z, w) list("zeta" = x, "lwr" = max(0, y[1]), "upr" = max(y[2], 0), "zeta_critical" = z, "zeta_conclusion" = w),
                original_zetas_list, bootstrap_cis, zeta_criticals, zeta_conclusion, SIMPLIFY = FALSE)
  out <- rbindlist(l = out, idcol = "comparison")
  return(out)

}

