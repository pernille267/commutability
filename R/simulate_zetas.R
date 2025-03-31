#' Simulate a Point Estimate of \eqn{\zeta}
#'
#' @description
#' This function simulates \eqn{\hat{\zeta}} once based on \code{parameters}.
#' It is almost exclusively used internally within the more comprehensive
#' \code{simulate_zetas()} function.
#'
#' @param parameters A \code{list}, \code{data.table}, or \code{data.frame}.
#'                   The simulation parameters. See \code{?sim_eqa_data()} for
#'                   available options and general guidance.
#'
#' @param simulation_indices A \code{numeric} vector. Kept available for the
#'                           \code{boot()} method. See the \code{boot-package}.
#'
#' @details
#' Given the values found in \code{parameters}, one value of \eqn{\hat{\zeta}}
#' is simulated. This is typically not a function end-users should use directly
#' but rather a helper-function for the considerably more comprehensive and
#' useful \code{simulate_zetas()}. The second argument
#' \code{simulation_indices} have no actual function. It is required because
#' the \code{boot-package} requires it in its methods but it is never actually
#' used.
#'
#' @return
#' A \code{double}. The simulated \eqn{\hat{\zeta}} value.
#'
#' @export
#'
#' @examples
#' # Simulate a zeta value for a pair of IVD-MDs susceptible to random DINS
#' simulate_zeta(parameters = list(n = 25, R = 3, prop = 0.05, mmax = 5))
#'
#' # Simulate a zeta value for a pair of IVD-MDs susceptible to upper systematic DINS
#' simulate_zeta(parameters = list(n = 20, R = 4, qpos = 1, qran = 0.2, mmax = 5))

simulate_zeta <- function(parameters, simulation_indices){
  return(estimate_zeta_ols(data = sim_eqa_data(parameters = parameters,
                                               type = 0L,
                                               AR = TRUE))$zeta)
}

#' Monte Carlo Simulations of \eqn{\hat{\zeta}}
#'
#' @description
#' Simulate statistics or a sample of \eqn{\hat{\zeta}} values. In practice,
#' \code{n} \eqn{\hat{\zeta}} values are simulated \eqn{m} times, and for each
#' of the \code{m} samples, an individual or a set of statistics are estimated.
#'
#' @param n An \code{integer}. Must be larger than or equal to \code{1}. The
#'          desired number of simulated \eqn{\hat{\zeta}} values for each
#'          replication of the target \code{statistic}(s).
#' @param parameters A \code{list}, \code{data.table}, or \code{data.frame}.
#'                   The simulation parameters. Refer to
#'                   \code{?sim_eqa_data} for more information.
#' @param statistic A \code{function} or \code{character} string. The desired
#'                  statistic to approximate using the \code{n} simulated
#'                  \eqn{\hat{\zeta}} values. If passed as a \code{character}
#'                  string, it must be one of the following:
#' \itemize{
#'   \item \code{'none': } Returns simulated samples of \eqn{\hat{\zeta}}
#'         values. Thus, no statistic is calculated.
#'   \item \code{'all': } Approximates the \code{mean}, \code{variance},
#'         \code{skewness}, \code{kurtosis}, and the following percentiles:
#'         1st, 2.5th, 5th, 10th, 25th, 50th, 75th, 90th, 95th, 97.5th, and 99th.
#'   \item \code{'all2': } Approximates the \code{mean}, \code{variance},
#'         \code{skewness}, \code{kurtosis}, and the 1st and 99th percentiles.
#'   \item \code{'moments': } Approximates the \code{mean}, \code{variance},
#'         \code{skewness} and \code{kurtosis}.
#'   \item \code{'moments12': } Approximates the \code{mean} and
#'         \code{variance}.
#'   \item \code{'moments34': } Approximates the \code{skewness} and
#'         \code{kurtosis}.
#'   \item \code{'quantiles': } Approximates the following percentiles: 1st,
#'         2.5th, 5th, 10th, 25th, 50th, 75th, 90th, 95th, 97.5th, and 99th.
#'   \item \code{'quantiles2': } Approximates the following percentiles: 1st,
#'         25th, 50th, 75th, and 99th.
#'   \item \code{'quartiles': } Approximates the three quartiles: the 25th,
#'         50th, and 75th percentiles.
#' }
#' @param m An \code{integer} \eqn{\geq 1}. The number of replicated
#'          approximations of \code{statistic}. It is not advisable to let
#'          \code{m} \eqn{\times} \code{n} \eqn{\geq 1e7}, due to unacceptable
#'          computation times.
#' @param attach A non-missing \code{logical} value. If \code{TRUE} (default),
#'               the approximated statistic(s) are merged with
#'               \code{parameters} allowing a direct mapping between simulation
#'               parameters and approximated statistic(s).
#' @param probs A \code{double} \eqn{\in} \code{[0, 1]}. Only relevant if
#'              \code{statistic = quantile(...)}.
#' @param simplify A non-missing \code{logical} value. If \code{TRUE}, the
#'                 output is attempted to be converted into a simplified
#'                 format. Typically results in a \code{data.table} object
#'                 or a \code{vector}, depending on \code{attach}.
#'
#' @return A \code{list}, \code{data.table} or \code{numeric} vector.
#' @export
#'
#' @examples
#' # Required packages
#' library(fasteqa)
#' library(data.table)
#'
#' # Approximate the standard deviation of the point estimate of zeta
#'
#' # Simulation parameters (data with heteroscedasticity)
#' sim_pars <- list(n = 20,
#'                  R = 4,
#'                  cvx = 0.01,
#'                  cvy = 0.02,
#'                  cil = 5,
#'                  ciu = 90,
#'                  eta = 5,
#'                  eta0 = 1)
#'
#' # Simulated standard deviation based on 100 simulated point estimates of zeta
#' sd_zeta_hat <- simulate_zetas(n = 100,
#'                               parameters = sim_pars,
#'                               statistic = sd,
#'                               attach = TRUE)
#'
#' print(sd_zeta_hat)

simulate_zetas <- function(n, parameters, statistic = median, m = 1L, attach = TRUE, probs = 0.99, simplify = FALSE){

  # Tests if 'parameters' is a list, data.table or data.frame. If not, throw an error
  if(!is.list(parameters) & !is.data.table(parameters) & !is.data.frame(parameters)){
    stop("The input 'parameters' is neither a list, data.table, nor data.frame.
            \nRegistered input class of 'parameters': '", class(parameters)[1], "'.
            \nPlease provide an input of type list, data.table or data.frame to proceed. The calculations cannot continue with the current input type.")
  }

  # Checks validity of 'n': It must be a non-missing integer equal to or larger than 2
  if(!is.atomic(n)){
    stop(sprintf(paste("The provided 'n' is not atomic.",
                      "Expected an integer equal to or larger than 2, but a %s",
                      "was provided instead.",
                      "Ensure than 'n' is an integer value equal to or larger than 2 and try again."),
                class(n)[1]))
  }
  if(length(n) > 1L){
    warning(sprintf(paste("The provided 'n' is of length %g. The first element of 'n' is used."), length(n)), immediate. = TRUE)
    n <- n[1]
  }
  n_finite_non_missing_numeric <- is.numeric(n) && length(n) > 0L && !is.na(n) && is.finite(n)

  if(!n_finite_non_missing_numeric){
    if(is.null(n)){
      stop(sprintf(paste("The provided value of 'n' (NULL) is of invalid class (NULL). 'n' is required to be a non-missing integer value equal to or larger than 2.",
                         "Modify your inputs accordingly, and try again.")))
    }
    else{
      stop(sprintf(paste("The provided value of 'n' (%s) is of invalid class (%s). 'n' is required to be a non-missing integer value equal to or larger than 2.",
                         "Modify your inputs accordingly, and try again."), n, class(n)))
    }
  }

  if(n_finite_non_missing_numeric && n < 0){
    stop(sprintf(paste("The provided value of 'n' (%g) is negative. 'n' is required to be an integer value equal to or larger than 2.",
                      "Modify your inputs accordingly, and try again."), n))
  }
  if(n_finite_non_missing_numeric && n < 2){
    stop(sprintf(paste("The provided value of 'n' (%g) is smaller than 2. 'n' is required to be an integer value equal to or larger than 2.",
                      "Modify your inputs accordingly, and try again."), n))
  }
  if(n_finite_non_missing_numeric && abs(n - round(n + .Machine$double.eps)) > .Machine$double.eps ** 0.5){
    stop(sprintf(paste("The provided value of 'n' (%g) is a decimal number. 'n' is required to be an integer value equal to or larger than 2.",
                      "Modify your inputs accordingly, and try again."), n))
  }


  # Checks validity of 'attach': It must be a non-missing logical value
  if(is.atomic(attach)){
    if(length(attach) > 1L){
      warning(sprintf(paste("The provided 'attach' is of length %g. The first element of 'attach' is used."), length(attach)), immediate. = TRUE)
      attach <- attach[1]
      if(is.numeric(attach) && !is.na(attach) && is.finite(attach) && any(attach == c(0, 1))){
        attach <- as.logical(attach)
      }
      else if(is.character(attach) && !is.na(attach) && any(attach == c("FALSE", "TRUE"))){
        warning(sprintf(paste("The first element of the provided 'attach' ('%s') is a character string, which is generally not supported.",
                              "However, the provided character string match either TRUE or FALSE, so it will be coerced to the corresponding logical value.",
                              "To avoid this type of warning message in the future, make sure 'attach' is a non-missing logical value."), attach), immediate. = TRUE)
        attach <- as.logical(attach)
      }

      if(is.null(attach) || is.na(attach)){
        if(is.null(attach)){
          stop(sprintf(paste("The first element of the provided 'attach' (NULL) is invalid.",
                             "Expected a non-missing logical value, but a NULL",
                             "was provided instead. Valid entries for 'attach' are either TRUE or FALSE.")))
        }
        else{
          stop(sprintf(paste("The first element of the provided 'attach' (NA) is invalid.",
                             "Expected a non-missing logical value, but a missing value",
                             "was provided instead. Valid entries for 'attach' are either TRUE or FALSE.")))
        }

      }
      if(!is.logical(attach)){
        stop(sprintf(paste("The first element of the provided 'attach' (%s) is invalid.",
                           "Expected a non-missing logical value, but a %s",
                           ifelse(isTRUE(is.character(attach)), "string", "value"),
                           "was provided instead. Valid entries for 'attach' are either TRUE or FALSE."),
                     as.character(attach), class(attach)[1]))
      }
    }
    else{
      if(is.numeric(attach) && !is.na(attach) && is.finite(attach) && any(attach == c(0, 1))){
        attach <- as.logical(attach)
      }
      else if(is.character(attach) && !is.na(attach) && any(attach == c("FALSE", "TRUE"))){
        warning(sprintf(paste("The provided 'attach' ('%s') is a character string, which is generally not supported.",
                              "However, the provided character string match either TRUE or FALSE, so it will be coerced to the corresponding logical value.",
                              "To avoid this type of warning message in the future, make sure 'attach' is a non-missing logical value."), attach), immediate. = TRUE)
        attach <- as.logical(attach)
      }
      if(is.null(attach) || is.na(attach)){
        if(is.null(attach)){
          stop(sprintf(paste("The provided 'attach' (NULL) is invalid.",
                             "Expected a non-missing logical value, but a NULL",
                             "was provided instead. Valid entries for 'attach' are either TRUE or FALSE.")))
        }
        else{
          stop(sprintf(paste("The provided 'attach' (NA) is invalid.",
                             "Expected a non-missing logical value, but a missing value",
                             "was provided instead. Valid entries for 'attach' are either TRUE or FALSE.")))
        }
      }
      if(!is.logical(attach)){
        stop(sprintf(paste("The provided 'attach' (%s) is invalid.",
                           "Expected a logical value, but a %s",
                           ifelse(isTRUE(is.character(attach)), "string", "value"),
                           "was provided instead. Valid entries for 'attach' are either TRUE or FALSE."),
                     as.character(attach), class(attach)[1]))
      }
    }
  }
  else{
    stop(sprintf(paste("The provided 'attach' is not atomic.",
                       "Expected a non-missing logical value, but a %s",
                       "was provided instead. Valid entries for 'attach' are either TRUE or FALSE."),
                 class(attach)[1]))
  }

  # Checks validity of 'simplify': It must be a non-missing logical value
  if(is.atomic(simplify)){
    if(length(simplify) > 1L){
      warning(sprintf(paste("The provided 'simplify' is of length %g. The first element of 'simplify' is used."), length(simplify)), immediate. = TRUE)
      simplify <- simplify[1]
      if(is.numeric(simplify) && !is.na(simplify) && is.finite(simplify) && any(simplify == c(0, 1))){
        simplify <- as.logical(simplify)
      }
      else if(is.character(simplify) && !is.na(simplify) && any(simplify == c("FALSE", "TRUE"))){
        warning(sprintf(paste("The first element of the provided 'simplify' ('%s') is a character string, which is generally not supported.",
                              "However, the provided character string match either TRUE or FALSE, so it will be coerced to the corresponding logical value.",
                              "To avoid this type of warning message in the future, make sure 'simplify' is a non-missing logical value."), simplify), immediate. = TRUE)
        simplify <- as.logical(simplify)
      }

      if(is.null(simplify) || is.na(simplify)){
        if(is.null(simplify)){
          stop(sprintf(paste("The first element of the provided 'simplify' (NULL) is invalid.",
                             "Expected a non-missing logical value, but a NULL",
                             "was provided instead. Valid entries for 'simplify' are either TRUE or FALSE.")))
        }
        else{
          stop(sprintf(paste("The first element of the provided 'simplify' (NA) is invalid.",
                             "Expected a non-missing logical value, but a missing value",
                             "was provided instead. Valid entries for 'simplify' are either TRUE or FALSE.")))
        }

      }
      if(!is.logical(simplify)){
        stop(sprintf(paste("The first element of the provided 'simplify' (%s) is invalid.",
                           "Expected a non-missing logical value, but a %s",
                           ifelse(isTRUE(is.character(simplify)), "string", "value"),
                           "was provided instead. Valid entries for 'simplify' are either TRUE or FALSE."),
                     as.character(simplify), class(simplify)[1]))
      }
    }
    else{
      if(is.numeric(simplify) && !is.na(simplify) && is.finite(simplify) && any(simplify == c(0, 1))){
        simplify <- as.logical(simplify)
      }
      else if(is.character(simplify) && !is.na(simplify) && any(simplify == c("FALSE", "TRUE"))){
        warning(sprintf(paste("The provided 'simplify' ('%s') is a character string, which is generally not supported.",
                              "However, the provided character string match either TRUE or FALSE, so it will be coerced to the corresponding logical value.",
                              "To avoid this type of warning message in the future, make sure 'simplify' is a non-missing logical value."), simplify), immediate. = TRUE)
        simplify <- as.logical(simplify)
      }
      if(is.null(simplify) || is.na(simplify)){
        if(is.null(simplify)){
          stop(sprintf(paste("The provided 'simplify' (NULL) is invalid.",
                             "Expected a non-missing logical value, but a NULL",
                             "was provided instead. Valid entries for 'simplify' are either TRUE or FALSE.")))
        }
        else{
          stop(sprintf(paste("The provided 'simplify' (NA) is invalid.",
                             "Expected a non-missing logical value, but a missing value",
                             "was provided instead. Valid entries for 'simplify' are either TRUE or FALSE.")))
        }

      }
      if(!is.logical(simplify)){
        stop(sprintf(paste("The provided 'simplify' (%s) is invalid.",
                           "Expected a logical value, but a %s",
                           ifelse(isTRUE(is.character(simplify)), "string", "value"),
                           "was provided instead. Valid entries for 'simplify' are either TRUE or FALSE."),
                     as.character(simplify), class(simplify)[1]))
      }
    }
  }
  else{
    stop(sprintf(paste("The provided 'simplify' is not atomic.",
                       "Expected a non-missing logical value, but a %s",
                       "was provided instead. Valid entries for 'simplify' are either TRUE or FALSE."),
                 class(simplify)[1]))
  }


  # 'result_list' are meant to store the simulation results.
  result_list <- NULL
  # Default value of 'M'. If 'M' exists in 'parameters', that value will be used instead.
  M <- 0
  # Checks if 'M' is part of 'parameters'
  if(!is.null(parameters$M)){
    # If 'M' is part of 'parameters', is its length equal to or larger than 2?
    if(length(parameters$M) > 1){
      # Only keep the first element if 'length(parameters$M) > 1'
      parameters$M <- parameters$M[1]
    }
    # Checks if 'M' is of valid type, is finite and equal to or larger than zero
    M_valid <-  is.numeric(parameters$M) && !is.na(parameters$M) && is.finite(parameters$M) && parameters$M >= 0
    if(M_valid){
      # Summary: If 'M' exists in 'parameters' AND is a valid number, the default 'M = 0' is replaced with that M contained in 'parameters'
      M <- parameters$M
    }
  }


  # Checks validity of 'm': It must be a non-missing integer equal to or larger than 1
  if(is.atomic(m)){
    if(length(m) > 1L){
      warning(sprintf(paste("The provided 'm' is of length %g. The first element of 'm' is used."), length(m)), immediate. = TRUE)
      m <- m[1]
    }
  }
  else{
    stop(sprintf(paste("The provided 'm' is not atomic.",
                       "Expected a non-missing integer value equal to or larger than 1, but a %s",
                       "was provided instead. Valid entries for 'simplify' are either TRUE or FALSE."),
                 class(m)[1]))
  }

  m_finite_non_missing_numeric <- is.numeric(m) && length(m) > 0L && !is.na(m) && is.finite(m)
  m_negative <- m_finite_non_missing_numeric && m < 0
  m_decimal <- m_finite_non_missing_numeric && abs(m - round(m + .Machine$double.eps)) > .Machine$double.eps ** 0.5
  m_integer <- m_finite_non_missing_numeric && !m_decimal

  if(isTRUE(m_integer && m > 1)){
    if(is.character(statistic) && (statistic == "none" || statistic == "raw")){
      stop(sprintf(paste("Invalid inputs: 'statistic' is provided as a character string (%s) equal to 'raw' or 'none', but 'm' (%g) is larger than 1.",
                         "If 'statistic' is set to 'raw' or 'none', only 'm' equal to 1 is valid. Modify your inputs accordingly and try again."),
                   statistic, m))
    }
    result_list <- lapply(X = as.list(1:m), FUN = function(x) as.numeric(n))
  }
  else if(isTRUE(m_integer && m == 1)){
    result_list <- boot(data = parameters, statistic = simulate_zeta, R = n)$t
    if(isTRUE(identical(statistic, quantile))){
      result_list <- apply(result_list, 2, statistic, probs = probs, na.rm = TRUE, simplify = FALSE)
      result_list <- as.list(result_list[[1]])
      result_list <- lapply(result_list, function(x) x * (1 + M) ** 2)
      tmp_list <- list("replicate_id" = 1)
      result_list <- c(tmp_list, result_list)
      rm(tmp_list)
    }
    # Checks if 'statistic' is of character type. If it is, it must be one of the valid options:
    # "none", "all", "all2", "moments", "moments12", "moments34", "quantiles", "quantiles2", "quartiles".
    else if(is.character(statistic)){
      result_list <- result_list * (1 + M) ** 2

      if(statistic == "none" || statistic == "raw"){
        if(simplify && !attach){
          return(result_list[,])
        }
        else if(!simplify && !attach){
          result_list <- list(replicate_id = rep(1, n), zeta = result_list[,])
          return(result_list)
        }
        else if(simplify && attach){
          result_list <- list(replicate_id = rep(1, n), zeta = result_list[,])
          parameters <- lapply(parameters, FUN = rep, n)
          result_list <- c(parameters, result_list)
          setDT(result_list)
          return(result_list)
        }
        else if(!simplify && attach){
          result_list <- list(replicate_id = rep(1, n), zeta = result_list[,])
          parameters <- lapply(parameters, FUN = rep, n)
          result_list <- c(parameters, result_list)
          return(result_list)
        }

      }

      else if(statistic == "all"){
        result_list <- apply(result_list, 2, function(x) list("mean" = mean(x, na.rm = TRUE),
                                                              "sd" = sd(x, na.rm = TRUE),
                                                              "skewness" = skewness(x),
                                                              "kurtosis" = kurtosis(x),
                                                              "1%" = quantile(x, probs = 0.01, names = FALSE, na.rm = TRUE),
                                                              "2.5%" = quantile(x, probs = 0.025, names = FALSE, na.rm = TRUE),
                                                              "5%" = quantile(x, probs = 0.05, names = FALSE, na.rm = TRUE),
                                                              "10%" = quantile(x, probs = 0.10, names = FALSE, na.rm = TRUE),
                                                              "25%" = quantile(x, probs = 0.25, names = FALSE, na.rm = TRUE),
                                                              "50%" = quantile(x, probs = 0.50, names = FALSE, na.rm = TRUE),
                                                              "75%" = quantile(x, probs = 0.75, names = FALSE, na.rm = TRUE),
                                                              "90%" = quantile(x, probs = 0.90, names = FALSE, na.rm = TRUE),
                                                              "95%" = quantile(x, probs = 0.95, names = FALSE, na.rm = TRUE),
                                                              "97.5%" = quantile(x, probs = 0.975, names = FALSE, na.rm = TRUE),
                                                              "99%" = quantile(x, probs = 0.99, names = FALSE, na.rm = TRUE)))
        # apply() returns a list when using the above syntax in 'apply()', so 'result_list' must be unlisted!
        result_list <- result_list[[1]]
      }

      else if(statistic == "all2"){
        result_list <- apply(result_list, 2, function(x) list("mean" = mean(x, na.rm = TRUE),
                                                              "sd" = sd(x, na.rm = TRUE),
                                                              "skewness" = skewness(x),
                                                              "kurtosis" = kurtosis(x),
                                                              "1%" = quantile(x, probs = 0.01, names = FALSE, na.rm = TRUE),
                                                              "99%" = quantile(x, probs = 0.99, names = FALSE, na.rm = TRUE)))
        result_list <- result_list[[1]]
      }
      else if(statistic == "moments"){
        result_list <- apply(result_list, 2, function(x) list("mean" = mean(x, na.rm = TRUE),
                                                              "sd" = sd(x, na.rm = TRUE),
                                                              "skewness" = skewness(x),
                                                              "kurtosis" = kurtosis(x)))
        result_list <- result_list[[1]]
      }
      else if(statistic == "moments12"){
        result_list <- apply(result_list, 2, function(x) list("mean" = mean(x, na.rm = TRUE),
                                                              "sd" = sd(x, na.rm = TRUE)))
        result_list <- result_list[[1]]
      }
      else if(statistic == "moments34"){
        result_list <- apply(result_list, 2, function(x) list("skewness" = skewness(x),
                                                              "kurtosis" = kurtosis(x)))
        result_list <- result_list[[1]]
      }
      else if(statistic == "quantiles"){
        result_list <- apply(result_list, 2, function(x) list("1%" = quantile(x, probs = 0.01, names = FALSE, na.rm = TRUE),
                                                              "2.5%" = quantile(x, probs = 0.025, names = FALSE, na.rm = TRUE),
                                                              "5%" = quantile(x, probs = 0.05, names = FALSE, na.rm = TRUE),
                                                              "10%" = quantile(x, probs = 0.10, names = FALSE, na.rm = TRUE),
                                                              "25%" = quantile(x, probs = 0.25, names = FALSE, na.rm = TRUE),
                                                              "50%" = quantile(x, probs = 0.50, names = FALSE, na.rm = TRUE),
                                                              "75%" = quantile(x, probs = 0.75, names = FALSE, na.rm = TRUE),
                                                              "90%" = quantile(x, probs = 0.90, names = FALSE, na.rm = TRUE),
                                                              "95%" = quantile(x, probs = 0.95, names = FALSE, na.rm = TRUE),
                                                              "97.5%" = quantile(x, probs = 0.975, names = FALSE, na.rm = TRUE),
                                                              "99%" = quantile(x, probs = 0.99, names = FALSE, na.rm = TRUE)))
        result_list <- result_list[[1]]
      }
      else if(statistic == "quantiles2"){
        result_list <- apply(result_list, 2, function(x) list("1%" = quantile(x, probs = 0.01, names = FALSE, na.rm = TRUE),
                                                              "25%" = quantile(x, probs = 0.25, names = FALSE, na.rm = TRUE),
                                                              "50%" = quantile(x, probs = 0.50, names = FALSE, na.rm = TRUE),
                                                              "75%" = quantile(x, probs = 0.75, names = FALSE, na.rm = TRUE),
                                                              "99%" = quantile(x, probs = 0.99, names = FALSE, na.rm = TRUE)))
        result_list <- result_list[[1]]
      }
      else if(statistic == "quartiles"){
        result_list <- apply(result_list, 2, function(x) list("25%" = quantile(x, probs = 0.25, names = FALSE, na.rm = TRUE),
                                                              "50%" = quantile(x, probs = 0.50, names = FALSE, na.rm = TRUE),
                                                              "75%" = quantile(x, probs = 0.75, names = FALSE, na.rm = TRUE)))
        result_list <- result_list[[1]]
      }

      else{
        stop(sprintf(paste("The 'statistic' parameter (%s) was passed as a character string, but it did not match with one of the valid options.",
                           "Valid options include 'none' 'all', 'all2', 'moments', 'moments12', 'moments34', 'quantiles', 'quartiles' and 'quantiles2'.",
                           "The simulations cannot proceed before 'statistic' is either one of these character strings or a valid R function with",
                           "the first argument taking a numeric vector."), statistic))
      }
    }
    else if(is.function(statistic)){
      result_list <- result_list * (1 + M) ** 2
      result_list <- apply(result_list, 2, statistic)
      result_list <- list("replicate_id" = 1, "statistic" = unlist(result_list, use.names = FALSE))
    }
    else{
      stop(sprintf(paste("The provided 'statistic' parameter (%s) is a invalid class (%s).",
                         "Acceptable inputs include: an R function with its first argument accepting a numeric vector or",
                         "one of the following character strings: 'none', 'all', 'all2', 'moments', 'moments12', 'moments34',",
                         " 'quantiles', 'quartiles' and 'quantiles2'."), statistic, class(statistic)[1]))
    }

    if(attach){
      tmp_list <- list(replicate_id = 1)
      if(is.null(result_list$replicate_id)){
        result_list <- c(tmp_list, result_list)
      }
      result_list <- c(parameters, result_list)
      if(simplify){
        setDT(result_list)
      }
      return(result_list)
    }

    else{
      tmp_list <- list(replicate_id = 1)
      if(is.null(result_list$replicate_id)){
        result_list <- c(tmp_list, result_list)
      }
    }
    if(simplify){
      result_list <- result_list$statistic
    }
    return(result_list)
  }

  # Error messages can vary depending on the specific issue with 'm', providing context to the user.
  else{
    if(m_negative){
      stop(sprintf(paste("The provided value of 'm' (%s) is negative. 'm' is required to be an integer value equal to or larger than 1."),
                   m))
    }
    else if(m_decimal){
      stop(sprintf(paste("The provided value of 'm' (%s) is a decimal number. 'm' is required to be an integer value equal to or larger than 1."),
                   m))
    }
    else{
      stop(sprintf(paste("The provided value of 'm' (%s) is of invalid class (%s). 'm' is required to be an integer value equal to or larger than 1."),
                   m, class(m)[1]))
    }
  }

  result_list <- lapply(X = result_list, FUN = function(x) boot(data = parameters, statistic = simulate_zeta, R = n)$t[,])
  if(isTRUE(identical(statistic, quantile))){
    result_list <- lapply(result_list, statistic, probs = probs, na.rm = TRUE)
    result_list <- lapply(result_list, function(x) x * (1 + M) ** 2)
  }

  else if(is.character(statistic)){
    result_list <- lapply(result_list, function(x) x * (1 + M) ** 2)

    if(statistic == "all"){
      result_list <- lapply(result_list, function(x) list("mean" = mean(x, na.rm = TRUE),
                                                          "sd" = sd(x, na.rm = TRUE),
                                                          "skewness" = skewness(x),
                                                          "kurtosis" = kurtosis(x),
                                                          "1%" = quantile(x, probs = 0.01, names = FALSE, na.rm = TRUE),
                                                          "2.5%" = quantile(x, probs = 0.025, names = FALSE, na.rm = TRUE),
                                                          "5%" = quantile(x, probs = 0.05, names = FALSE, na.rm = TRUE),
                                                          "10%" = quantile(x, probs = 0.10, names = FALSE, na.rm = TRUE),
                                                          "25%" = quantile(x, probs = 0.25, names = FALSE, na.rm = TRUE),
                                                          "50%" = quantile(x, probs = 0.50, names = FALSE, na.rm = TRUE),
                                                          "75%" = quantile(x, probs = 0.75, names = FALSE, na.rm = TRUE),
                                                          "90%" = quantile(x, probs = 0.90, names = FALSE, na.rm = TRUE),
                                                          "95%" = quantile(x, probs = 0.95, names = FALSE, na.rm = TRUE),
                                                          "97.5%" = quantile(x, probs = 0.975, names = FALSE, na.rm = TRUE),
                                                          "99%" = quantile(x, probs = 0.99, names = FALSE, na.rm = TRUE)))
    }

    else if(statistic == "all2"){
      result_list <- lapply(result_list, function(x) list("mean" = mean(x, na.rm = TRUE),
                                                          "sd" = sd(x, na.rm = TRUE),
                                                          "skewness" = skewness(x),
                                                          "kurtosis" = kurtosis(x),
                                                          "1%" = quantile(x, probs = 0.01, names = FALSE, na.rm = TRUE),
                                                          "99%" = quantile(x, probs = 0.99, names = FALSE, na.rm = TRUE)))
    }

    else if(statistic == "moments"){
      result_list <- lapply(result_list, function(x) list("mean" = mean(x, na.rm = TRUE),
                                                          "sd" = sd(x, na.rm = TRUE),
                                                          "skewness" = skewness(x),
                                                          "kurtosis" = kurtosis(x)))
    }

    else if(statistic == "moments12"){
      result_list <- lapply(result_list, function(x) list("mean" = mean(x, na.rm = TRUE),
                                                          "sd" = sd(x, na.rm = TRUE)))
    }

    else if(statistic == "moments34"){
      result_list <- lapply(result_list, function(x) list("skewness" = skewness(x),
                                                          "kurtosis" = kurtosis(x)))
    }

    else if(statistic == "quantiles"){
      result_list <- lapply(result_list, function(x) list("1%" = quantile(x, probs = 0.01, names = FALSE, na.rm = TRUE),
                                                          "2.5%" = quantile(x, probs = 0.025, names = FALSE, na.rm = TRUE),
                                                          "5%" = quantile(x, probs = 0.05, names = FALSE, na.rm = TRUE),
                                                          "10%" = quantile(x, probs = 0.10, names = FALSE, na.rm = TRUE),
                                                          "25%" = quantile(x, probs = 0.25, names = FALSE, na.rm = TRUE),
                                                          "50%" = quantile(x, probs = 0.50, names = FALSE, na.rm = TRUE),
                                                          "75%" = quantile(x, probs = 0.75, names = FALSE, na.rm = TRUE),
                                                          "90%" = quantile(x, probs = 0.90, names = FALSE, na.rm = TRUE),
                                                          "95%" = quantile(x, probs = 0.95, names = FALSE, na.rm = TRUE),
                                                          "97.5%" = quantile(x, probs = 0.975, names = FALSE, na.rm = TRUE),
                                                          "99%" = quantile(x, probs = 0.99, names = FALSE, na.rm = TRUE)))
    }

    else if(statistic == "quantiles2"){
      result_list <- lapply(result_list, function(x) list("1%" = quantile(x, probs = 0.01, names = FALSE, na.rm = TRUE),
                                                          "25%" = quantile(x, probs = 0.25, names = FALSE, na.rm = TRUE),
                                                          "50%" = quantile(x, probs = 0.50, names = FALSE, na.rm = TRUE),
                                                          "75%" = quantile(x, probs = 0.75, names = FALSE, na.rm = TRUE),
                                                          "99%" = quantile(x, probs = 0.99, names = FALSE, na.rm = TRUE)))
    }

    else if(statistic == "quartiles"){
      result_list <- lapply(result_list, function(x) list("25%" = quantile(x, probs = 0.25, names = FALSE, na.rm = TRUE),
                                                          "50%" = quantile(x, probs = 0.50, names = FALSE, na.rm = TRUE),
                                                          "75%" = quantile(x, probs = 0.75, names = FALSE, na.rm = TRUE)))
    }

    else{
      stop(sprintf(paste("The 'statistic' parameter (%s) was passed as a character string, but it did not match with one of the valid options.",
                         "Valid options include 'none', 'all', 'all2', 'moments', 'moments12', 'moments34', 'quantiles', 'quartiles' and 'quantiles2'.",
                         "The simulations cannot proceed before 'statistic' is either one of these character strings or a valid R function with",
                         "the first argument taking a numeric vector."), statistic))
    }
  }
  else if(is.function(statistic)){
    result_list <- lapply(result_list, function(x) x * (1 + M) ** 2)
    result_list <- lapply(result_list, statistic)
  }

  else{
    stop(sprintf(paste("The provided 'statistic' parameter (%s) is a invalid class (%s).",
                       "Acceptable inputs include: an R function with its first argument accepting a numeric vector or",
                       "one of the following character strings: 'none', 'all', 'all2', 'moments', 'moments12', 'moments34',",
                       " 'quantiles', 'quartiles' and 'quantiles2'."), statistic, class(statistic)[1]))
  }

  if(attach && is.function(statistic)){
    parameters <- lapply(parameters, FUN = rep, m)
    result_list <- list("replicate_id" = 1:length(result_list),
                        "statistic" = unlist(result_list, use.names = FALSE))
    result_list <- c(parameters, result_list)
    if(simplify){
      setDT(result_list)
    }
    return(result_list)
  }
  else if(attach && is.character(statistic)){
    parameters <- lapply(parameters, FUN = rep, m)
    parameters$replicate_id <- 1:m
    result_list <- rbindlist(result_list, idcol = NULL)
    result_list <- c(parameters, result_list)
    if(simplify){
      setDT(result_list)
    }
    return(result_list)
  }
  single_statistic <- all(unlist(lapply(result_list, length)) == 1)
  if(single_statistic){
    result_list <- list("replicate_id" = 1:length(result_list),
                        "statistic" = unlist(result_list, use.names = FALSE))
    if(simplify){
      result_list <- result_list$statistic
    }
  }
  else{
    result_list <- mapply(FUN = c,
                          sapply(1:length(result_list), function(x) list(replicate_id = x), simplify = FALSE),
                          result_list,
                          SIMPLIFY = FALSE)
    if(simplify){
      result_list <- rbindlist(result_list, idcol = NULL)
    }
    else{
      result_list <- rbindlist(result_list, idcol = NULL)
      result_list <- lapply(result_list, FUN = function(x) x)
    }
  }
  return(result_list)
}
