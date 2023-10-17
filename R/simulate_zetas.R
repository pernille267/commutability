#' Monte Carlo Simulations for the Estimator of zeta
#'
#' Simulate statistics of the zeta estimator using Monte Carlo methods.
#'
#' @param n An \code{integer} equal to or larger than 1, specifying the number of zeta value simulations for each replication of the target \code{statistic}.
#' @param parameters A \code{list}, \code{data.table}, or \code{data.frame} with simulation parameters. Refer to \code{?simulate_eqa_data} for detailed options and descriptions.
#' @param statistic An \code{R function} applied to the simulated zeta values or a \code{character} string selecting from predefined options to approximate various statistics of the estimated zeta value. Valid character strings are:
#' \itemize{
#'   \item \code{none}: Outputs the raw simulated values.
#'   \item \code{all}: Computes the first four moments and the following percentiles: 1st, 2.5th, 5th, 10th, 25th, 50th, 75th, 90th, 95th, 97.5th, and 99th.
#'   \item \code{all2}: Computes the first four moments and the 1st and 99th percentiles.
#'   \item \code{moments}: Computes the first four moments.
#'   \item \code{moments12}: Computes the first two moments.
#'   \item \code{moments34}: Computes the third and fourth moments.
#'   \item \code{quantiles}: Computes the following percentiles: 1st, 2.5th, 5th, 10th, 25th, 50th, 75th, 90th, 95th, 97.5th, and 99th.
#'   \item \code{quantiles2}: Computes the following percentiles: 1st, 25th, 50th, 75th, and 99th.
#'   \item \code{quartiles}: Computes the 25th, 50th, and 75th percentiles (i.e., the quartiles).
#' }
#' @param m An \code{integer} ≥ 1, indicating the number of replications for the \code{statistic}. If \code{m > 1}, \code{n} zeta values are simulated \code{m} times. For each set of \code{n} simulated values, the \code{statistic} is computed, yielding \code{m} replications. Be cautious: large \code{n} and \code{m} values significantly impact computation time. It is advised not to let n times m exceed \code{1e7L} to prevent excessive runtimes.
#' @param attach A non-missing \code{logical} value. If \code{TRUE} (default), the computed statistic(s) for the zeta value are added to the \code{parameters} object for easy reference.
#' @param probs A \code{double} between 0 and 1. Relevant when \code{statistic} is \code{quantile}. For example, to simulate the 99th percentile of zeta's estimator, set \code{statistic = "quantile"} and \code{probs = 0.99}.
#' @param simplify A non-missing \code{logical} value. If \code{TRUE}, the output will be attempted to be simplified. If \code{attach} is \code{TRUE} and \code{simplify} is \code{TRUE} the output will be always be simplified to a \code{data.table} object. On the other hand, if \code{attach} is \code{FALSE} the output will only be simplified to a \code{data.table} if several statistics are simulated, and simplified to a vector if only one statistic is simulated.
#'
#' @return A \code{list} either with both the input parameters and simulated statistics for zeta's estimator or just the simulated statistics, based on the \code{attach} option.
#' @export
#'
#' @examples
#' # Simulate 100 standard deviations of zeta's estimator with 'eta' = 5 (heteroscedasticity).
#' # Simulations use a design with 'n' = 20 clinical samples and 'R' = 4 replicates.
#' simulate_zetas(n = 100,
#'                parameters = list(n = 20, R = 4, eta = 5, eta0 = 1),
#'                statistic = sd,
#'                attach = TRUE)
#'
#' # Simulate 1 99th percentiles of zeta's estimator with 'prop' = 0.15 and
#' # 'mmax' = 10 (random DINS).
#' # Simulations use a design with n = 25 clinical samples and R = 3 replicates.
#' simulate_zetas(n = 1000,
#'                parameters = list(n = 25, R = 3, prop = 0.15, mmax = 10),
#'                statistic = quantile,
#'                attach = TRUE,
#'                probs = 0.99)

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
                                                              "skewness" = skewness(x, na.rm = TRUE),
                                                              "kurtosis" = kurtosis(x, na.rm = TRUE),
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
                                                              "skewness" = skewness(x, na.rm = TRUE),
                                                              "kurtosis" = kurtosis(x, na.rm = TRUE),
                                                              "1%" = quantile(x, probs = 0.01, names = FALSE, na.rm = TRUE),
                                                              "99%" = quantile(x, probs = 0.99, names = FALSE, na.rm = TRUE)))
        result_list <- result_list[[1]]
      }
      else if(statistic == "moments"){
        result_list <- apply(result_list, 2, function(x) list("mean" = mean(x, na.rm = TRUE),
                                                              "sd" = sd(x, na.rm = TRUE),
                                                              "skewness" = skewness(x, na.rm = TRUE),
                                                              "kurtosis" = kurtosis(x, na.rm = TRUE)))
        result_list <- result_list[[1]]
      }
      else if(statistic == "moments12"){
        result_list <- apply(result_list, 2, function(x) list("mean" = mean(x, na.rm = TRUE),
                                                              "sd" = sd(x, na.rm = TRUE)))
        result_list <- result_list[[1]]
      }
      else if(statistic == "moments34"){
        result_list <- apply(result_list, 2, function(x) list("skewness" = skewness(x, na.rm = TRUE),
                                                              "kurtosis" = kurtosis(x, na.rm = TRUE)))
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
                                                          "skewness" = skewness(x, na.rm = TRUE),
                                                          "kurtosis" = kurtosis(x, na.rm = TRUE),
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
                                                          "skewness" = skewness(x, na.rm = TRUE),
                                                          "kurtosis" = kurtosis(x, na.rm = TRUE),
                                                          "1%" = quantile(x, probs = 0.01, names = FALSE, na.rm = TRUE),
                                                          "99%" = quantile(x, probs = 0.99, names = FALSE, na.rm = TRUE)))
    }

    else if(statistic == "moments"){
      result_list <- lapply(result_list, function(x) list("mean" = mean(x, na.rm = TRUE),
                                                          "sd" = sd(x, na.rm = TRUE),
                                                          "skewness" = skewness(x, na.rm = TRUE),
                                                          "kurtosis" = kurtosis(x, na.rm = TRUE)))
    }

    else if(statistic == "moments12"){
      result_list <- lapply(result_list, function(x) list("mean" = mean(x, na.rm = TRUE),
                                                          "sd" = sd(x, na.rm = TRUE)))
    }

    else if(statistic == "moments34"){
      result_list <- lapply(result_list, function(x) list("skewness" = skewness(x, na.rm = TRUE),
                                                          "kurtosis" = kurtosis(x, na.rm = TRUE)))
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
