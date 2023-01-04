#' All calculations regarding zeta
#'
#' @param data \code{data table}, \code{data frame} or \code{list} - Clinical sample data grouped by 'comparison'. Data must be grouped by 'comparison' and contain \code{comparison}, \code{SampleID}, \code{ReplicateID}, \code{MP_A} and \code{MP_B}.
#' @param B \code{integer} - Number of bootstrap replicates used to estimate bootstrap confidence intervals. The default is 2000, which is the typical for bootstrap confidence intervals. Set B = 0 to avoid estimation of bootstrap confidence intervals and only to return the point estimates of zeta.
#' @param type Type of bootstrap confidence interval. There are four options:
#' \itemize{
#'   \item{\code{normal}: }{Standard normal bootstrap confidence interval}
#'   \item{\code{basic}: }{Basic bootstrap confidence interval}
#'   \item{\code{percentile}: }{Percentile bootstrap confidence interval}
#'   \item{\code{BCa}: }{Bias- and skewness-corrected bootstrap confidence interval using Jack knife}
#' }
#' @param level \code{numeric} - Confidence level of bootstrap confidence interval. A 95 percent confidence level is the default.
#' @param M If \code{zeta_critical} is not specified (i.e., NA or not a double), what \code{M} should be used to obtain a upper value of zeta
#' @param N if \code{zeta_critical} is not specified (i.e., NA or not a double), how many iterations should be used in the simulation of \code{zeta_critical}? Default is N = 1e4L.
#' @param zeta_critical \code{double}. Default is NA, which will initiate a Monte Carlo simulation based on your average study design across all MS comparison to obtain an adequate upper zeta value. \code{M} and \code{N} must be specified to obtain a valid upper limit
#'
#' @description Obtain all necessary information on zeta data for each unique pair of IVD-MDs in \code{data}. The output is also on the form required by merge_results(), making it very useful
#'
#' @return A \code{data table} with entries \code{comparison}, \code{zeta}, \code{lwr}, \code{upr}, \code{zeta_critical} and \code{zeta_conclusion}
#' @export
#'
#' @examples \dontrun{
#'   print(1)
#' }

estimate_zeta_data <- function(data, B = 2e3L, type = "percentile", level = 95/100, M = 0, N = 1e4L, zeta_critical = NA){

  if(!is.data.table(data)){
    if(is.data.frame(data)){
      data <- as.data.table(data)
    }
    else if(is.list(data)){
      setDT(data)
    }
    else{
      stop("data was not a data table, data frame or list, but a '", class(data), "'.", "\n",
           "Estimation of zeta data is accordingly terminated")
    }
  }

  estimate_zeta_data <- resample_samples <- estimate_zeta <- bootstrap_ci <- leave_one_out <- BCa_bootstrap_ci <- simulate_eqa_data <- NULL

  if(typeof(data$SampleID) != "character"){
    data$SampleID <- as.character(data$SampleID)
  }
  if(typeof(data$ReplicateID) != "character"){
    data$ReplicateID <- as.character(data$ReplicateID)
  }
  if(!any("comparison" == names(data))){
    stop("comparison was not found in data. Make sure it is included. Are you sure data is on long format?")
  }
  data_list <- split(data, by = "comparison")
  original_zetas <- lapply(X = data_list, FUN = function(x) unname(unlist(estimate_zeta(x))))

  if(B < 50L & B >= 1L){
    warning("B should be at least 2,000 to get fair bootstrap confidence interval estimates.", "\n",
            "Having B = ", B, " < 50 is not good enough.", "\n",
            "No confidence intervals are attempted estimated. Choose B greater or equal to 50 to force estimation of confidence intervals.")
    original_zetas <- lapply(X = original_zetas, FUN = function(x) data.table(zeta = x))
    return(rbindlist(l = original_zetas, idcol = "comparison"))

  }
  else if(B < 1L){
    original_zetas <- lapply(X = original_zetas, FUN = function(x) data.table(zeta = x))
    return(rbindlist(l = original_zetas, idcol = "comparison"))
  }


  resampled_data <- lapply(X = data_list, FUN = function(x) replicate(n = B, expr = setDT(resample_samples(data = x, silence = 1)), simplify = FALSE))
  bootstrapped_zetas <- lapply(X = resampled_data, FUN = function(x) unname(unlist(lapply(X = x, FUN = estimate_zeta))))
  if(type == "normal"){
    bootstrap_cis <- mapply(FUN = function(x, y) bootstrap_ci(x, y, 1, level), bootstrapped_zetas, original_zetas, SIMPLIFY = FALSE)
  }
  else if(type == "basic"){
    bootstrap_cis <- mapply(FUN = function(x, y) bootstrap_ci(x, y, 2, level), bootstrapped_zetas, original_zetas, SIMPLIFY = FALSE)
  }
  else if(type == "percentile"){
    bootstrap_cis <- mapply(FUN = function(x, y) bootstrap_ci(x, y, 3, level), bootstrapped_zetas, original_zetas, SIMPLIFY = FALSE)
  }
  else if(type == "BCa"){
    loo_data <- lapply(X = data_list, FUN = function(x) sapply(1:length(unique(x$SampleID)), FUN = function(y) leave_one_out(x, y), simplify = FALSE))
    loo_data <- lapply(X = loo_data, FUN = function(x) lapply(x, FUN = setDT))
    loo_zetas <- lapply(X = loo_data, FUN = function(x) unname(unlist(lapply(X = x, FUN = estimate_zeta))))
    bootstrap_cis <- mapply(FUN = function(x, y, z) BCa_bootstrap_ci(x, y, z, level), bootstrapped_zetas, loo_zetas, original_zetas, SIMPLIFY = FALSE)
  }
  else{
    bootstrap_cis <- mapply(FUN = function(x, y) bootstrap_ci(x, y, 3, level), bootstrapped_zetas, original_zetas, SIMPLIFY = FALSE)
  }

  if(is.null(zeta_critical) || (!is.numeric(zeta_critical)) || is.na(zeta_critical)){
    simulation_parameters <- lapply(X = data_list, FUN = function(x) list(n = length(unique(x$SampleID)), R = length(unique(x$ReplicateID))))
    simulated_data <- lapply(X = simulation_parameters, FUN = function(x) replicate(n = N, expr = setDT(simulate_eqa_data(x)), simplify = FALSE))
    simulated_zetas <-  lapply(X = simulated_data, FUN = function(x) unname(unlist(lapply(X = x, FUN = estimate_zeta))))
    rm(simulated_data)
    simulated_zeta_criticals <- unname(unlist(lapply(X = simulated_zetas, FUN = quantile, names = FALSE, probs = 0.99)))
    simulated_zeta_criticals <- simulated_zeta_criticals * (1 + M) ** 2
    zeta_critical <- round(mean(simulated_zeta_criticals), 2)
  }
  zeta_criticals <- lapply(X = data_list, FUN = function(x) zeta_critical)
  zeta_conclusion <- mapply(FUN = function(x, y) as.integer(x >= y), original_zetas, zeta_criticals, SIMPLIFY = FALSE)
  out <- mapply(FUN = function(x, y, z, w) list("zeta" = x, "lwr" = max(0, y[1]), "upr" = max(y[2], 0), "zeta_critical" = z, "zeta_conclusion" = w),
                original_zetas, bootstrap_cis, zeta_criticals, zeta_conclusion, SIMPLIFY = FALSE)
  out <- rbindlist(l = out, idcol = "comparison")
  return(out)

}

