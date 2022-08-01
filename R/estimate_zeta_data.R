#' All calculations regarding zeta
#'
#' @param data \code{list} or \code{data table} grouped by \code{"comparison"}. Must contain \code{comparison}, \code{SampleID}, \code{ReplicateID}, \code{MP_A} and \code{MP_B}
#' @param B Number of bootstrap replicates used to estimate bootstrap confidence intervals. The default is 2000, which is the typical for bootstrap confidence intervals
#' @param type Type of bootstrap confidence interval. There are four options:
#' \itemize{
#'   \item{\code{normal}: }{Standard normal bootstrap confidence interval}
#'   \item{\code{basic}: }{Basic bootstrap confidence interval}
#'   \item{\code{percentile}: }{Percentile bootstrap confidence interval}
#'   \item{\code{BCa}: }{Bias- and skewness-corrected bootstrap confidence interval using Jack knife}
#' }
#' @param level Confidence level of bootstrap confidence interval. A 95 percent confidence level is the default
#' @param M If \code{zeta_critical} is not specified (i.e., NA or not a double), what \code{M} should be used to obtain a upper value of zeta
#' @param zeta_critical A number. Default is NA, which will initiate a Monte Carlo simulation based on your average study design across all MS comparison to obtain an adequate upper zeta value. \code{M} must be specified to obtain a valid upper limit
#'
#' @description Obtain all necessary information on zeta data for each unique pair of IVD-MDs in \code{data}. The output is also on the form required by merge_results(), making it very useful
#'
#' @return A \code{data table} with entries \code{comparison}, \code{zeta}, \code{lwr}, \code{upr}, \code{zeta_critical} and \code{zeta_conclusion}
#' @export
#'
#' @examples \dontrun{
#'   print(1)
#' }

estimate_zeta_data <- function(data, B = 2e3, type = "percentile", level = 0.95, M = 0, zeta_critical = NA){
  setDT(data)
  estimate_zeta_data <- resample_samples <- estimate_zeta <- bootstrap_ci <- leave_one_out <- BCa_bootstrap_ci <- simulate_eqa_data <- NULL
  data$SampleID <- as.character(data$SampleID)
  data$ReplicateID <- as.character(data$ReplicateID)
  data_list <- split(data, by = "comparison")
  original_zetas <- lapply(X = data_list, FUN = function(x) unname(unlist(estimate_zeta(x))))
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

  if((!is.double(zeta_critical)) | is.na(zeta_critical)){
    simulation_parameters <- lapply(X = data_list, FUN = function(x) list(n = length(unique(x$SampleID)), R = length(unique(x$ReplicateID))))
    simulated_data <- lapply(X = simulation_parameters, FUN = function(x) replicate(n = 1e4, expr = setDT(simulate_eqa_data(x)), simplify = FALSE))
    simulated_zetas <-  lapply(X = simulated_data, FUN = function(x) unname(unlist(lapply(X = x, FUN = estimate_zeta))))
    rm(simulated_data)
    simulated_zeta_criticals <- unname(unlist(lapply(X = simulated_zetas, FUN = quantile, names = FALSE, probs = 0.99)))
    simulated_zeta_criticals <- simulated_zeta_criticals * (1 + M)^2
    zeta_critical <- round(mean(simulated_zeta_criticals), 2)
  }
  zeta_criticals <- lapply(X = data_list, FUN = function(x) zeta_critical)
  zeta_conclusion <- mapply(FUN = function(x, y) as.integer(x >= y), original_zetas, zeta_criticals, SIMPLIFY = FALSE)
  out <- mapply(FUN = function(x, y, z, w) list("zeta" = x, "lwr" = max(0.25, y[1]), "upr" = max(y[2], 0.5), "zeta_critical" = z, "zeta_conclusion" = w),
                original_zetas, bootstrap_cis, zeta_criticals, zeta_conclusion, SIMPLIFY = FALSE)
  out <- rbindlist(l = out, idcol = "comparison")
  return(out)

}

