#' Run a complete commutability evaluation based on CS data and EQA/CR material data
#'
#' @param data \code{list}, \code{data table} or \code{data frame} - Clinical sample data (CS data) for the commutability assessment experiment
#' @param new_data \code{list}, \code{data table} or \code{data frame} - Evaluated samples' data (EQAM / CRM data) for the commutability assessment experiment
#' @param method_pi \code{character} - Which method should be used to estimate the Deming prediction intervals. Default is \code{fg} which is Fuller and Gillard's approach. Alternatively, \code{clsi} may be used for EP14's approach
#' @param method_bs \code{character} - Which bootstrap method should be used to estimated the confidence levels of zeta and imprecision estimates
#' @param level_pi \code{double} - Number between 0 and 1, which is the required confidence level for the estimated prediction intervals
#' @param level_bs \code{double} - Number between 0 and 1, which is the required confidence level for the estimated bootstrap confidence intervals
#' @param M \code{double} - If upper_zeta is not given, how much differences in non-selectivity between IVD-MD pairs should we accept in terms of relative increase in prediction interval width. For example, 0.15 signify that we accept an average of 15 \% relative increase
#' @param upper_zeta \code{double} - Either NULL or a number. A estimated value of zeta above \code{upper_zeta} results in concluding with excessive differences in non-selectivity between IVD-MD pairs. If \code{NULL}, M will be used to look for an appropriate upper limit utilizing Monte Carlo simulations
#' @param output \code{character} - Either \code{sufficient} or \code{complete}. Passing \code{sufficient} will only output the necessary data analysis for the commutability evaluation analysis. Passing \code{complete} will output all data analysis for the commutability evaluation analysis
#'
#' @return A two-dimensional \code{list} containing \code{merged_pb_data} and \code{merged_ce_data}, and these are of \code{data table} class. \code{merged_pb_data} is used for plotting, whereas \code{merged_ce_data} is the summary of the data analysis concerning the commutability evaluation experiment
#' @export
#'
#' @examples print(1)

do_commutability_evaluation <- function(data, new_data, method_pi = "fg", method_bs = "percentile", level_pi = 0.99, level_bs = 0.95, M = 0, upper_zeta = 2.25, output = "sufficient"){
  pb_data <- estimate_prediction_data(data = data, new_data = "gen_250", method = method_pi, level = level_pi, rounding = 3L)
  ce_data <- estimate_prediction_data(data = data, new_data = new_data, method = method_pi, level = level_pi, rounding = 3L, B = 1e3)
  impr_data <- estimate_imprecision_data(data = data, type = method_bs, level = level_bs)
  if(any(is.null(upper_zeta)) | any(upper_zeta < 1) | any(is.na(upper_zeta)) | any(!(is.double(upper_zeta) | is.integer(upper_zeta)))){
    if(M < 0.001){warning(paste0("Chosen M, that is, ", M, " is unrealistically small (< 0.1%)")); M <- 0.001}
    zeta_data <- estimate_zeta_data(data = data, type = method_bs, level = level_bs, M = M, zeta_critical = upper_zeta)
    merged_results <- merge_results(pb_data = pb_data,
                                    ce_data = ce_data,
                                    imprecision_data = impr_data,
                                    zeta_data = zeta_data,
                                    rounding = 3L,
                                    include_imprecision_estimates = if(output=="complete"){TRUE}else{FALSE})
    merged_results$merged_pb_data |> setDT()
    merged_results$merged_ce_data <- merge(merged_results$merged_ce_data, ce_data[,c("comparison", "SampleID", "inside_rate")], by = c("comparison", "SampleID")) |> setDT()
    return(merged_results)
  }

  else{
    if(M > 0 | M < 0){M <- 0}
    zeta_data <- estimate_zeta_data(data = data, type = method_bs, level = level_bs, M = M, zeta_critical = upper_zeta)
    merged_results <- merge_results(pb_data = pb_data,
                                    ce_data = ce_data,
                                    imprecision_data = impr_data,
                                    zeta_data = zeta_data,
                                    rounding = 3L,
                                    include_imprecision_estimates = if(output=="complete"){TRUE}else{FALSE})
    merged_results$merged_pb_data |> setDT()
    merged_results$merged_ce_data <- merge(merged_results$merged_ce_data, ce_data[,c("comparison", "SampleID", "inside_rate")], by = c("comparison", "SampleID")) |> setDT()
    return(merged_results)
  }
}
