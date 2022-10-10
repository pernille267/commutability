#' Run a complete commutability evaluation based on CS data and EQA/CR material data
#'
#' @param data \code{list}, \code{data table} or \code{data frame} - Clinical sample data (CS data) for the commutability assessment experiment
#' @param new_data \code{list}, \code{data table} or \code{data frame} - Evaluated samples' data (EQAM / CRM data) for the commutability assessment experiment
#' @param B Integer - How many bootstrap replicates should be used to calculate confidence intervals for bootstrap confidence intervals
#' @param N Integer - How many bootstrap replicates should be used to calculate inside rates
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

do_commutability_evaluation <- function(data, new_data, B = 2e3L, N = 1e3L, method_pi = "fg", method_bs = "percentile", level_pi = 0.99, level_bs = 0.95, M = 0, upper_zeta = 2.25, output = "sufficient"){
  pb_data <- estimate_prediction_data(data = data, new_data = "gen_250", method = method_pi, level = level_pi, rounding = 3L)
  ce_data <- estimate_prediction_data(data = data, new_data = new_data, method = method_pi, level = level_pi, rounding = 3L, B = N)
  impr_data <- estimate_imprecision_data(data = data, type = method_bs, level = level_bs, B = B)
  simulate_zeta_upper <- FALSE

  if(length(upper_zeta) > 1){
    upper_zeta <- upper_zeta[1]
  }
  if(is.character(upper_zeta)){
    upper_zeta <- stri_replace_all_regex(str = upper_zeta, pattern = "[^[:digit:].]", replacement = "")
    number_detected <- !stri_detect(regex = "[^[:digit:].]")
    if(number_detected){
      test_1 <- stri_sub(upper_zeta, from = 1L, to = 1L) |> stri_detect(regex = "[^[:digit:]]") |> isFALSE()
      test_2 <- stri_sub(upper_zeta, from = -1L, to = -1L) |> stri_detect(regex = "[^[:digit:]]") |> isFALSE()
      if(all(test_1, test_2)){
        upper_zeta <- paste0(stri_sub(upper_zeta, from = 1L, to = 1L), ".", stri_sub(upper_zeta, from = -1L, to = -1L)) |> as.numeric()
        if(upper_zeta < 1 | upper_zeta > 20){
          simulate_zeta_upper <- TRUE
        }
        else{
          warning(paste0(upper_zeta, " is used as upper zeta. If this was not your intention, make sure you enter the upper zeta value correctly!"))
        }
      }
    }
    else{
      simulate_zeta_upper <- TRUE
    }
  }

  else if(is.null(upper_zeta)){
    simulate_zeta_upper <- TRUE
  }
  else if(length(upper_zeta) == 0){
    simulate_zeta_upper <- TRUE
  }
  else if(is.na(upper_zeta)){
    simulate_zeta_upper <- TRUE
  }


  if(simulate_zeta_upper){
    if(M < 0){
      M <- 0
    }
    zeta_data <- estimate_zeta_data(data = data, B = B, type = method_bs, level = level_bs, M = M, zeta_critical = NULL)
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
    zeta_data <- estimate_zeta_data(data = data, type = method_bs, level = level_bs, M = 0, zeta_critical = upper_zeta, B = B)
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
