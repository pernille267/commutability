#' Run a complete commutability evaluation analysis based on clinical sample data and external quality assessment / certified reference material data
#'
#' @param data A \code{list}, \code{data.table} or \code{data.frame} containing the measurements of clinical samples grouped by \code{comparison}, \code{SampleID} and \code{ReplicateID}.
#' @param new_data A \code{list}, \code{data.table}, or \code{data.frame} with measurements of either external quality assessment materials or reference materials grouped by \code{comparison}, \code{SampleID} and \code{ReplicateID}.
#' @param B An \code{integer} >= 100 that represents the number of bootstrap replicates utilized for estimating bootstrap confidence intervals. The default is set to 2000, a common choice for such calculations. Please be aware that if your \code{data} contains more than five unique IVD-MDs, the resampling process might take a couple of seconds to complete due to the computational complexity.
#' @param N An \code{integer} >= 100 that specifies the number of bootstrap resamples to be used for estimating the inside rates (commutability conclusion consistency) for the external quality assessment materials. Set to \code{NULL} to exclude calculations of inside rates.
#' @param method_pi A \code{character} string referring to the method to employ for estimating the Deming prediction intervals. The default is \code{'fg'}, standing for the Fuller and Gillard approach. Alternatively, the approach derived by CLSI EP14 can be used by setting \code{'clsi'}.
#' @param method_bs A \code{character} string that determines the bootstrap method to apply for estimating the confidence intervals of zeta and imprecision estimates, based on IVD-MD comparisons.
#' @param level_pi A \code{double} value that must be between 0 and 1, defining the desired confidence level for the estimated prediction intervals. The default confidence level is \code{level_pi = 0.99}.
#' @param level_bs A \code{double} value that must be between 0 and 1, designating the desired confidence level for the estimated bootstrap confidence intervals. The default confidence level is \code{level_bs = 0.95}.
#' @param M A \code{double} >= 0. If \code{upper_zeta} is either \code{NULL}, \code{NA}, or an improperly formatted \code{character} string (see details), and \code{avoid_simulations = FALSE}, then this parameter signifies the tolerable level of differences in non-selectivity between all IVD-MD pairs within \code{data} and \code{new_data}. This tolerance level is expressed as an average relative increase in the widths of pointwise prediction intervals. For example, specifying M as 0.15 implies an acceptance of an average relative increase of 15\%. If \code{M > 0}, \code{upper_zeta} is approximated through Monte Carlo simulations unless \code{avoid_simulations = TRUE}. In such a scenario, an error would be triggered. If \code{M} < 0, \code{M} will be silently set to 0 instead.
#' @param upper_zeta A \code{double} >= 1. An estimated zeta value that exceeds \code{upper_zeta} will suggest substantial differences in non-selectivity concerning the particular IVD-MD pair. If \code{upper_zeta} is provided as \code{NULL}, \code{NA}, or a \code{character} string with invalid syntax (refer to details), and \code{avoid_simulations = FALSE}, then the function will utilize the parameter \code{M} >= 0 to approximate a suitable upper limit for zeta through Monte Carlo simulations.
#' @param output A \code{character} string that can be \code{'sufficient'} or \code{'complete'}. Selecting \code{'sufficient'} will yield only the essential data analysis required for the commutability evaluation analysis. Choosing \code{'complete'} will produce a comprehensive data analysis for the commutability evaluation.
#' @param avoid_simulations A \code{logical} value that ensures that an error is thrown in case \code{upper_zeta} is a \code{character} string with invalid syntax (see details) instead of approximating \code{upper_zeta} using Monte Carlo simulations. The default value of \code{avoid_simulations} are set to \code{FALSE}
#'
#' @return A 2D \code{list} containing \code{merged_pb_data} and \code{merged_ce_data}, each being a \code{data.table} object. The \code{merged_pb_data} comprises pointwise prediction interval or prediction band data, intended solely for plotting purposes. On the other hand, \code{merged_ce_data} delivers a thorough, tabulated summary of the commutability evaluation experiment data analysis, grouped by \code{comparison} and \code{SampleID}. Both \code{merged_pb_data} and \code{merged_ce_data} are necessary input parameters for the \code{plot_commutability_evaluation_plots()} function.
#' @export
#'
#' @examples print(1)

do_commutability_evaluation <- function(data, new_data, B = 2e3L, N = 1e3L, method_pi = "fg", method_bs = "percentile", level_pi = 0.99, level_bs = 0.95, M = 0, upper_zeta = 2.25, output = "sufficient", avoid_simulations = FALSE){

  if(!is.data.table(data)){
    if(is.data.frame(data) | is.list(data)){
      setDT(data)
    }
    else{
      stop("The input 'data' is neither a data.table, list, nor data frame.
            \nRegistered input class of 'data': '", class(data)[1], "'.
            \nPlease provide an input of type data.table, list, or data frame to proceed. The calculations cannot continue with the current input type.")

    }
  }

  if(!is.data.table(new_data)){
    if(is.data.frame(new_data) | is.list(new_data)){
      setDT(new_data)
    }
    else{
      stop("The input 'data' is neither a data.table, list, nor data frame.
            \nRegistered input class of 'data': '", class(new_data)[1], "'.
            \nPlease provide an input of type data.table, list, or data frame to proceed. The calculations cannot continue with the current input type.")

    }
  }

  required_columns <- c('comparison', 'SampleID', 'ReplicateID', 'MP_A', 'MP_B')
  missing_columns_data <- setdiff(required_columns, names(data))
  missing_columns_new_data <- setdiff(required_columns, names(new_data))

  if(length(missing_columns_data) > 0){
    stop(paste0("Some required columns are missing from 'data':", "\n",
                "* Missing column(s): [",
                paste(missing_columns_data, collapse=", "),
                "]", "\n",
                "* The argument 'data' must include 'comparison', 'SampleID', 'ReplicateID', 'MP_A' and 'MP_B'."))
  }

  if(length(missing_columns_new_data) > 0){
    stop(paste0("Some required columns are missing from 'new_data':", "\n",
                "* Missing column(s): [",
                paste(missing_columns_new_data, collapse=", "),
                "]", "\n",
                "* The argument 'new_data' must include 'comparison', 'SampleID', 'ReplicateID', 'MP_A' and 'MP_B'."))
  }

  data_list <- split(x = data, by = "comparison", keep.by = FALSE)
  new_data_list <- split(x = new_data, by = "comparison", keep.by = FALSE)

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

  R_cs_list <- lapply(X = data_list, function(x) count_samplewise_replicates(x, "ceiling")$R_i)
  R_eq_list <- lapply(X = new_data_list, function(x) count_samplewise_replicates(x, "ceiling")$R_i)
  R_ratio_list <- mapply(function(R_cs, R_eq) R_cs / R_eq, R_cs_list, R_eq_list, SIMPLIFY = F)

  pb_data <- estimate_prediction_data(data = data, new_data = "gen_250", method = method_pi, level = level_pi, rounding = 3L, override_R_ratio = R_ratio_list)
  ce_data <- estimate_prediction_data(data = data, new_data = new_data, method = method_pi, level = level_pi, rounding = 3L, B = N)
  impr_data <- estimate_imprecision_data(data = data, type = method_bs, level = level_bs, B = B)
  simulate_zeta_upper <- FALSE

  if(length(upper_zeta) > 1){
    upper_zeta <- upper_zeta[1]
  }

  if(is.null(upper_zeta) || length(upper_zeta) == 0 || is.na(upper_zeta)){
    simulate_zeta_upper <- TRUE
  }

  else if(is.character(upper_zeta)){
    number_detected <- stri_detect_regex(str = upper_zeta, pattern = "^\\d+\\.\\d+$")
    if(isFALSE(number_detected)){
      attempt_fix_upper_zeta <- stri_replace_all_regex(str = upper_zeta, pattern = "[^[:digit:].]", replacement = "")
      attempt_fix_upper_zeta <- stri_replace_all_regex(str = attempt_fix_upper_zeta, pattern = "\\.{2,}", replacement = ".")
      if(isTRUE(stri_startswith(str = attempt_fix_upper_zeta, fixed = "."))){
        attempt_fix_upper_zeta <- stri_replace_first(attempt_fix_upper_zeta, fixed = ".", replacement = "")
      }
      if(isTRUE(stri_endswith(str = attempt_fix_upper_zeta, fixed = "."))){
        attempt_fix_upper_zeta <- stri_replace_last(attempt_fix_upper_zeta, fixed = ".", replacement = "")
      }
      n_dots <- stri_count_fixed(attempt_fix_upper_zeta, ".")
      if(n_dots >= 2){
        second_dot_position <- stri_locate_first_fixed(attempt_fix_upper_zeta, fixed = ".")[1, 2] +
          stri_locate_first_fixed(stri_sub(attempt_fix_upper_zeta,
                                           stri_locate_first_fixed(attempt_fix_upper_zeta,
                                                                   fixed = ".")[1, 2] + 1), fixed = ".")[1, 1]
        attempt_fix_upper_zeta <- stri_sub(attempt_fix_upper_zeta, 1, second_dot_position - 1)
      }

      number_detected <- stri_detect_regex(attempt_fix_upper_zeta, pattern = "^\\d+\\.\\d+$")
      if(isTRUE(number_detected)){
        upper_zeta <- as.numeric(attempt_fix_upper_zeta)
      }
      else if(isTRUE(avoid_simulations)){
        stop(sprintf("The 'upper_zeta' parameter was provided as a character string [%s] and could not be converted to a double type number.
                     \nWe tried to extract a valid number from the provided 'upper_zeta' value, which is '%s'.
                     \nUnfortunately, the syntax does not match the expected pattern '[number].[number]'.
                     \nPlease provide 'upper_zeta' in the correct character format (e.g., '2.25') or a double when 'avoid_simulations' are set to TRUE so the calculations can proceed.",
                     upper_zeta,
                     attempt_fix_upper_zeta))
      }
      else{
        simulate_zeta_upper <- TRUE
      }
    }
    else if(isTRUE(number_detected)){
      upper_zeta <- as.numeric(attempt_fix_upper_zeta)
    }

    else if(isTRUE(avoid_simulations)){
      stop(sprintf("The 'upper_zeta' parameter was provided as a character string [%s] and could not be converted to a double type number.
                    \nThe syntax match the expected pattern '[number].[number]', but could still not be coerced to a double.
                    \nPlease provide 'upper_zeta' as a double when 'avoid_simulations' are set to TRUE so the calculations can proceed.",
                    upper_zeta))
    }
    else{
      simulate_zeta_upper <- TRUE
    }
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
