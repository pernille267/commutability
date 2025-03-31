#' Validates \code{data}
#'
#' @description
#' This function is used to validate and convert the \code{data} argument
#' in \code{estimate_prediction_data}.
#'
#' @param data A object to be validated.
#' @param na_rm A \code{logical} value. If \code{TRUE}, \code{NA} values are
#'              removed from the validated \code{data}.
#' @return Processed \code{data}
#' @keywords internal
validate_data_epd <- function(data, na_rm = TRUE){

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
    stop(sprintf("Missing required columns in data: [%s]",
                 paste(missing_columns, collapse=", ")))
  }

  # Removes NA values from 'data' if 'na_rm' = TRUE
  if(isTRUE(na_rm)){
    data <- na.omit(data)
  }

  return(data)

}

#' Validates \code{new_data}
#'
#' @description
#' This function is used to validate and convert the \code{new_data} argument
#' in \code{estimate_prediction_data}.
#'
#' @param new_data A object to be validated.
#' @param na_rm A \code{logical} value. If \code{TRUE}, \code{NA} values are
#'              removed from the validated \code{new_data}.
#' @return Processed \code{new_data}
#' @keywords internal
validate_new_data_epd <- function(new_data, na_rm = TRUE){

  # Avoid modifying original input
  new_data <- copy(new_data)

  # Convert new_data if it is not a data.table yet.
  if(!is.data.table(new_data)){
    if(is.data.frame(new_data) | is.list(new_data)){
      setDT(new_data)
    }
    else{
      stop("The input 'new_data' is neither a data.table, list, nor data frame.
            \nRegistered input class of 'new_data': '", class(new_data)[1], "'.
            \nPlease provide an input of type data.table, list, or data frame to proceed. The input can also be NULL or a character string adhering to a specific syntax (please refer to the function documentation for more information on these alternative input options). The calculations cannot continue with the current input type of 'new_data'.")
    }
  }

  # Check mandatory column names
  required_columns <- c("comparison", "MP_B")
  missing_columns <- setdiff(required_columns, names(new_data))

  # If mandatory columns missing: Throw error.
  if(length(missing_columns) > 0){
    stop(paste0("Some required columns are missing from 'new_data':", "\n",
                "* Missing column(s): [",
                paste(missing_columns, collapse=", "),
                "]", "\n",
                "* The argument 'new_data' must include 'comparison' and 'MP_B' if 'new_data' is of class data.table, list or data frame."))
  }

  # Removes NA values from 'new_data' if 'na_rm' = TRUE
  if(isTRUE(na_rm)){
    new_data <- na.omit(object = new_data)
  }

  return(new_data)

}

#' Validates Names of \code{data} and \code{new_data}
#'
#' @description
#' This function is used to check if \code{data} and \code{new_data} have the
#' same names and the same order. This is a helper function used in
#' \code{estimate_prediction_data}
#'
#' @param data_list Object to be checked.
#' @param new_data_list Object to be checked.
#'
#' @return
#' Returns nothing if check is successful. Otherwise, an error is thrown.
#' @keywords internal
check_names_epd <- function(data_list, new_data_list){

  # Checks if 'data_list' and 'new_data_list' have the same names and in the same order
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
}

#' Extracts Number of Pointwise Prediction Intervals From \code{new_data}
#'
#' @description
#' If \code{new_data} is a \code{character} string on the correct format,
#' extracts the number of pointwise prediction intervals used to construct
#' \code{PB} data. This is a helper function used in
#' \code{estimate_prediction_data}. Should not be used by end-users.
#'
#' @param x A \code{character} string.
#'
#' @return
#' An \code{integer}.
#' @keywords internal
extract_n_gens <- function(x){

  # Edge case: if x = NA_character_
  if(is.na(x)){
    warning("new_data is NA. Defaulting to n = 100")
    return(100)
  }

  # Split string according to punctionation
  split_string <- stri_split(x, regex = "[:punct:]")[[1]]
  converted_string <- split_string[split_string != ""]

  # Validity check
  if(length(converted_string) != 2 || !any("gen" == converted_string)){
    stop(sprintf("Invalid input '%s'. Expected format 'gen#number' or 'number#gen'.",
                 x))
  }

  n_gens <- converted_string[converted_string != "gen"]
  n_gens_numeric <- as.numeric(stri_replace_all(str = n_gens,
                                                replacement = "",
                                                regex = "[^0-9]"))

  if(is.na(n_gens_numeric) || n_gens_numeric < 2 || n_gens_numeric > 1e5){
    warning("Invalid or out-of-range number provided. Defaulting to n = 100.")
    return(100)
  }

  return(n_gens_numeric)
}

#' Generates \code{n} Uniformly Distributed \code{MP_B} Values
#'
#' @description
#' Generates \code{n} uniformly distributed \code{MP_B} values in the interval
#' \eqn{[\mathrm{lwr}, \mathrm{upr}]}. Should not be used by end-users.
#'
#' @param n An \code{integer}.
#' @param lwr A \code{double}. Lower limit for generated values of \code{MP_B}.
#' @param upr A \code{double}. Upper limit for generated values of \code{MP_B}.
#'
#' @return
#' A \code{numeric} vector of length \code{n}.
#' @keywords internal
gen_grid <- function(n = 100, lwr, upr){
  out_grid <- seq(from = lwr, to = upr, length.out = n)
  return(out_grid)
}


#' @title
#' Estimate Inside Rate Using Bootstrap Resampling
#'
#' @param B An \code{integer}. Must be larger than or equal to \code{2}. The
#'          desired number of bootstrap replicates used to estimate the inside
#'          rate.
#' @param data A \code{data.table}. Should contain the following variables:
#'             \itemize{
#'              \item \code{SampleID: } A \code{character} vector. The sample
#'                    identifiers for the clinical samples.
#'              \item \code{MP_A: } A \code{numeric} vector. The observed
#'                    measurement results from IVD-MD \code{MP_A} (response).
#'              \item \code{MP_B: } A \code{numeric} vector. The observed
#'                    measurement results from IVD-MD \code{MP_B} (predictor).
#'             }
#' @param new_data A \code{data.table}. Should contain the same variables as
#'                 \code{data}.
#' @param R An \code{integer}. Must be larger than or equal to \code{2}. The
#'          ceiling of the average number of replicated measurements used in
#'          \code{new_data}.
#' @param R_ratio A \code{double}. The ratio between the ceiling of the average
#'                number of replicated measurements used in \code{data} and
#'                \code{R}.
#' @param level A \code{double}. Must be between \code{0} and \code{1}. The
#'              desired nominal confidence level for the estimated prediction
#'              intervals. Defaults to \code{0.99} (\eqn{99\%}).
#' @param method A \code{character} string. The method used to estimate
#'               prediction intervals. See \code{?estimate_prediction_data} for
#'               more information.
#' @param rounding An \code{integer}. The number of decimals to include in the
#'                 \code{numeric} results. Defaults to \code{3L}.
#'
#' @description
#' Estimate the \code{SampleID}-wise inside rates by resampling the
#' \code{inside} indicator variables \code{B} times.
#'
#' @details
#' For each \eqn{b = 1, 2, \ldots, B}, resample \code{data}. This results in
#' \code{B} prediction datasets, which contains the \code{inside} indicator
#' variables. For each unique \code{SampleID} in \code{new_data}, we take the
#' average of the \code{B} resampled values of \code{inside} and this is the
#' resulting estimated inside rate.
#'
#' Note that the estimation of inside rates using either \code{method = 'ss'}
#' or \code{method = 'ssw'} will take considerably longer than using either
#' of the linear choices of \code{method}.
#'
#' @returns
#' A \code{data.table}. Contains the inside rates (denoted by
#' \code{inside_rate}) for each unique \code{SampleID} in \code{new_data}.
#'
#' @export
#'
#' @examples
#' # Required packages
#' library(fasteqa)
#'
#' test_cs_data <- test_data[SampleID != "1" & SampleID != "2"]
#' test_eq_data <- test_data[SampleID == "1" | SampleID == 2, fun_of_replicates(.SD)]
#'
#' print(bootstrap_inside_rate(B = 20,
#'                             data = test_cs_data,
#'                             new_data = test_eq_data,
#'                             R = 3,
#'                             R_ratio = 1,
#'                             level = 0.95,
#'                             method = "fg",
#'                             rounding = 3L))
#'
#'
bootstrap_inside_rate <- function(B, data, new_data, R, R_ratio, level = 0.95, method = "fg", rounding = 3L){

  # Avoid modifying original data and new_data
  data <- copy(data)
  new_data <- copy(new_data)

  # Binding global variables
  SampleID <- inside <- NULL

  # If one desires linear models
  if (any(method == c("fg", "clsi", "ols"))) {
    output <- replicate(n = B,
                        expr = {
                           bdata <- resample_samples(data)
                           bimpr_data <- global_precision_estimates(bdata)
                           bmor_data <- fun_of_replicates(bdata)
                           predict_eqa(data = bmor_data,
                                       new_data = new_data,
                                       imprecision_estimates = bimpr_data,
                                       R = R,
                                       R_ratio = R_ratio,
                                       method = method,
                                       level = level,
                                       allow_reverse_regression = FALSE,
                                       rounding = rounding)

                         },
                         simplify = FALSE)

    output <- rbindlist(output, idcol = "resample_id")
    output <- output[, list(inside_rate = mean(inside, na.rm = TRUE)),
                     by = SampleID]

    return(output)

  }

  # If one desires smoothing spline models
  else if (any(method == c("ss", "ssw"))) {

    # MOR data
    mor_data <- fun_of_replicates(data)
    effective_df <- tryCatch(expr = smoothing_spline(data = mor_data,
                                                     weights = ifelse(method == "ssw", "estimate", 1),
                                                     df = NULL,
                                                     lambda = NULL,
                                                     df_max = 7.5,
                                                     attempt_fast = FALSE,
                                                     na_rm = TRUE)$df,
                             error = function(e) NULL)

    if(is.null(effective_df)){
      warning("Could not obtain a suitable df")
      Recall(B, data, new_data, R, R_ratio, level, metod = "fg", rounding)
    }

    if(effective_df <= 2.2){
      effective_df <- 2.2
    }

    output <- replicate(n = B,
                        expr = {
                          bmor_data <- resample_fun_of_samples(mor_data)
                          bpredict_ss <- tryCatch(expr = {
                            predict_smoothing_spline(data = bmor_data,
                                                     new_data = new_data,
                                                     weighted = method == "ssw",
                                                     df = effective_df,
                                                     lambda = NULL,
                                                     df_max = 7.5,
                                                     R_ratio = R_ratio,
                                                     level = level,
                                                     simultaneous = FALSE,
                                                     negative_ok = TRUE,
                                                     attempt_fast = FALSE,
                                                     include_prediction_variance = FALSE,
                                                     rounding = rounding)
                          },
                          error = function(e) NULL,
                          warning = function(w) NULL)
                        },
                        simplify = FALSE)

    output <- Filter(function(x) !is.null(x), output)
    output <- rbindlist(output, idcol = "resample_id")
    output <- output[, list(inside_rate = mean(inside, na.rm = TRUE)),
                     by = SampleID]

    return(output)

  }
}

#' @title
#' Estimate Prediction Intervals Based on Clinical Sample (CS) data and
#' Evaluated Material Data
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
#' @param new_data A \code{list}, \code{data.table} or \code{data.frame}. The
#'                 CS data. Must contain the following variables:
#'                 \itemize{
#'                    \item \code{comparison: } See \code{data}.
#'                    \item \code{MP_B: } See \code{data}.
#'                 }
#'                 Can contain:
#'                 \itemize{
#'                    \item \code{SampleID: } A \code{character} vector. The
#'                    sample identifiers for the evaluated material samples.
#'                    \item \code{ReplicateID: } See \code{data}.
#'                    \item \code{MP_A: } See \code{data}.
#'                 }
#'                 Can alternatively be a \code{character} string. See details.
#'
#' @param B An \code{integer}. The desired number of bootstrap resamples to be
#'          used for estimating the inside rates (commutability conclusion
#'          consistency) for the evaluated materials. If \code{NULL} (default),
#'          inside rates are not calculated.
#' @param method A \code{character} string. The method used to estimate
#'               prediction intervals. The default is \code{'fg'}. Current
#'               possible prediction interval estimation methods include:
#' \itemize{
#'   \item \code{fg: } Implements standard Deming regression with prediction
#'          intervals estimated using the Fuller and Gillard method.
#'   \item \code{clsi: } Utilizes standard Deming regression with prediction
#'                       intervals estimated using the CLSI EP14 approach.
#'   \item \code{ols: } Implements Ordinary Least Squares regression.
#'   \item \code{ss: } Implements Smoothing Splines with prediction intervals
#'                     approximated using an heuristic extension of the
#'                     bayesian confidence intervals derived by Grace Wahba.
#'                     See references.
#'   \item \code{ssw: } Implements Weighted Smoothing Splines with prediction
#'                      intervals approximated using a heuristic extension of
#'                      the bayesian confidence intervals derived by Grace
#'                      Wahba. See references.
#' }
#' @param level A \code{double}. Must be between \code{0} and \code{1}. The
#'              desired nominal confidence level for the estimated prediction
#'              intervals. Defaults to \code{0.99} (\eqn{99\%}).
#' @param rounding An \code{integer}. The number of decimals to include in the
#'                 \code{numeric} results. Defaults to \code{3L}.
#' @param override_R_ratio A \code{list} or \code{NULL} (default). When
#'                         provided as a \code{list}, it should include the
#'                         IVD-MD comparison-specific ratios of the number of
#'                         replicates between evaluated material data and CS
#'                         data.
#' @param na_rm A \code{logical} value. Indicates whether \code{NA} values are
#'              silently removed from \code{data} and \code{new_data}. Defaults
#'              to \code{TRUE}.
#'
#' @details
#'
#' The output of this function depends greatly on the input of \code{new_data}.
#' If \code{new_data} contains \code{SampleID} and \code{MP_A}, the function
#' returns the so-called CE (Commutability Evaluation) data. If either of these
#' are omitted, or if \code{new_data} is a \code{character} string, the
#' function returns the so-called PB (Prediction Band) data.
#'
#' Note: CE data is typically coded \code{ce_data}. PB data is often coded
#' \code{pb_data}. These codings are important to remember because they are
#' used in e.g., \code{do_commutability_evaluation()} and
#' \code{plot_commutability_evaluation_plots()}.
#'
#' When \code{new_data} is a \code{character} string it most be on the format
#' \code{'gen#number'} or \code{'number#gen'}, where \code{number} is an
#' integer. For example, setting \code{new_data = 'gen#100'} or
#' \code{new_data = '100#gen'} would instruct the function to generate
#' \code{100} pointwise prediction intervals uniformly distributed across the
#' domain of the IVD-MD measurements for each comparison.
#'
#' @return
#' A \code{data.table}. Depending on the structure of \code{new_data},
#' contains either commutability evaluation data (CE data) or prediction band
#' data (PB data).
#' @export
#'
#' @examples
#' # Required packages
#' library(smooth.commutability)
#' library(fasteqa)
#' library(data.table)
#'
#' test_crp_cs_data <- copy(crp_cs_data)
#' test_crp_eq_data <- copy(crp_eqam_data)
#'
#' # Estimate CE (Commutability Evaluation) data using weighted smoothing spline
#' print(estimate_prediction_data(data = test_crp_cs_data,
#'                                new_data = test_crp_eq_data[SampleID == "1"],
#'                                B = 50,
#'                                method = "ssw",
#'                                level = 0.99,
#'                                rounding = 3L,
#'                                override_R_ratio = NULL,
#'                                na_rm = TRUE))
#'
#' # Estimate PB (Prediction Band) data using weighted smoothing spline
#' pb_data <- estimate_prediction_data(data = test_crp_cs_data,
#'                                     new_data = "gen#20",
#'                                     B = NULL,
#'                                     method = "ssw",
#'                                     level = 0.99,
#'                                     rounding = 2L,
#'                                     override_R_ratio = NULL,
#'                                     na_rm = TRUE)
#'
#' # The PB data for the AQT90 - Chroma comparison
#' pb_data_AQT90_Chroma <- pb_data[comparison == "AQT90 - Chroma"]
#' print(pb_data_AQT90_Chroma)
#'
#' # Plotting the PB data for the AQT90 - Chroma comparison
#' mor_test_crp_cs_data <- test_crp_cs_data[comparison == "AQT90 - Chroma", fun_of_replicates(.SD)]
#' plot(x = mor_test_crp_cs_data$MP_B,
#'      y = mor_test_crp_cs_data$MP_A,
#'      main = "Prediction Band Data For AQT90 - Chroma",
#'      xlab = "Chroma",
#'      ylab = "AQT90",
#'      pch = 20)
#' lines(x = pb_data_AQT90_Chroma$predictor,
#'       y = pb_data_AQT90_Chroma$lwr,
#'       col = "red")
#' lines(x = pb_data_AQT90_Chroma$predictor,
#'       y = pb_data_AQT90_Chroma$upr,
#'       col = "red")
#'
estimate_prediction_data <- function(data,
                                     new_data = NULL,
                                     B = NULL,
                                     method = "fg",
                                     level = 0.99,
                                     rounding = 3L,
                                     override_R_ratio = NULL,
                                     na_rm = TRUE){

  # Binding global variables
  MP_B <- comparison <- inside <- SampleID <- NULL

  # Indicator variables
  inside_rate_successful <- FALSE
  inner_is_list <- TRUE


  # Validate 'data' argument
  data <- validate_data_epd(data = data, na_rm = na_rm)

  # If 'new_data' is NULL, 'new_data' uses the comparison-wise MP_B values of 'data'
  if(is.null(new_data)){
    new_data <- data[, list(MP_B = MP_B), by = comparison]
  }

  # If 'new_data' is a character string. Trigger gen_grid()
  else if(is.character(new_data)){
    n_gens <- extract_n_gens(x = new_data)
    new_data <- data[, list(MP_B = gen_grid(n = n_gens,
                                            lwr = min(MP_B, na.rm = TRUE),
                                            upr = max(MP_B, na.rm = TRUE))),
                     by = comparison]
  }

  # Validate 'new_data' argument
  new_data <- validate_new_data_epd(new_data = new_data, na_rm = na_rm)

  # Check if 'new_data' satisfies CE data requirements
  is_ce <- all(c("comparison", "SampleID", "MP_A", "MP_B") %in% names(new_data))

  # Check if 'new_data' satisfies PB data requirements
  is_pb <- (!is_ce) && all(c("comparison", "MP_B") %in% names(new_data))

  # Create lists grouped by comparison
  comparisons <- unique(data$comparison)
  data_list <- split(x = data, by = "comparison", keep.by = FALSE)
  new_data_list <- split(x = new_data, by = "comparison", keep.by = FALSE)
  names(data_list) <- names(new_data_list) <- comparisons

  # Check if names match and have the same order
  check_names_epd(data_list, new_data_list)

  # Calculate imprecision estimates
  impr_data <- lapply(X = data_list, FUN = global_precision_estimates)

  # Calculate number of replicates for each clinical sample
  R_cs_list <- lapply(X = data_list,
                      FUN = function(comp) {
                        count_samplewise_replicates(comp, "ceiling")$R_i
                      })
  # Convert to mean-of-replicates data
  mor_data_list <- lapply(X = data_list,
                          FUN = fun_of_replicates)

  # Calculate CE data if SampleID and ReplicateID is in new_data
  if (is_ce) {

    # Checks if ReplicateID is in new_data
    if (any("ReplicateID" == names(new_data))) {
      R_eq_list <- lapply(X = new_data_list,
                          FUN = function(comp) {
                            count_samplewise_replicates(comp, "ceiling")$R_i
                          })
      R_ratio_list <- sapply(X = seq_len(length(R_cs_list)),
                             FUN = function(comp){
                               R_cs_list[[comp]] / R_eq_list[[comp]]
                             },
                             simplify = FALSE)
      mor_new_data_list <- lapply(X = new_data_list,
                                  FUN = fun_of_replicates)
    }
    else {
      R_eq_list <- lapply(X = seq_len(length(R_cs_list)),
                          FUN = function(comp) {
                            if (!is.null(override_R_ratio)) {
                              R_cs_list[[comp]] / override_R_ratio[[comp]]
                            }
                            else {
                              R_cs_list[[comp]]
                            }
                          })
      R_ratio_list <- sapply(X = seq_len(length(R_cs_list)),
                             FUN = function(comp){
                               if (!is.null(override_R_ratio)) {
                                 override_R_ratio[[comp]]
                               }
                               else {
                                 1
                               }
                             },
                             simplify = FALSE)
      mor_new_data_list <- new_data_list

    }

    # Calculate inside rates if B is not NULL
    if(!is.null(B)){
      # Check if B is valid
      if(is.numeric(B) && length(B) == 1 && (!is.na(B)) && B >= 2){
        inside_rates <- tryCatch(expr = sapply(X = seq_len(length(data_list)),
                                               FUN = function(comp){
                                                 comp_data <- data_list[[comp]]
                                                 comp_new_data <- mor_new_data_list[[comp]]
                                                 comp_R_ratio <- R_ratio_list[[comp]]
                                                 comp_R_eq <- R_eq_list[[comp]]
                                                 bootstrap_inside_rate(
                                                   B = round(B),
                                                   data = comp_data,
                                                   new_data = comp_new_data,
                                                   R = comp_R_eq,
                                                   R_ratio = comp_R_ratio,
                                                   level = level,
                                                   method = method,
                                                   rounding = rounding
                                                 )
                                               },
                                               simplify = FALSE),
                                 error = function(e) NULL)

        if(!is.null(inside_rates)){
          inside_rate_successful <- TRUE
          names(inside_rates) <- comparisons
          inside_rates <- rbindlist(inside_rates, idcol = "comparison")
          inside_rates$inside_rate <- round(x = inside_rates$inside_rate,
                                            digits = rounding)
        }
        else{
          #stop(
          #  "Error: Could not calculate inside rates ..."
          #)
        }
      }
      else{
        stop(
          sprintf(
            "Error: B is not valid. It should be an integer equal to or larger than 2."
          )
        )
      }
    }
  }
  # According to the input, CE data is the most suitable output
  if (is_ce) {

    out <- NULL

    # For debugging. remove later.
    if(!exists(x = "mor_new_data_list")){
      stop(
        "Error: mor_new_data_list does not exist. This should not be possible!"
      )
    }

    if(method %in% c("ss", "ssw")){
      out <- sapply(X = seq_len(length(new_data_list)),
                    FUN = function(comp) {
                      predict_smoothing_spline(
                        data = mor_data_list[[comp]],
                        new_data = mor_new_data_list[[comp]],
                        weighted = (method == "ssw"),
                        df = NULL,
                        lambda = NULL,
                        df_max = 7.5,
                        R_ratio = R_ratio_list[[comp]],
                        level = level,
                        simultaneous = FALSE,
                        negative_ok = TRUE,
                        attempt_fast = FALSE,
                        include_prediction_variance = FALSE,
                        rounding = rounding
                      )
                    },
                    simplify = FALSE)
      inner_is_list <- FALSE
    }
    else{
      out <- sapply(X = seq_len(length(new_data_list)),
                    FUN = function(comp) {
                      domain_MP_B <- range(mor_data_list[[comp]]$MP_B,
                                           na.rm = TRUE)
                      evaluated_MP_B <- mor_new_data_list[[comp]]$MP_B
                      extrapolate <- as.integer(evaluated_MP_B < domain_MP_B[1] |
                                                  evaluated_MP_B > domain_MP_B[2])
                      c(
                        predict_eqa(
                          data = mor_data_list[[comp]],
                          new_data = mor_new_data_list[[comp]],
                          imprecision_estimates = impr_data[[comp]],
                          R = R_eq_list[[comp]],
                          R_ratio = R_ratio_list[[comp]],
                          method = method,
                          level = level,
                          allow_reverse_regression = FALSE,
                          rounding = rounding
                        ),
                        list(
                          "extrapolate" = extrapolate
                        )
                      )
                    },
                    simplify = FALSE)
    }

    names(out) <- comparisons
    if(inner_is_list){
      out <- lapply(out, setDT)
    }
    out <- rbindlist(out, idcol = "comparison")
    setcolorder(out, c("comparison",
                       "SampleID",
                       "MP_B",
                       "MP_A",
                       "prediction",
                       "lwr",
                       "upr",
                       "inside"))

    if(inside_rate_successful){
      out <- merge(out, inside_rates, by = c("comparison","SampleID"))
    }

    return(out)

  }

  # According to the input, PB data is the most suitable output
  else if (is_pb) {

    # Calculate R and R ratio components
    R_eq_list <- lapply(X = seq_len(length(R_cs_list)),
                        FUN = function(comp) {
                          if (!is.null(override_R_ratio)) {
                            R_cs_list[[comp]] / override_R_ratio[[comp]]
                          }
                          else {
                            R_cs_list[[comp]]
                          }
                        })

    R_ratio_list <- sapply(X = seq_len(length(R_cs_list)),
                           FUN = function(comp){
                             if (!is.null(override_R_ratio)) {
                               override_R_ratio[[comp]]
                             }
                             else {
                               1
                             }
                           },
                           simplify = FALSE)

    # If method is ss or ssw: use smoothing splines
    if (method %in% c("ss", "ssw")) {
      out <- sapply(X = seq_len(length(new_data_list)),
                    FUN = function(comp) {
                      predict_smoothing_spline(data = mor_data_list[[comp]],
                                               new_data = new_data_list[[comp]],
                                               weighted = method == "ssw",
                                               df = NULL,
                                               lambda = NULL,
                                               df_max = 7.5,
                                               R_ratio = R_ratio_list[[comp]],
                                               level = level,
                                               simultaneous = FALSE,
                                               negative_ok = TRUE,
                                               attempt_fast = FALSE,
                                               include_prediction_variance = FALSE,
                                               rounding = rounding)
                    },
                    simplify = FALSE)
      inner_is_list <- FALSE
    }
    # If method is something else, use linear regression
    else {
      out <- sapply(X = seq_len(length(new_data_list)),
                    FUN = function(comp) {
                      predict_eqa(data = mor_data_list[[comp]],
                                  new_data = new_data_list[[comp]],
                                  imprecision_estimates = impr_data[[comp]],
                                  R = round(R_eq_list[[comp]]),
                                  R_ratio = R_ratio_list[[comp]],
                                  method = method,
                                  level = level,
                                  allow_reverse_regression = FALSE,
                                  rounding = rounding)
                    },
                    simplify = FALSE)
    }

    names(out) <- comparisons
    if(inner_is_list){
      out <- lapply(X = out, FUN = setDT)
    }
    out <- rbindlist(out, idcol = "comparison")
  }

  else{
    stop(
      sprintf(
        "Error: Could not see that either CE data or PB data are suitable based on input new_data"
      )
    )
  }

  out <- list("comparison" = out$comparison,
              "predictor" = out$MP_B,
              "prediction" = out$prediction,
              "lwr" = out$lwr,
              "upr" = out$upr)
  setDT(out)
  return(out)

}



