#' Estimate prediction intervals for Deming regression based on clinical sample (CS) data and possibly external quality assessment (EQA) material data or certified reference (CR) material data
#'
#' @param data A \code{list}, \code{data.table} or \code{data.frame} containing the measurements of clinical samples grouped by \code{comparison}, \code{SampleID} and \code{ReplicateID}.
#' @param new_data A \code{list}, \code{data.table}, or \code{data.frame} with measurements of either external quality assessment materials, reference materials or new values for \code{MP_B}. If \code{SampleID}, \code{MP_A}, and \code{MP_B} columns are provided, the function will produce commutability evaluation data (CE data) based on the external quality assessment material measurements or reference material measurements. If \code{new_data = NULL}, it will generate prediction band data for the \code{MP_B} values of \code{data}. Alternatively, \code{new_data} can also be a \code{character} string in the format 'gen#number' (see details).
#' @param B An \code{integer} that specifies the number of bootstrap resamples to be used for estimating the inside rates (commutability conclusion consistency) for the external quality assessment materials. Set to \code{NULL} to exclude calculations of inside rates.
#' @param method A \code{character} string that denotes the method to be used for estimating the prediction intervals. The default is \code{'fg'}. Current possible prediction interval estimation methods include:
#' \itemize{
#'   \item{\code{fg}: }{Implements standard Deming regression with prediction intervals calculated using the Fuller and Gillard method.}
#'   \item{\code{clsi}: }{Utilizes standard Deming regression, with prediction intervals derived from the CLSI EP14 method.}
#'   \item{\code{ols}: }{Implements Ordinary Least Squares regression, where the predictor is selected to minimize the variance of the ignored IVD-MD uncertainty. Specifically, \code{MP_A} is used as the predictor when \code{lambda < 1}, otherwise, \code{MP_B} is utilized.}
#' }
#' @param level A \code{double} between 0 and 1, representing the confidence level of the estimated prediction intervals.
#' @param rounding An \code{integer} that defines the number of decimals to be returned in the output. The default is \code{3}.
#' @param override_R_ratio Either a \code{list} or \code{NULL}, with \code{NULL} being the default. When provided as a \code{list}, it should include the IVD-MD comparison-specific ratios of the number of replicates between EQA/CR material data and CS data. This parameter is beneficial when distinct IVD-MD comparisons have varying replicate counts among CS data and EQA/CR data, and you need to compute pointwise prediction intervals or prediction bands.
#' @param na_rm A \code{logical} that indicates whether NA-values should be silently removed from \code{data} and \code{new_data}. The default is \code{TRUE}.
#'
#' @details When \code{new_data} is a \code{character} string it most be on the format 'gen#number' or 'number#gen', where number is an integer. For example, setting \code{new_data = 'gen#100'} or \code{new_data = '100#gen'} would instruct the function to generate 100 pointwise prediction intervals uniformly distributed across the range of IVD-MD measurements for each comparison. Note that is is not yet possible to generate prediction band data using \code{method = 'ols'}.
#'
#' @return A \code{data.table} containing either commutability evaluation data (CE data) or prediction band data (PB data), depending on the content of \code{new_data}.
#' @export
#'
#' @examples print(1)
estimate_prediction_data <- function(data, new_data = NULL, B = NULL, method = "fg", level = 0.99, rounding = 3L, override_R_ratio = NULL, na_rm = TRUE){

  # Binding global variables
  MP_B <- comparison <- inside <- SampleID <- NULL

  # Checks if 'data' is of correct class. Should be data.table, but list and data.frame is also accepted
  # If 'data' is either list or data.frame, 'data' will be converted to data.table. If not, an error is thrown
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

  # Checks the column names of 'data': must contain:
  # 'comparison', SampleID', 'ReplicateID', 'MP_A' and 'MP_B'
  required_columns <- c('comparison', 'SampleID', 'ReplicateID', 'MP_A', 'MP_B')
  missing_columns <- setdiff(required_columns, names(data))

  # Throws an error if some column names are missing of 'data'
  if(length(missing_columns) > 0){
    stop(paste0("Some required columns are missing from 'data':", "\n",
                "* Missing column(s): [",
                paste(missing_columns, collapse=", "),
                "]", "\n",
                "* The argument 'data' must include 'comparison', 'SampleID', 'ReplicateID', 'MP_A' and 'MP_B'."))
  }
  # Removes NA values from 'data' if 'na_rm' = TRUE
  if(isTRUE(na_rm)){
    data <- na.omit(object = data)
  }

  # If 'new_data' is NULL, 'new_data' uses the comparison-wise MP_B values of 'data'
  if(is.null(new_data)){
    new_data <- data[, list(MP_B = MP_B), by = comparison]
  }
  # If 'new_data' is of character type it should be on the format 'gen[punctuation][number]', where number is an integer
  # Passing 'new_data' = 'gen[punctuation][number]' generate [number] pointwise prediction intervals uniformly over the concentration interval
  if(is.character(new_data)){
    split_string <- stri_split(new_data, regex = "[:punct:]")[[1]]
    converted_string <- split_string[split_string != ""]

    if(length(converted_string) != 2){
      stop(sprintf("Error: The input string '%s' was split into %d non-empty elements: [%s]. It is expected to split into exactly two non-empty elements, with the first being 'gen' and the second being an integer or vice versa. Calculations are terminated.",
                   new_data,
                   length(converted_string),
                   paste(converted_string, collapse=", ")))
    }

    if(converted_string[1] == "gen"){
      n_gens <- converted_string[2]
      n_gens_is_number <- stri_detect(str = n_gens, regex = "^[0-9]+$")

      # If n_gens contains anything else than just digits, we try to find solutions
      if(isFALSE(n_gens_is_number)){
        n_gens <- stri_replace_all_regex(converted_string[2], "[^0-9]", "")
        n_gens_is_number <- stri_detect(str = n_gens, regex = "^[0-9]+$")
        if(isFALSE(n_gens_is_number)){
          warning("The value following 'gen' should be a number. Defaulting to 100.")
          n_gens <- 1e2
        }
        else if(isTRUE(n_gens_is_number)){
          n_gens <- as.numeric(n_gens)
          if(n_gens < 2 || n_gens > 1e5){
            warning("The number following 'gen' should be between 2 and 100,000 (inclusive). Defaulting to 100.")
            n_gens <- 1e2
          }
        }
      }
      else{
        n_gens <- as.numeric(n_gens)
      }
      new_data <- data[, list(MP_B = seq(from = min(MP_B, na.rm = TRUE),
                                         to = max(MP_B, na.rm = TRUE),
                                         length.out = n_gens)),
                       by = comparison]
    }
    else if(converted_string[2] == "gen"){
      n_gens <- converted_string[1]
      n_gens_is_number <- stri_detect(str = n_gens, regex = "^[0-9]+$")

      # If n_gens contains anything else than just digits, we try to find solutions
      if(isFALSE(n_gens_is_number)){
        n_gens <- stri_replace_all_regex(converted_string[1], "[^0-9]", "")
        n_gens_is_number <- stri_detect(str = n_gens, regex = "^[0-9]+$")
        if(isFALSE(n_gens_is_number)){
          warning("The value before 'gen' should be a number. Defaulting to 100.")
          n_gens <- 1e2
        }
        else if(isTRUE(n_gens_is_number)){
          n_gens <- as.numeric(n_gens)
          if(n_gens < 2 || n_gens > 1e5){
            warning("The number before 'gen' should be between 2 and 100,000 (inclusive). Defaulting to 100.")
            n_gens <- 1e2
          }
        }
      }
      else{
        n_gens <- as.numeric(n_gens)
      }
      new_data <- data[, list(MP_B = seq(from = min(MP_B, na.rm = TRUE),
                                         to = max(MP_B, na.rm = TRUE),
                                         length.out = n_gens)),
                       by = comparison]
    }
    else{
      stop("Invalid format for 'new_data' when passed as a character. The valid syntax is either 'gen[punctuation][integer]' or '[integer][punctuation]gen'. In these expressions, [punctuation] stands for a punctuation symbol (e.g., '.', '-' ,'/', '#', etc.), and [integer] stands for an integer larger than 1.
         \nYour input: '", new_data, "' does not adhere to these rules.
         \nPlease revise your 'new_data' input and try again. The calculations cannot proceed in the current form.")
    }
  }

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

  # Checks the column names of 'new_data': must at least contain:
  # 'comparison', and 'MP_B'
  required_columns <- c("comparison", "MP_B")
  missing_columns <- setdiff(required_columns, names(new_data))

  # Throws an error if some column names are missing of 'new_data'
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

  # Make lists so that we can use mapply() later
  data_list <- split(x = data, by = "comparison", keep.by = FALSE)
  new_data_list <- split(x = new_data, by = "comparison", keep.by = FALSE)

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

  impr_data <- lapply(X = data_list, FUN = function(x) x[, global_precision_estimates(.SD)])
  R_cs_list <- lapply(X = data_list, FUN = function(x) count_samplewise_replicates(x, "ceiling")$R_i)

  # Convert to mean-of-replicates data
  mor_data_list <- lapply(X = data_list, FUN = fun_of_replicates)

  if(any("ReplicateID" == names(new_data))){
    if(any("SampleID" == names(new_data))){
      R_eq_list <- lapply(X = new_data_list, FUN = function(x) count_samplewise_replicates(x, "ceiling")$R_i)
      R_ratio_list <- mapply(function(x, y) x / y, R_cs_list, R_eq_list, SIMPLIFY = FALSE)
      mor_new_data_list <- lapply(X = new_data_list, FUN = fun_of_replicates)
      if(!is.null(B)){
        resample_data_list <- lapply(X = data_list, FUN = function(x) replicate(n = B, expr = resample_samples(x), simplify = FALSE))
        resample_impr_data_list <- lapply(X = resample_data_list, FUN = function(x) lapply(X = x, FUN = function(y) global_precision_estimates(y)))
        resample_mor_data_list <- lapply(X = resample_data_list, FUN = function(x) lapply(X = x, FUN = function(y) fun_of_replicates(y)))

        # For loop doing the thing
        #resample_predictions <- as.list(1:length(data_list))
        #resample_predictions <- lapply(resample_predictions, function(x) numeric(B))
        #
        #for(j in 1:length(data_list)){
        #  for(b in 1:B){
        #    resample_predictions[[j]][[b]] <- predict_eqa(data = resample_mor_data_list[[j]][[b]],
        #                                                  new_data = mor_new_data_list[[j]],
        #                                                  imprecision_estimates = resample_impr_data_list[[j]][[b]],
        #                                                  R = R_eq_list[[j]],
        #                                                  R_ratio = R_ratio_list[[j]],
        #                                                  method = method,
        #                                                  level = level,
        #                                                  rounding = 3L)
        #  }
        #}

        # Do B resamples of predictions for each unique IVD-MD comparison
        # The output of this will be a nested list where for each unique IVD-MD comparison, there will be B lists.
        resample_predictions <- mapply(FUN = function(resampled_mor_data, mor_new_data, resampled_impr_data, R, R_ratio)
          mapply(FUN = function(bth_resampled_mor_data, bth_resampled_impr_data)
            predict_eqa(data = bth_resampled_mor_data,
                        new_data = mor_new_data,
                        imprecision_estimates = bth_resampled_impr_data,
                        R = R,
                        R_ratio = R_ratio,
                        method = method,
                        level = level,
                        rounding = rounding),
                 resampled_mor_data,
                 resampled_impr_data,
                 SIMPLIFY = FALSE),
          resample_mor_data_list,
          mor_new_data_list,
          resample_impr_data_list,
          R_eq_list,
          R_ratio_list,
          SIMPLIFY = FALSE)

        resample_predictions <- lapply(resample_predictions, FUN = function(x) rbindlist(lapply(x, FUN = setDT)))
        resample_predictions <- rbindlist(resample_predictions, idcol = "comparison")
        inside_rates <- resample_predictions[, list(inside_rate = mean(inside, na.rm = TRUE)),
                                             by = list(comparison, SampleID)]
      }
    }
    else{
      stop("'ReplicateID' was found in 'new_data', but 'SampleID' was not. Both or none must be given in 'new_data'")
    }
  }

  # Depending on the names of 'new_data_list', we can deduce whether commutability evaluation is performed
  # or whether if we just seek to estimate the pointwise prediction intervals

  # 'is_ce' is TRUE if all elements of 'new_data_list' have the columns 'SampleID', 'MP_A' and 'MP_B'
  is_ce <- lapply(X = new_data_list, FUN = function(x) all(c("SampleID", "MP_A", "MP_B") %in% names(x)))
  is_ce <- all(unlist(is_ce))
  if(isFALSE(is_ce)){
    # 'is_pb' is TRUE if 'is_ce' is FALSE and all elements of 'new_data_list' have the column 'MP_B'
    is_pb <- lapply(X = new_data_list, FUN = function(x) any("MP_B" == names(x)))
    is_pb <- all(unlist(is_pb))
  }
  else if(isTRUE(is_ce)){
    out <- mapply(FUN = function(mor_data, mor_new_data, impr_estimates, R, R_ratio)
      predict_eqa(data = mor_data,
                  new_data = mor_new_data,
                  imprecision_estimates = impr_estimates,
                  R = R,
                  R_ratio = R_ratio,
                  method = method,
                  level = level,
                  rounding = rounding),
              mor_data_list,
              mor_new_data_list,
              impr_data,
              R_eq_list,
              R_ratio_list,
              SIMPLIFY = FALSE)

    out <- lapply(out, function(x) setDT(x))
    out <- rbindlist(out, idcol = "comparison")
    setcolorder(out, c("comparison", "SampleID", "MP_B", "MP_A",
                       "prediction", "lwr", "upr", "inside"))

    if(!is.null(B)){
      out <- merge(out, inside_rates, by = c("comparison","SampleID"))
    }

    return(out)

  }

  if(isTRUE(is_pb)){
    if(!is.null(override_R_ratio)){
      out <- mapply(FUN = function(mor_data, mor_new_data, impr_estimates, R, R_ratio)
        predict_eqa(data = mor_data,
                    new_data = mor_new_data,
                    imprecision_estimates = impr_estimates,
                    R = R,
                    R_ratio = R_ratio,
                    method = method,
                    level = level,
                    rounding = rounding),
        mor_data_list,
        new_data_list,
        impr_data,
        mapply(function(R_cs, R_rat) round(R_cs / R_rat), R_cs_list, override_R_ratio, SIMPLIFY = FALSE),
        override_R_ratio,
        SIMPLIFY = FALSE)
      out <- lapply(out, function(x) setDT(x))
      out <- rbindlist(out, idcol = "comparison")
    }
    else{
      out <- mapply(FUN = function(mor_data, mor_new_data, impr_estimates, R)
        predict_eqa(data = mor_data,
                    new_data = mor_new_data,
                    imprecision_estimates = impr_estimates,
                    R = R,
                    R_ratio = 1,
                    method = method,
                    level = level,
                    rounding = rounding),
        mor_data_list,
        new_data_list,
        impr_data,
        R_cs_list,
        SIMPLIFY = FALSE)
      out <- lapply(out, function(x) setDT(x))
      out <- rbindlist(out, idcol = "comparison")
    }


    out <- list("comparison" = out$comparison, "predictor" = out$MP_B, "prediction" = out$prediction, "lwr" = out$lwr, "upr" = out$upr)
    setDT(out)
    return(out)
  }
  else{
    stop("new_data is not on the correct form")
  }
}

