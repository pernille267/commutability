#' Perform linear model assessment tests
#'
#' @param data A \code{data.table}, \code{list}, or \code{data.frame} containing the clinical sample data, grouped by 'comparison', 'SampleID' and 'ReplicateID'. All replicated measurements must be included in the data for accurate computation of imprecision estimates.
#' @param B An \code{integer} representing the number of bootstrap replicates used in calculating the rejection rates of assessment tests. A value of 1 or more is required when \code{include_rejection_rates} is set to \code{TRUE}. If \code{include_rejection_rates} is \code{FALSE}, this parameter is ignored. Please avoid setting \code{B} to an integer above 100,000 due to the resulting intensive computational time.
#' @param method A \code{character} string that specifies the linear regression model for calculating residuals in the Shapiro-Wilk normality tests. The possible options are \code{'fg'} (default), \code{'clsi'}, and \code{'ols'}. The \code{'fg'} option uses Deming regression as defined by Fuller & Gillard, \code{'clsi'} uses Deming regression according to the CLSI EP14 standard, and \code{'ols'} uses ordinary least squares regression. This parameter does not affect the Breusch-Pagan homoscedasticity tests.
#' @param level A \code{double} value (between 0 and 1) that represents 1 - significance level of the assessment tests. The default value is \code{0.95}, corresponding to a test significance level of \code{0.05}.
#' @param include_rejection_rates This \code{logical} parameter, when set to \code{TRUE} (default), includes the assessment test rejection rates in the output. If set to \code{FALSE}, the function will bypass any bootstrap estimation and exclude rejection rates from the output.
#' @param simultaneous_testing This \code{logical} parameter, when set to \code{TRUE} (default), triggers correction for multiple testing. Conversely, when set to \code{FALSE}, 1 - level is applied uniformly for all assessment tests without considering multiple testing.
#' @param na_rm A non-missing \code{logical} value. If set to \code{TRUE}, the function will discard \code{NA} values from \code{data} before performing assessment test calculations.
#' @param silence This \code{integer} parameter controls the verbosity of the function. If \code{silence} is 1 or more, verbose output is disabled. If \code{silence} is less than 1, verbose output is enabled.
#'
#' @return A \code{data.table} with columns 'comparison', 'test_name', 'test', 'p.value', and 'conclusion' showing the results of the performed assessment tests. If \code{include_rejection_rates} is set to \code{TRUE} and \code{B} is an integer value of 1 or more, an additional column 'rejection_rate' is included in the output.
#' @export
#'
#' @examples print(1)

perform_assessment_tests <- function(data, B = 2e2L, method = "fg", level = 0.95, include_rejection_rates = TRUE, simultaneous_testing = TRUE, na_rm = FALSE, silence = 1L){

  # Handling global variables
  SampleID <- ReplicateID <- conclusion <- p.value <- NULL

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

  if(isTRUE(na_rm)){
    data <- na.omit(data)
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

  # Check if 'SampleID' and 'ReplicateID' are character strings, and convert them if they are not
  if(isFALSE(is.character(data$SampleID))){
    data[, SampleID := as.character(SampleID)]
  }
  if(isFALSE(is.character(data$ReplicateID))){
    data[, ReplicateID := as.character(ReplicateID)]
  }

  # Check validity of 'level'. It is expected to be a numeric value between 0 and 1, exclusive.
  if(length(level) > 1L){
    level <- level[1]
  }
  invalid_level <- !is.numeric(level) || is.null(level) || is.na(level) || level <= 0 || level >= 1
  if(isTRUE(invalid_level)){
    stop(sprintf("The input 'level' [%s] is invalid. It is expected to be a non-missing numeric value strictly between 0 and 1.",
                 if (is.null(level)) {"NULL"} else {paste(capture.output(dput(level)), collapse = "")}))
  }

  # Check validity of 'include_rejection_rates'. It is expected to be a non-missing logical value
  if(length(include_rejection_rates) > 1L){
    include_rejection_rates <- include_rejection_rates[1]
  }
  invalid_include_rejection_rates <- is.null(include_rejection_rates) || is.na(include_rejection_rates) || !is.logical(include_rejection_rates)
  if(isTRUE(invalid_include_rejection_rates)){
    stop(sprintf("The input 'include_rejection_rates' [%s] is invalid. It is expected to be a single, non-missing logical value, i.e., TRUE or FALSE.",
                 if (is.null(include_rejection_rates)) {"NULL"} else {paste(capture.output(dput(include_rejection_rates)), collapse = "")}))
  }

  # Check validity of 'simultaneous_testing'. It is expected to be a non-missing logical value
  if(length(simultaneous_testing) > 1L){
    simultaneous_testing <- simultaneous_testing[1]
  }
  invalid_simultaneous_testing <- is.null(simultaneous_testing) || is.na(simultaneous_testing) || !is.logical(simultaneous_testing)
  if(isTRUE(invalid_simultaneous_testing)){
    stop(sprintf("The input 'simultaneous_testing' [%s] is invalid. It is expected to be a single, non-missing logical value, i.e., TRUE or FALSE.",
                 if (is.null(simultaneous_testing)) {"NULL"} else {paste(capture.output(dput(simultaneous_testing)), collapse = "")}))
  }

  data_list <- split(x = data, by = "comparison", keep.by = FALSE)
  data_list_mor <- lapply(X = data_list, FUN = fun_of_replicates)
  impr_list <- lapply(X = data_list, FUN = global_precision_estimates)

  # Bonferroni-correct significance level 'alpha' if 'simultaneous_testing' is set to TRUE
  effective_alpha <- 1 - level
  if(isTRUE(simultaneous_testing)){
    effective_alpha <- effective_alpha / length(data_list)
  }

  # Original normality test results for 'data'
  original_normality_tests <- mapply(FUN = function(mor_data, imprecision_data)
    residuals_eqa(data = mor_data, imprecision_estimates = imprecision_data, method = method),
    data_list_mor,
    impr_list,
    SIMPLIFY = FALSE)
  original_normality_tests <- lapply(X = original_normality_tests,
                                     FUN = function(x) list("test_name" = "Shapiro-Wilk",
                                                            "test" = "normality",
                                                            "p.value" = shapiro.test(x$residuals)$p.value))
  original_normality_tests <- rbindlist(original_normality_tests, idcol = "comparison")
  original_normality_tests[, conclusion := ifelse(p.value < effective_alpha, "normality rejected", "normality not rejected")]

  # Original homoscedasticity test results for 'data'
  original_homoscedasticity_tests <- lapply(X = data_list_mor,
                                            FUN = function(x)
                                              list("test_name" = "Breusch-Pagan",
                                                   "test" = "variance homogeneity",
                                                   "p.value" = breusch_pagan(list("X" = cbind(1, x$MP_B), "y" = x$MP_A))$p.value))
  original_homoscedasticity_tests <- rbindlist(original_homoscedasticity_tests, idcol = "comparison")
  original_homoscedasticity_tests[, conclusion := ifelse(p.value < effective_alpha, "variance homogeneity rejected", "variance homogeneity not rejected")]

  if(isTRUE(include_rejection_rates)){
    if(length(B) > 1L){
      B <- B[1]
    }
    B_NULL_or_NA <- is.null(B) || is.na(B)
    B_character <- is.character(B)
    B_logical <- !is.na(B) && is.logical(B)
    B_zero_or_negative <- !is.na(B) && is.numeric(B) && is.finite(B) && B <= 0
    B_inf <- !is.na(B) && is.numeric(B) && is.infinite(B)
    B_decimal <- !is.na(B) && is.numeric(B) && is.finite(B) && abs(B - round(B)) > .Machine$double.eps ** 0.5
    invalid_B <- B_NULL_or_NA || B_character || B_logical || B_zero_or_negative || B_inf
    if(isTRUE(invalid_B)){
      stop(sprintf(paste("Invalid input for 'B'. Expected an finite integer greater than or equal to 1, but received [%s] of type '%s'.",
                         "If 'include_rejection_rates' is TRUE, 'B' must be of the correct type.",
                         "To exclude estimates of test rejection rates, set 'include_rejection_rates' to FALSE."),
                   if (is.null(B)) {"NULL"} else {paste(capture.output(dput(B)), collapse = "")},
                   class(B)[1]))
    }
    else if(isTRUE(B_decimal)){
      if(B > 0 && B <= 0.5){
        B <- 1L
      }
      else if(B > 0.5 && B <= 1e5L){
        B <- round(B + .Machine$double.eps ** 0.5)
      }
      else{
        warning(sprintf(paste("The input 'B' [%d] is greater than 100,000, indicating a potential lengthy computation time.",
                              "If this large 'B' value was unintended, consider interrupting the execution to avoid unnecessary delays.",
                              "The accuracy gain from using such a large 'B' is typically insufficient relative to the increased computation time.",
                              "If the 'B' value was intended, feel free to ignore this message."),
                        B), immediate. = TRUE)
        B <- round(B + .Machine$double.eps ** 0.5)
      }
    }
  }


  if(isTRUE(include_rejection_rates)){

    resampled_data <- lapply(data_list, FUN = function(x) replicate(n = B, expr = resample_samples(x), simplify = FALSE))
    resampled_mor_data <- lapply(resampled_data, function(x) lapply(x, fun_of_replicates))
    resampled_impr_data <- lapply(resampled_data, function(x) lapply(x, global_precision_estimates))

    resampled_residuals_vs_fitted <- as.list(1:length(resampled_data))
    names(resampled_residuals_vs_fitted) <- names(resampled_data)

    for(i in 1:length(resampled_data)){
      resampled_residuals_vs_fitted[[i]] <- mapply(FUN = function(mor_data, impr_data){
        residuals_eqa(mor_data, impr_data, method)},
        resampled_mor_data[[i]],
        resampled_impr_data[[i]],
        SIMPLIFY = FALSE
      )
    }

    resampled_normality_tests <- lapply(X = resampled_residuals_vs_fitted, FUN = function(x)
      lapply(x, FUN = function(y)
        shapiro.test(y$residuals)$p.value))

    resampled_homoscedasticity_tests <- lapply(X = resampled_mor_data, FUN = function(x)
      lapply(x, FUN = function(y)
        breusch_pagan(list("X" = cbind(1, y$MP_B),
                           "y" = y$MP_A))$p.value))

    rejection_rates_normality_tests <- lapply(X = resampled_normality_tests, FUN = function(x)
      list("rejection_rate" = mean(unlist(x, use.names = FALSE) < effective_alpha, na.rm = TRUE)))
    rejection_rates_normality_tests <- rbindlist(rejection_rates_normality_tests, idcol = "comparison")

    rejection_rates_homoscedasticity_tests <- lapply(X = resampled_homoscedasticity_tests, FUN = function(x)
      list("rejection_rate" = mean(unlist(x, use.names = FALSE) < effective_alpha, na.rm = TRUE)))
    rejection_rates_homoscedasticity_tests <- rbindlist(rejection_rates_homoscedasticity_tests, idcol = "comparison")

  }

  if(isTRUE(include_rejection_rates)){
    normality_tests <- merge(original_normality_tests, rejection_rates_normality_tests, by = "comparison")
    homoscedasticity_tests <- merge(original_homoscedasticity_tests, rejection_rates_homoscedasticity_tests, by = "comparison")
  }
  else{
    normality_tests <- original_normality_tests
    homoscedasticity_tests <- original_homoscedasticity_tests
  }

  # Combine test normality test results 'normality_tests' with homoscedasticity test results 'homoscedasticity_tests' using rbind().
  all_test_results <- rbind(normality_tests, homoscedasticity_tests)

  # Checks the class of the 'p.value' column of the 'all_test_results' data.table object.
  p.value_NULL <- is.null(all_test_results$p.value) || length(all_test_results$p.value) <= 1L
  p.value_not_numeric <- !is.numeric(all_test_results$p.value)

  # 'NULL', or not 'numeric' are deemed invalid classes for 'p.value'
  invalid_p.value_vector <- p.value_NULL || p.value_not_numeric

  # Throws errors if p.value vector of 'all_test_results' are of incorrect class
  if(isTRUE(invalid_p.value_vector)){
    if(isTRUE(p.value_NULL)){
      stop(sprintf(paste("None or only one of the test p-values were calculated, which indicates a computational or data issue that needs to be addressed.",
                         "The reason for not calculating p-values is not known at this point!")))
    }
    else{
      stop(sprintf(paste("None of the test p-values are 'numeric' presenting, but rather of class '%s' or missing.",
                         "The reason for this class mismatch is a computational or data issue that needs to be addressed."), class(all_test_results$p.value)[1]))
    }
  }

  # Checks individual p.value elements whether they are 'NA' or exceeding the acceptable range for p-values.
  invalid_p.value_elementwise <- sapply(X = all_test_results$p.value, FUN = function(x){
    p.value_NA <- is.na(x)
    p.value_negative <- !is.na(x) && is.numeric(x) && x < 0
    p.value_larger_than_one <- !is.na(x) && is.numeric(x) && x > 1
    return(p.value_NA || p.value_negative || p.value_larger_than_one)
  })

  # Throws an error if any of the elements of the 'p.value' vector is 'NA', smaller than 0 or larger than 1.
  if(isTRUE(any(invalid_p.value_elementwise))){
    stop(sprintf(paste("Invalid p-values detected in the assessment tests' results.",
                       "The problematic p-values are: [%s]. These correspond to the IVD-MD comparisons: [%s], and the assessment tests: [%s].",
                       "Please note that a valid p-value should be a finite numeric value between 0 and 1, inclusive.",
                       "P-values that are missing, less than 0, or greater than 1, may indicate a computational or data issue that needs to be addressed."),
                 paste(round(all_test_results$p.value[invalid_p.value_elementwise], 3L), collapse = ", "),
                 paste(all_test_results$comparison[invalid_p.value_elementwise], collapse = ", "),
                 paste(all_test_results$test[invalid_p.value_elementwise], collapse = ", ")))
  }

  # Round all p-values to three digits, and those p-values smaller than 0.001 will be truncated to '< 0.001' to enhance readability
  all_test_results$p.value <- ifelse(all_test_results$p.value < 1e-3, "< 0.001", as.character(round(all_test_results$p.value, 3L)))

  return(all_test_results)
}



