#' Validate and Converts \code{data}
#'
#' @description
#' This function is used to validate and convert the \code{data} argument
#' in \code{perform_assessment_tests}.
#'
#' @param data A object to be validated and converted.
#'
#' @return The converted \code{data}
#' @keywords internal
validate_data_pat <- function(data) {

  # Binding of global variables
  SampleID <- ReplicateID <- NULL

  # Avoid modifying the original data
  data <- copy(data)

  # Checks if 'data' is of correct class
  if(!is.data.table(data)){
    if(is.data.frame(data) || is.list(data)){
      setDT(data)
    }
    else{
      stop(
        "The input 'data' is neither a data.table, list, nor data.frame.",
        " Registered input class of 'data': '",
        class(data)[1], "'.",
        " Please provide an input of type data.table, list, or data.frame to proceed.",
        " The calculations cannot continue with the current input type."
      )

    }
  }

  # Checks if 'data' have the required columns
  required_columns <- c('comparison', 'SampleID', 'ReplicateID', 'MP_A', 'MP_B')
  missing_columns <- setdiff(required_columns, names(data))
  if(length(missing_columns) > 0){
    stop(
      "Some required columns are missing from 'data': ",
      "Missing column(s): [",
      paste(missing_columns, collapse=", "),
      "]",
      " The data argument must include 'comparison', 'SampleID', ",
      "'ReplicateID', 'MP_A' and 'MP_B'"
    )
  }

  # Handle NA-values
  valid_rows <- (!is.na(data$MP_A)) & (!is.na(data$MP_B))
  if (sum(valid_rows) >= 5) {
    data <- data[which(valid_rows), ]
  }
  else {
    stop(
      "There are fewer than 5 valid observations after removing NA-values ",
      "from 'data'. Calculations are terminated."
    )
  }

  # Check and possible convert classes of SampleID and Replicates
  if(!is.character(data$SampleID)){
    data[, SampleID := as.character(SampleID)]
  }
  if(!is.character(data$ReplicateID)){
    data[, ReplicateID := as.character(ReplicateID)]
  }

  return(data)

}

#' Validate and Convert \code{level}
#'
#' @description
#' This function is used to validate and coerce the \code{level} argument
#' in \code{perform_assessment_tests}.
#'
#' @param level A \code{double}. See \code{?perform_assessment_tests()}.
#'
#' @return The validated and possibly coerced \code{level}
#' @keywords internal
validate_level_pat <- function(level) {

  # Check if not numeric
  if (!is.numeric(level)) {
    warning(
      "level is expected to be a numeric, but got a ",
      class(level),
      ".",
      " The default value level = 0.95 is used instead."
    )
    level <- 0.95
  }

  # Check if length is different from 1
  if (length(level) != 1) {
    if (length(level) > 1) {
      warning(
        "level is of length ",
        length(level),
        ".",
        " Only the first value of level is used."
      )
      level <- level[1]
    }
    else {
      warning(
        "level is expected to be a double, but got an empty ",
        "numeric vector.",
        ".",
        " The default value level = 0.95 is used instead."
      )
      level <- 0.95
    }
  }

  # Check if level is NA
  if (is.na(level)) {
    warning(
      "level is expected to be a double, but got NA.",
      " The default value level = 0.95 is used instead."
    )
    level <- 0.95
  }

  # Check if level is between 0 and 1.
  if (level < 0 || level > 1) {
    stop(
      "level is expected to be a double between 0 and 1, but got ",
      level,
      ", which is clearly not within the allowable interval."
    )
  }

  return(level)



}

#' Validate and Convert \code{B}
#'
#' @description
#' This function is used to validate and coerce the \code{B} argument
#' in \code{perform_assessment_tests}.
#'
#' @param B An \code{integer}. See \code{?perform_assessment_tests()}.
#'
#' @return The validated and possibly coerced \code{B}
#' @keywords internal
validate_B_pat <- function(B) {

  # B can be NULL, NA_REAL_ or a double
  # doubles will be coerced to integer using rounding

  # Check if NULL
  if (is.null(B)) {
    B <- NULL
    return(B)
  }

  # Check if not numeric
  else if (!is.numeric(B)) {
    stop(
      "B is expected to be an integer, but got a ",
      class(B),
      " ('",
      B,
      "')."
    )
  }

  # Check if length is different from 1
  if (length(B) != 1) {
    if (length(B) > 1) {
      warning(
        "B is of length ",
        length(B),
        ".",
        " Only the first value of B is used."
      )
      B <- B[1]
    }
    else {
      stop(
        "B is expected to be an integer, but got an empty ",
        class(B),
        " vector."
      )
    }
  }

  # Check if B is NA
  if (is.na(B)) {
    B <- NULL
  }

  # Early Quit if B is NULL
  if (is.null(B)) {
    return(B)
  }

  # In the worst case, B should now be a double, and in the best case: integer.
  input_B <- B

  # Round B and convert to integer
  B <- tryCatch(
    expr = {
      as.integer(round(B))
    },
    error = function(e) "error",
    warning = function(w) "warning"
  )

  # Attempt to coerce B to an integer
  if (B == "error" || B == "warning") {
    stop(
      "Attempted to coerce B = ",
      input_B,
      " to an integer, but failed."
    )
  }

  # Check if B is too small
  if (B < 10) {
    warning(
      "B is too small (< 10): Input is ",
      input_B,
      " (coerced to: ",
      B,
      "L). ",
      "rejection_rate is not calculated."
    )
    B <- NULL
  }

  else if(B > 1e4) {
    warning(
      "B is exceedingly large (> 1e4L): Input is ",
      input_B,
      " (coerced to: ",
      B,
      "L). ",
      "B = 1e4L is used instead."
    )
    B <- 1e4L
  }

  return(B)

}

#' Execute Normality Tests for \code{data}
#'
#' @description
#' This function is used to perform the normality tests that is part of the
#' output of \code{perform_assessment_tests}.
#'
#' @param data See \code{?perform_assessment_tests()}.
#' @param method See \code{?perform_assessment_tests()}.
#' @param level See \code{?perform_assessment_tests()}.
#' @param B See \code{?perform_assessment_tests()}.
#' @param n_sim_tests An \code{integer}. The number of simultanuous tests to
#'                    perform for normality and homoscedasticity separately.
#' @param buffer A \code{double}. Must be non-negative. The percentage increase
#'               in effective \code{B}. May be relevant if the inner
#'               algorithm is prone to producing NAs.
#'
#' @return A \code{data.table}. The Shapiro-Wilk normality test results.
#' @keywords internal
get_normality_test_results <- function(data,
                                       method,
                                       level = 0.95,
                                       B = NULL,
                                       n_sim_tests = 10,
                                       buffer = 0.05) {

  # Avoid modifying original data
  data <- copy(data)

  # Output values
  test_name <- "Shapiro-Wilk"
  orig_pval <- NULL
  rejection_rate <- NULL
  conclusion <- NULL

  # Linear modelling
  if (any(method == c("fg", "clsi", "ols"))) {
    impr_data <- global_precision_estimates(data)
    mor_data <- fun_of_replicates(data)

    orig_residuals <- tryCatch(
      expr = {
        residuals_eqa(data = mor_data,
                      imprecision_estimates = impr_data,
                      method = method,
                      studentize = TRUE,
                      unit_sd = FALSE,
                      invalid_NA = TRUE)$residuals
      },
      error = function(e) NULL,
      warning = function(e) NULL
    )

    # Early exit if residuals cannot be calculated for original data
    if (is.null(orig_residuals)) {
      warning(
        "Could not calculate the Shapiro-Wilk p-value for this IVD-MD comparison. ",
        "NA values are returned for this comparison."
      )
      if (!is.null(B)) {
        output <- list("testing" = "normality",
                       "test_name" = test_name,
                       "p.value" = NA_real_,
                       "conclusion" = NA_character_,
                       "rejection_rate" = NA_real_)

        return(output)
      }
      output <- list("testing" = "normality",
                     "test_name" = test_name,
                     "p.value" = NA_real_,
                     "conclusion" = NA_character_)

      return(output)

    }

    orig_pval <- shapiro.test(orig_residuals)$p.value * n_sim_tests

    if (!is.null(B)) {
      replicated_pvals <- replicate(
        n = ceiling(B * (1 + buffer)),
        expr = {
          bdata <- resample_samples(data)
          bimpr_data <- global_precision_estimates(bdata)
          bmor_data <- fun_of_replicates(bdata)
          bresiduals <- tryCatch(
            expr = {
              residuals_eqa(data = bmor_data,
                            imprecision_estimates = bimpr_data,
                            method = method,
                            studentize = TRUE,
                            unit_sd = FALSE,
                            invalid_NA = TRUE)$residuals
            },
            error = function(e) NULL,
            warning = function(w) NULL
          )

          if (is.null(bresiduals)) {
            return(NA_real_)
          }

          return(shapiro.test(bresiduals)$p.value * n_sim_tests)
        },
        simplify = TRUE
      )

      replicated_pvals <- replicated_pvals[!is.na(replicated_pvals)]
      if (length(replicated_pvals) > B) {
        replicated_pvals <- replicated_pvals[1:B]
      }
      rejection_rate <- mean(replicated_pvals < 1 - level)

    }
  }

  else if (any(method == c("ss", "ssw"))) {
    weighting <- switch(method,
                        "ss" = 1,
                        "ssw" = "estimate",
                        1)
    mor_data <- fun_of_replicates(data)
    ss_fit <- tryCatch(
      expr = {
        smoothing_spline(data = mor_data,
                         weights = weighting,
                         df = NULL,
                         lambda = NULL,
                         df_max = 7.5,
                         na_rm = TRUE)
      },
      error = function(e) NULL,
      warning = function(w) NULL
    )

    # Early exit if residuals cannot be calculated for original data
    if (is.null(ss_fit)) {
      warning(
        "Could not calculate the Shapiro-Wilk p-value for this IVD-MD comparison. ",
        "NA values are returned for this comparison."
      )
      if (!is.null(B)) {
        output <- list("testing" = "normality",
                       "test_name" = test_name,
                       "p.value" = NA_real_,
                       "conclusion" = NA_character_,
                       "rejection_rate" = NA_real_)

        return(output)
      }
      output <- list("testing" = "normality",
                     "test_name" = test_name,
                     "p.value" = NA_real_,
                     "conclusion" = NA_character_)

      return(output)

    }

    orig_df <- ss_fit$df
    orig_residuals <- ss_fit$residuals * sqrt(ss_fit$weights) / sqrt(ss_fit$var_eps * (1 - ss_fit$df / length(ss_fit$u)))
    orig_pval <- shapiro.test(orig_residuals)$p.value * n_sim_tests

    if (!is.null(B)) {
      replicated_pvals <- replicate(
        n = ceiling(B * (1 + buffer)),
        expr = {
          bmor_data <- resample_fun_of_samples(mor_data)
          bresiduals <- tryCatch(
            expr = {
              ss_fit <- smoothing_spline(data = bmor_data,
                                         weights = weighting,
                                         df = ifelse(orig_df <= 2.2, 2, orig_df),
                                         lambda = NULL,
                                         df_max = 7.5,
                                         na_rm = TRUE)
              ss_fit$residuals * sqrt(ss_fit$weights) / sqrt(ss_fit$var_eps * (1 - ss_fit$df / length(ss_fit$u)))

            },
            error = function(e) NULL,
            warning = function(w) NULL
          )

          if (is.null(bresiduals)) {
            return(NA_real_)
          }

          return(shapiro.test(bresiduals)$p.value * n_sim_tests)
        },
        simplify = TRUE
      )

      replicated_pvals <- replicated_pvals[!is.na(replicated_pvals)]
      if (length(replicated_pvals) > B) {
        replicated_pvals <- replicated_pvals[1:B]
      }
      rejection_rate <- mean(replicated_pvals < 1 - level)

    }
  }

  if (!is.null(orig_pval)) {
    conclusion <- if (orig_pval < 1 - level) {"reject"} else {"not reject"}
  }


  if (!is.null(rejection_rate)) {
    output <- list("testing" = "normality",
                   "test_name" = test_name,
                   "p.value" = orig_pval,
                   "conclusion" = conclusion,
                   "rejection_rate" = rejection_rate)

    return(output)
  }

  output <- list("testing" = "normality",
                 "test_name" = test_name,
                 "p.value" = orig_pval,
                 "conclusion" = conclusion)

  return(output)


}

#' Execute Heteroscedasticity Tests for \code{data}
#'
#' @description
#' This function is used to perform the heteroscedasticity tests that is
#' part of the output of \code{perform_assessment_tests}.
#'
#' @param data See \code{?perform_assessment_tests()}.
#' @param level See \code{?perform_assessment_tests()}.
#' @param B See \code{?perform_assessment_tests()}.
#' @param koenker See \code{robust} in \code{?perform_assessment_tests()}.
#' @param n_sim_tests An \code{integer}. The number of simultanuous tests to
#'                    perform for normality and homoscedasticity separately.
#' @param buffer A \code{double}. Must be non-negative. The percentage increase
#'               in effective \code{B}. May be relevant if the inner
#'               algorithm is prone to producing NAs.
#'
#' @return
#' A \code{data.table}. The Breusch-Pagan heteroscedasticity test results.
#' @keywords internal
get_heteroscedasticity_test_results <- function(data,
                                                level = 0.95,
                                                B = NULL,
                                                koenker = TRUE,
                                                n_sim_tests = 10,
                                                buffer = 0.05) {

  # Avoid modifying original data
  data <- copy(data)

  # Output values
  test_name <- "Breusch-Pagan"
  orig_pval <- NULL
  rejection_rate <- NULL
  conclusion <- NULL

  # Get MOR data
  mor_data <- fun_of_replicates(data)

  # Attempt to get p-value based on original data
  orig_pval <- tryCatch(
    expr = {
      bp_test(data = mor_data, koenker = koenker) * n_sim_tests
    },
    error = function(e) NULL,
    warning = function(e) NULL
  )

  # Early exit if original p-value cannot be calculated
  if (is.null(orig_pval)) {
    warning(
      "Could not calculate the Breusch-Pagan p-value for this IVD-MD comparison. ",
      "NA values are returned for this comparison."
    )

    if (!is.null(B)) {
      output <- list("testing" = "homoscedasticity",
                     "test_name" = test_name,
                     "p.value" = NA_real_,
                     "conclusion" = NA_character_,
                     "rejection_rate" = NA_real_)

      return(output)
    }
    output <- list("testing" = "homoscedasticity",
                   "test_name" = test_name,
                   "p.value" = NA_real_,
                   "conclusion" = NA_character_)

    return(output)
  }

  if (!is.null(B)) {
    replicated_pvals <- replicate(
      n = ceiling(B * (1 + buffer)),
      expr = {
        bmor_data <- resample_fun_of_samples(mor_data)
        bpval <- tryCatch(
          expr = {
            bp_test(data = bmor_data,
                    koenker = koenker) * n_sim_tests
          },
          error = function(e) NA_real_,
          warning = function(e) NA_real_
        )
      },
      simplify = TRUE
    )

    replicated_pvals <- replicated_pvals[!is.na(replicated_pvals)]
    if (length(replicated_pvals) > B) {
      replicated_pvals <- replicated_pvals[1:B]
    }
    rejection_rate <- mean(replicated_pvals < 1 - level)

  }

  conclusion <- if (orig_pval < 1 - level) {"reject"} else {"not reject"}

  if (!is.null(rejection_rate)) {
    output <- list("testing" = "homoscedasticity",
                   "test_name" = test_name,
                   "p.value" = orig_pval,
                   "conclusion" = conclusion,
                   "rejection_rate" = rejection_rate)

    return(output)
  }

  output <- list("testing" = "homoscedasticity",
                 "test_name" = test_name,
                 "p.value" = orig_pval,
                 "conclusion" = conclusion)

  return(output)

}


#' Converts Standard to Holm-adjusted p-values and Test Conclusions
#'
#' @description
#' This function modifies Bonferroni-adjusted and non-adjusted p-values
#' to Bonferroni-Holm-adjusted p-values. The test conclusion is altered based
#' on this modification too. However, \code{rejection_rate} is not touched.
#'
#' @param data See \code{?perform_assessment_tests()}.
#' @param level See \code{?perform_assessment_tests()}.
#' @param n_sim_tests An \code{integer}. The number of simultanuous tests to
#'                    perform for normality and homoscedasticity separately.
#'
#' @return
#' A \code{data.table}, where the p-values and conclusions are modified based
#' on the Holm-adjustment.
#' @keywords internal
bonferroni_to_holm <- function(data, level, n_sim_tests = 10) {

  # Bind global variables
  conclusion <- p.value <- NULL

  # Shrten:
  m <- n_sim_tests

  # Avoid modifying the original data
  data <- copy(data)

  # Update p.values and conclusions
  data[, p.value := p.adjust(p.value / m, method = "holm")]
  data[, conclusion := ifelse(p.value < 1 - level, "reject", "not reject")]

  return(data)

}

#' @title
#' Processed Raw Output to a desired Output Structure
#'
#' @description
#' This function cleans \code{raw_output} and possibly reformats its variables
#' so that they are pleasing on the eyes.
#'
#' @param raw_output A \code{data.table}. The temporary output of
#'                   \code{perform_assessment_tests}
#' @param output A \code{character} string. The desired output format.
#'               See \code{?perform_assessment_tests}.
#' @param weighted A \code{logical} value. If \code{TRUE}, heteroscedasticity
#'                 test results are irrelevant, because heteroscedasticity is
#'                 addressed by the model.
#' @param rounding An \code{integer}. The number of digits used in the rounding
#'                 of the results.
#' @param repeat_string A \code{character} string. Repeated results in the
#'                      output are replaced with this string. Defaults to
#'                      \code{'.'}.
#'
#' @return
#' A \code{data.table}, that is processed \code{perform_assessment_tests} raw
#' output according to the desired \code{output} format.
#' @keywords internal
process_output_pat <- function(raw_output,
                               output = "visual",
                               weighted = FALSE,
                               rounding = 3L,
                               repeat_string = ".") {

  # Bind global variables
  testing <- test_name <- p.value <- conclusion <- rejection_rate <- NULL

  # Required local functions
  format_decimals <- function(x, digits = 3L, threshold = 0.001) {
    below_threshold <- (!is.na(x)) & x < threshold
    formatted <- sprintf(paste0("%.", digits, "f"), x)
    threshold_text <- sprintf(paste0("< %.", digits, "f"), threshold)
    formatted[below_threshold] <- threshold_text
    formatted[stri_detect(formatted, fixed = "NA")] <- NA_character_
    return(formatted)
  }

  replace_repeats <- function(x, repeat_string = ".") {
    result <- x
    i <- 1
    while (i < length(x)) {
      j <- i + 1
      while (j <= length(x) && x[j] == x[i]) {
        if (j < length(x) && x[j+1] == x[i]) {
          result[j] <- repeat_string
        }
        j <- j + 1
      }
      i <- j
    }
    return(result)
  }

  get_homoscedasticity_ids <- function(x) {
    return(which(x == "homoscedasticity"))
  }

  # Define processed_output
  processed_output <- copy(raw_output)

  # Limit p.value to [0, 1]
  processed_output$p.value <- pmin(
    processed_output$p.value,
    1,
    na.rm = FALSE
  )

  # If not visual, just return raw_output, but with rounded results ...
  if (output != "visual") {
    processed_output$p.value <- round(
      x = processed_output$p.value,
      digits = rounding
    )
    if (!is.null(processed_output$rejection_rate)) {
      processed_output$rejection_rate <- round(
        x = processed_output$rejection_rate,
        digits = rounding
      )
    }
    return(processed_output)
  }

  # Formatting
  processed_output[, testing := replace_repeats(
    x = testing,
    repeat_string = repeat_string
  )]

  processed_output[, test_name := replace_repeats(
    x = test_name,
    repeat_string = repeat_string
  )]

  processed_output[, p.value := format_decimals(
    x = p.value,
    digits = max(
      1L,
      min(
        rounding,
        4L
      )
    ),
    threshold = 1 / 10 ** max(
      1L,
      min(
        rounding,
        4L
      )
    )
  )]

  # Format rejection_rate if it exists
  if (!is.null(processed_output$rejection_rate)) {
    processed_output[, rejection_rate := format_decimals(
      x = rejection_rate,
      digits = max(
        1L,
        min(
          rounding,
          4L
        )
      ),
      threshold = 1 / 10 ** max(
        1L,
        min(rounding, 4L)
      )
    )]
  }

  # Modify irrelevant test results if weighted model is used
  if (weighted) {

    # Get homoscedasticity test row IDs
    homo_ids <- get_homoscedasticity_ids(raw_output$testing)

    # Replace p.value values with "----"
    processed_output$p.value[homo_ids] <- paste0(
      rep(
        x = "-",
        max(
          1L,
          min(
            rounding,
            4L
          )
        ) + 1
      ),
      collapse = ""
    )

    # Replace conclusion values with "irrelevant"
    processed_output$conclusion[homo_ids] <- "irrelevant"

    # Replace rejection_rate values with "----"
    if (!is.null(processed_output$rejection_rate)) {
      processed_output$rejection_rate[homo_ids] <- paste0(
        rep(
          "-",
          times = max(
            1L,
            min(
              rounding,
              4L
            )
          ) + 1
        ),
        collapse = ""
      )
    }
  }

  # Replace repeating results with ...
  processed_output[, conclusion := replace_repeats(
    x = conclusion,
    repeat_string = repeat_string
  )]

  # Use 'nice' names
  raw_names <- names(processed_output)
  nice_names <- c("IVD-MD Comparison",
                  "Testing",
                  "Test Name",
                  "p-value",
                  "Conclusion",
                  "Rejection Rate")
  if (!any("rejection_rate" == raw_names)) {
    nice_names <- nice_names[-match("Rejection Rate", nice_names)]
  }

  setnames(x = processed_output,
           old = raw_names,
           new = nice_names,
           skip_absent = TRUE)

  # Avoid invisible output
  processed_output <- copy(processed_output)

  return(processed_output)


}

#' @title
#' Perform Linear Model Assessment Tests
#'
#' @param data A \code{data.table}, \code{list}, or \code{data.frame}.
#'             Must contain the clinical sample data with variables:
#'             \itemize{
#'                \item \code{comparison: } A \code{character} vector. The
#'                      comparison identifiers. Typically on the form
#'                      \code{'MP_A - MP_B'}.
#'                \item \code{SampleID: } A \code{character} vector. The sample
#'                      identifiers for the clinical samples.
#'                \item \code{ReplicateID: } A \code{character} vector. The
#'                      replicated measurement identifiers.
#'                \item \code{MP_A: } A \code{numeric} vector. The observed
#'                      measurement results from IVD-MD \code{MP_A} (response).
#'                \item \code{MP_B: } A \code{numeric} vector. The observed
#'                      measurement results from IVD-MD \code{MP_A} (predictor).
#'             }
#' @param B An \code{integer}. Must be larger than \code{10}. The number of
#'          bootstrap replicates used to estimate the rejection rate for each
#'          test. See details.
#' @param method A \code{character} string. The desired model for calculating
#'               residuals that is used in the Shapiro-Wilk normality test. The
#'               possible options are \code{'fg'} (default), \code{'clsi'},
#'               \code{'ols'}, \code{'ss'} and \code{'ssw'}. The Breusch-Pagan
#'               tests will always use \code{'ols'}, so take care interpreting
#'               the Breusch-Pagan test results under non-linearity.
#' @param robust A \code{logical} value. If \code{TRUE} (default), the Koenker
#'               version of the Breusch-Pagan test is used for testing for
#'               variance inhomogeneity.
#' @param level A \code{double}. Must be between \code{0} and \code{1}. The
#'              corresponding non-adjusted test significance levels are given
#'              by \eqn{\alpha = 1 - \mathrm{level}}.
#' @param adjust_p_value A \code{logical} value. If \code{TRUE} (default),
#'                       non-adjusted p-values are Bonferroni-adjusted to
#'                       keep the familiy-wise significance level equal to
#'                       \eqn{1 - \mathrm{level}} for each test.
#' @param holm A \code{logical} value. If \code{TRUE}, non-adjusted p-values
#'             are Holm-Bonferroni corrected. Otherwise (default), the
#'             non-adjusted p-values are Bonferroni corrected. Only relevant
#'             if \code{adjust_p_value} is \code{TRUE}. Note:
#'             \code{rejection_rate} will not account for Holm-Bonferroni
#'             correction.
#'
#' @param output A \code{character} string. If \code{'visual'} (default), the
#'               output is formatted in a clean way. However, it is not
#'               optimal to use for further calculations.
#'
#'
#' @description
#' Perform normality and homoscedasticity tests for each IVD-MD comparison
#' in \code{data}.
#'
#'
#'
#' @return
#' A \code{data.table} with columns:
#' \itemize{
#'  \item \code{comparison: } See \code{data}.
#'  \item \code{test_name: } A \code{character} vector. The test names.
#'  \item \code{p.value: } A \code{numeric} vector. The raw or adjusted
#'        p-values.
#'  \item \code{conclusion: } A \code{character} vector. The conclusion based
#'        on each test.
#'  \item \code{rejection_rate: } A \code{numeric} vector. The proportion of
#'        resampled test statistics that resulted in rejected of the null.
#'        Will only appear if \code{B} is an integer larger than \code{10}.
#' }
#' @export
#'
#' @examples print(1)

perform_assessment_tests <- function(data,
                                     B = 2e2L,
                                     method = "fg",
                                     robust = TRUE,
                                     level = 0.95,
                                     adjust_p_value = TRUE,
                                     holm = TRUE,
                                     output = "visual"){

  # Handling global variables
  comparison <- SampleID <- ReplicateID <- conclusion <- p.value <- NULL

  # Validate and convert data
  data <- validate_data_pat(data)

  # Validate and convert level
  level <- validate_level_pat(level)

  # Validate and convert B
  B <- validate_B_pat(B)

  # Get number of unique comparisons
  n_comparisons <- length(unique(data$comparison))

  # Normality test results
  normality_tests <- data[, get_normality_test_results(data = .SD,
                                                       method = method,
                                                       level = level,
                                                       B = B,
                                                       n_sim_tests = ifelse(adjust_p_value,
                                                                            n_comparisons,
                                                                            1L),
                                                       buffer = 0.05),
                          by = comparison]

  # Homoscedasticity test results
  homoscedasticity_tests <- data[, get_heteroscedasticity_test_results(data = .SD,
                                                                       level = level,
                                                                       B = B,
                                                                       koenker = robust,
                                                                       n_sim_tests = ifelse(adjust_p_value,
                                                                                            n_comparisons,
                                                                                            1L),
                                                                       buffer = 0.05),
                                 by = comparison]

  # Combine test results
  can_combine <- tryCatch(
    expr = {
      all(names(normality_tests) == names(homoscedasticity_tests))
    },
    error = function(e) FALSE,
    warning = function(w) FALSE
  )

  if (!can_combine) {
    warning(
      "Could not combine normality_tests and homoscedasticity_tests data.table ",
      "objects using rbind() because their columns do not match. A list containing both ",
      "data.table objects are returned instead.",
      immediate. = TRUE
    )
    return(list("normality_tests" = normality_tests,
                "homoscedasticity_tests" = homoscedasticity_tests))
  }

  # Adjust p.values using Holm
  if (adjust_p_value && holm && can_combine) {
    normality_tests <- bonferroni_to_holm(data = normality_tests,
                                          n_sim_tests = n_comparisons,
                                          level = level)
    homoscedasticity_tests <- bonferroni_to_holm(data = homoscedasticity_tests,
                                                 n_sim_tests = n_comparisons,
                                                 level = level)
  }

  raw_output <- rbind(normality_tests, homoscedasticity_tests)

  return(process_output_pat(raw_output = raw_output,
                            output = output,
                            weighted = method == "ssw",
                            rounding = 3L,
                            repeat_string = "."))
}



