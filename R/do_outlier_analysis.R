#' Calculate the Desired \code{variable}
#'
#' @description
#' This function is used to calculate the desired \code{variable} to be used in
#' \code{do_outlier_analysis()}. This function should not be used by end-users.
#'
#' @param data A \code{data.table}.
#' @param variable A \code{character} string. See \code{?do_outlier_analysis()}.
#'
#' @return
#' The same as \code{data}, but where \code{variable} is calculated based on
#' \code{MP_A} and \code{MP_B} for each \code{SampleID} in \code{data}.
#' @keywords internal
estimate_variable <- function(data, variable) {

  # Bind global variables
  SampleID <- MP_A <- MP_B <- NULL

  # Avoid modifying original data
  output <- copy(data)

  # Calculate mean of replicates
  output <- fun_of_replicates(output)

  # Convert to data.table
  setDT(output)

  # Keep unique Rows
  output <- unique(output)

  # Depending on variable, calculate it ...
  if (variable == "influence") {

    compute_residuals <- function(x, y) {

      # Objective functions
      objective_function_deg1 <- function(params, pred, resp) {
        c0 <- params[1]
        c1 <- params[2]
        model_error <- resp - (c0 + c1 * pred)
        mse <- mean(model_error ** 2, na.rm = TRUE)
        return(mse)
      }

      objective_function_deg2 <- function(params, pred, resp) {
        c0 <- params[1]
        c1 <- params[2]
        c2 <- params[3]
        model_error <- resp - (c0 + c1 * pred + c2 * pred ** 2)
        mse <- mean(model_error ** 2, na.rm = TRUE)
        return(mse)
      }

      # Model Extraction functions
      get_raw_residuals <- function(params, pred, resp) {
        if (length(params) == 3) {
          c0 <- params[1]
          c1 <- params[2]
          c2 <- params[3]
          out <- resp - (c0 + c1 * pred + c2 * pred ** 2)
          return(list(raw_residuals = out,
                      p = 3))
        }
        else if (length(params) == 2) {
          c0 <- params[1]
          c1 <- params[2]
          out <- resp - (c0 + c1 * pred)
          return(list(raw_residuals = out,
                      p = 2))
        }
        else {
          stop(
            "Could not get raw residuals."
          )
        }
      }

      get_residual_variance <- function(raw_residuals) {
        p <- raw_residuals$p
        raw_residuals <- raw_residuals$raw_residuals
        n <- length(raw_residuals[!is.na(raw_residuals)])
        residual_variance <- sum(raw_residuals ** 2, na.rm = TRUE) / (n - p)
        return(residual_variance)
      }

      get_leverage <- function(pred) {
        n <- length(pred[!is.na(pred)])
        J <- cbind(1, pred, pred**2)
        H <- J %*% solve(t(J) %*% J) %*% t(J)
        leverage_values <- diag(H)
        influence_values <- (n - 1) * (leverage_values - 1 / n)
        return(leverage_values)
      }

      # Minimization
      estimated_pars <- tryCatch(
        expr = {
          nlm(
            f = objective_function_deg2,
            p = c(0, 1, 0),
            pred = x,
            resp = y
          )$estimate
        },
        error = function(e) NULL,
        warning = function(w) NULL
      )

      # Check if minimization was successful
      if (is.null(estimated_pars)) {

        # Try Linear Model Instead if minimization was not successful
        estimated_pars <- tryCatch(
          expr = {
            nlm(
              f = objective_function_deg1,
              p = c(0, 1),
              pred = x,
              resp = y
            )$estimate
          },
          error = function(e) NULL,
          warning = function(w) NULL
        )

        # Check if second minimization attempt was successful
        if (is.null(estimated_pars)) {
          stop(
            "Could not fit a suitable model to the data."
          )
        }
      }

      # Try to extract raw residuals from model fit
      candidate_raw_residuals <- tryCatch(
        expr = {
          get_raw_residuals(
            params = estimated_pars,
            pred = x,
            resp = y
          )
        },
        error = function(e) NULL,
        warning = function(w) NULL
      )

      # Check if extraction of raw residuals was successful
      if (is.null(candidate_raw_residuals)) {
        stop(
          "Could not extract the raw residuals from the fitted model."
        )
      }

      # Try to extract estimated residual variance from model fit
      candidate_residual_variance <- tryCatch(
        expr = {
          get_residual_variance(
            raw_residuals = candidate_raw_residuals
          )
        },
        error = function(e) NULL,
        warning = function(w) NULL
      )

      # Check if extraction of estimated residual variance was successful
      if (is.null(candidate_residual_variance)) {
        stop(
          "Could not calculate the estimated residual variance of the fitted model."
        )
      }

      # Try to extract leverage values from model fit
      candidate_leverage_values <- tryCatch(
        expr = {
          get_leverage(
            pred = x
          )
        },
        error = function(e) NULL,
        warning = function(w) NULL
      )

      # Check if extraction of leverage values from model fit was successful
      if (is.null(candidate_leverage_values)) {
        stop(
          "Could not calculate the estimated residual variance of the fitted model."
        )
      }

      valid_raw_residuals <- candidate_raw_residuals$raw_residuals
      valid_estimated_residual_variance <- candidate_residual_variance
      valid_leverage_values <- candidate_leverage_values

      standardized_residuals <- valid_raw_residuals / (sqrt(valid_estimated_residual_variance) * sqrt(1 - valid_leverage_values))

      if (sum(is.na(standardized_residuals)) >= 20) {
        stop("There are too many NA-values!")
      }

      return(standardized_residuals)
    }

    output <- output[, list(
      SampleID = SampleID,
      MP_A = MP_A,
      MP_B = MP_B,
      variable = tryCatch(
        expr = {
          compute_residuals(MP_B, MP_A)
        },
        error = function(e) MP_A - MP_B,
        warning = function(w) MP_A - MP_B
      ))]
  }

  else if (variable == "raw_difference") {
    output <- output[, list(
      MP_A = MP_A,
      MP_B = MP_B,
      variable = MP_A - MP_B
    ),
    by = SampleID]
  }

  else if (variable == "absolute_difference") {
    output <- output[, list(
      MP_A = MP_A,
      MP_B = MP_B,
      variable = abs(MP_A - MP_B)
    ),
    by = SampleID]
  }

  else if (variable == "relative_difference") {
    output <- output[, list(
      MP_A = MP_A,
      MP_B = MP_B,
      variable = abs(MP_A - MP_B) / MP_B
    ),
    by = SampleID]
  }

  else if (variable == "log_difference") {
    output <- output[, list(
      MP_A = MP_A,
      MP_B = MP_B,
      variable = log(MP_A) - log(MP_B)
    ),
    by = SampleID]
  }

  return(output)

}

#' Calculations and Summaries of Outlier Tests
#'
#' @description
#' This function is used within \code{do_outlier_analysis()} to calculate
#' test statistics, rejection regions, and test summaries. This function should
#' not be used by end-users.
#'
#' @param data A \code{data.table}. See \code{?do_outlier_analysis()}.
#' @param method A \code{character} string. See \code{?do_outlier_analysis()}.
#' @param variable A \code{character} string. See \code{?do_outlier_analysis()}.
#' @param level A \code{double} value. See \code{?do_outlier_analysis()}.
#'
#' @return
#' The same as \code{data}, but where \code{variable} is calculated based on
#' \code{MP_A} and \code{MP_B} for each \code{SampleID} in \code{data}.
#' @keywords internal
outlier_tests <- function(data, method, variable, level = 0.99) {

  # Bind global variables
  comparison <- SampleID <- MP_A <- MP_B <- outlier <- conclusion <- NULL

  # Initialize
  outlier_test_results <- NULL
  n_comparisons <- length(unique(data$comparison))
  n_simulations <- if (n_comparisons > choose(10, 2)) {
    1000
  }
  else if (n_comparisons >= choose(9, 2)) {
    2000
  }
  else if (n_comparisons >= choose(8, 2)) {
    2800
  }
  else if (n_comparisons >= choose(7, 2)) {
    3700
  }
  else if (n_comparisons >= choose(6, 2)) {
    5200
  }
  else if (n_comparisons >= choose(5, 2)) {
    7800
  }
  else {
    10000
  }


  # Tietjen Moore Test AD HOC
  tietjen_moore_ad_hoc <- function(test_data, variable, level = 0.99) {

    calc_tm_obs <- function(x_all, x_wo_outliers, m_x_all, m_x_wo_outliers) {
      num <- sum((x_wo_outliers - m_x_wo_outliers) ** 2, na.rm = TRUE)
      den <- sum((x_all - m_x_all) ** 2, na.rm = TRUE)
      return(num / den)
    }

    calc_tm_crit <- function(n_sims = 2e3,
                             n_samples,
                             n_outliers,
                             m_x_all,
                             sd_x_all,
                             variable,
                             level) {
      if (variable %in% c("absolute_difference", "relative_difference")) {
        tm_obs_simulations <- replicate(
          n = n_sims,
          expr = {
            simulated_x_all <- abs(rnorm(n = n_samples,
                                         mean = m_x_all,
                                         sd = sd_x_all))
            simulated_x_wo_outliers <- simulated_x_all[-order(abs(simulated_x_all), decreasing = TRUE)[1:n_outliers]]
            simulated_m_x_all <- mean(simulated_x_all, na.rm = TRUE)
            simulated_m_x_wo_outliers <- mean(simulated_x_wo_outliers, na.rm = TRUE)
            tm_obs_candidate <- tryCatch(
              expr = {
                calc_tm_obs(simulated_x_all,
                            simulated_x_wo_outliers,
                            simulated_m_x_all,
                            simulated_m_x_wo_outliers)
              },
              error = function(e) NA_real_,
              warning = function(w) NA_real_
            )
            return(tm_obs_candidate)
          }
        )
        tm_crit <- quantile(x = tm_obs_simulations,
                            probs = 1 - level,
                            names = FALSE,
                            na.rm = TRUE)
        return(tm_crit)
      }
      else if (variable %in% c("raw_difference", "log_difference", "influence")) {
        tm_obs_simulations <- replicate(
          n = n_sims,
          expr = {
            simulated_x_all <- rnorm(n = n_samples,
                                     mean = m_x_all,
                                     sd = sd_x_all)
            simulated_x_wo_outliers <- simulated_x_all[-order(abs(simulated_x_all), decreasing = TRUE)[1:n_outliers]]
            tm_obs_candidate <- tryCatch(
              expr = {
                calc_tm_obs(simulated_x_all, simulated_x_wo_outliers)
              },
              error = function(e) NA_real_,
              warning = function(w) NA_real_
            )
            return(tm_obs_candidate)
          }
        )
        tm_crit <- quantile(x = tm_obs_simulations,
                            probs = 1 - level,
                            names = FALSE,
                            na.rm = TRUE)
        return(tm_crit)
      }

      return(NA_real_)

    }

    # Avoid modifying original data
    test_data <- copy(test_data)

    # Number of unique samples
    n_samples <- length(unique(test_data$SampleID))

    # Number of outliers
    n_outliers <- sum(test_data$outlier, na.rm = TRUE)

    # Check if no outliers
    if (n_outliers == 0) {
      test_data <- test_data[, list(statistic = NA_real_,
                                    critical = NA_real_,
                                    conclusion = NA),
                             by = c("SampleID", "variable", "outlier")]
      return(test_data)
    }

    # Data
    x_all <- test_data$variable
    x_wo_outliers <- test_data[outlier == FALSE]$variable

    # Parameters
    m_x_all <- mean(x_all, na.rm = TRUE)
    m_x_wo_outliers <- mean(x_wo_outliers, na.rm = TRUE)
    sd_x_all <- sd(x_all, na.rm = TRUE)

    # Calculate test statistic
    tm_obs <- calc_tm_obs(x_all = x_all,
                          x_wo_outliers = x_wo_outliers,
                          m_x_all = m_x_all,
                          m_x_wo_outliers = m_x_wo_outliers)

    # Simulate rejection region
    tm_crit <- calc_tm_crit(n_sims = n_simulations,
                            n_samples = n_samples,
                            n_outliers = n_outliers,
                            m_x_all = m_x_all,
                            sd_x_all = sd_x_all,
                            variable = variable,
                            level = level)

    # Summarize data
    test_data <- test_data[, list(statistic = tm_obs,
                                  critical = tm_crit,
                                  conclusion = tm_obs < tm_crit),
                           by = c("SampleID", "variable", "outlier")]

    return(test_data)



  }

  # Studentized Q-range Test (only for where = 'replicates')
  do_qrange_test <- function(data, level = 0.99) {

    qrange_test <- function(x, level, n, R, std) {

      # Checks if length is invalid
      if (length(x[!is.na(x)]) < 2) {
        #warning(
        #  "x is expected to be of length > 2L, but x is of length ",
        #  length(x[!is.na(x)]),
        #  "L. Studentized Q-range test results in FALSE by default."
        #)
        return(FALSE)
      }

      q_obs <- diff(range(x, na.rm = TRUE))
      q_tukey_quantile <- qtukey(p = level,
                                nmeans = length(x[!is.na(x)]),
                                df = n * (R - 1))
      q_critical <- q_tukey_quantile * std

      return(q_obs > q_critical)
    }

    # Avoid modifying original data
    data <- copy(data)

    # Calculate imprecision estimates
    impr_data <- global_precision_estimates(data)

    # Calculate n and R
    comp_n <- length(unique(data$SampleID))
    comp_R <- count_samplewise_replicates(data = data, summary = "mean")$R_i

    # Extract Standard Deviations
    std_A <- tryCatch(
      expr = sqrt(impr_data$Var_A),
      error = function(e) NA_real_
    )

    std_B <- tryCatch(
      expr = sqrt(impr_data$Var_B),
      error = function(e) NA_real_
    )

    if (is.na(std_A) || is.na(std_B)) {
      stop(
        "Failed to calculate SD_A or SD_B.",
        "Calculations are terminated."
      )
    }


    # Do Q-range outlier test
    test_data <- tryCatch(
      expr = {
        data[, list(
          outlier_A = qrange_test(x = MP_A,
                                  level = level,
                                  n = comp_n,
                                  R = comp_R,
                                  std = std_A),
          outlier_B = qrange_test(x = MP_B,
                                  level = level,
                                  n = comp_n,
                                  R = comp_R,
                                  std = std_B)
        ),
        by = SampleID]
      },
      error = function(e) NULL
    )

    if (is.null(test_data)) {
      stop(
        "Could not peform the studentized Q-range test on at least one of the ",
        "IVD-MD comparisons. Calculations are terminated."
      )
    }

    return(test_data)

  }

  # Burnett's Test (only for where = 'samples')
  do_burnett_test <- function(data, variable) {

    burnett_criterion <- function(x) {

      # Checks if length is invalid
      if (length(x[!is.na(x)]) < 2) {
        warning(
          "x is expected to be of length > 2L, but x is of length ",
          length(x[!is.na(x)]),
          "L. Burnett test results in FALSE by default."
        )
        return(NA)
      }

      # Burnett parameters
      intercept <- 2.1685
      slope <- 0.2826

      # Relevant parameters
      n <- length(x[!is.na(x)])

      # Perform test
      b_crit <- intercept + slope * log(n)

      return(list("mean" = mean(x, na.rm = TRUE),
                  "sd" = sd(x, na.rm = TRUE),
                  "crit" = b_crit))


    }

    burnett_obs <- function(x, mean = 0, sd = 1) {
      return(abs(x - mean) / sd)
    }

    # Avoid modifying original data
    test_data <- estimate_variable(data = copy(data),
                                   variable = variable)

    comp_criterion <- burnett_criterion(test_data$variable)
    comp_mean <- comp_criterion$mean
    comp_sd <- comp_criterion$sd
    comp_criterion <- comp_criterion$crit

    # Check if outlier
    test_data <- test_data[, list(
      variable = variable,
      outlier = burnett_obs(x = variable,
                            mean = comp_mean,
                            sd = comp_sd) > comp_criterion
    ),
    by = SampleID]

    # NA-values are converted to FALSE
    test_data[, outlier := ifelse(is.na(outlier), FALSE, outlier)]

    return(test_data)

  }

  # Do Chauvenet Test (only for where = 'samples')
  do_chauvenet_test <- function(data, variable, level = 0.99) {

    chauvenet_criterion <- function(x) {

      # Checks if length is invalid
      if (length(x[!is.na(x)]) < 2) {
        warning(
          "x is expected to be of length > 2L, but x is of length ",
          length(x[!is.na(x)]),
          "L. Burnett test results in FALSE by default."
        )
        return(NA)
      }

      # Relevant parameters
      n <- length(x[!is.na(x)])

      # Perform test
      c_crit <- qnorm(1 - 1 / 4 / n)

      return(list("mean" = mean(x, na.rm = TRUE),
                  "sd" = sd(x, na.rm = TRUE),
                  "crit" = c_crit))


    }

    chauvenet_obs <- function(x, mean = 0, sd = 1) {
      return(abs(x - mean) / sd)
    }

    # Avoid modifying original data
    test_data <- estimate_variable(data = copy(data),
                                   variable = variable)

    comp_criterion <- chauvenet_criterion(test_data$variable)
    comp_mean <- comp_criterion$mean
    comp_sd <- comp_criterion$sd
    comp_criterion <- comp_criterion$crit

    # Check if outlier
    test_data <- test_data[, list(
      variable = variable,
      outlier = chauvenet_obs(x = variable,
                              mean = comp_mean,
                              sd = comp_sd) > comp_criterion
    ),
    by = SampleID]

    test_data <- tietjen_moore_ad_hoc(test_data = test_data,
                                      variable = variable,
                                      level = level)


    test_data <- test_data[, list(
      variable = variable,
      outlier = ifelse(is.na(outlier & conclusion), FALSE, outlier & conclusion)
    ),
    by = SampleID]

    return(test_data)

  }

  # Do IQR Test (only for where = 'samples')
  do_iqr_test <- function(data, variable, level = 0.99) {

    iqr_criterion <- function(x) {

      # Checks if length is invalid
      if (length(x[!is.na(x)]) < 2) {
        warning(
          "x is expected to be of length > 2L, but x is of length ",
          length(x[!is.na(x)]),
          "L. Burnett test results in FALSE by default."
        )
        return(NA)
      }

      # Relevant parameters
      n <- length(x[!is.na(x)])

      # Perform test
      c_crit_lwr <- quantile(x = x,
                             probs = 0.25,
                             names = FALSE,
                             na.rm = TRUE) - 1.5 * IQR(x = x,
                                                       na.rm = TRUE)
      c_crit_upr <- quantile(x = x,
                             probs = 0.75,
                             names = FALSE,
                             na.rm = TRUE) + 1.5 * IQR(x = x,
                                                       na.rm = TRUE)

      return(list("crit" = c(c_crit_lwr, c_crit_upr)))


    }

    # Avoid modifying original data
    test_data <- estimate_variable(data = copy(data),
                                   variable = variable)

    comp_criterion <- iqr_criterion(test_data$variable)$crit

    # Check if outlier
    test_data <- test_data[, list(
      variable = variable,
      outlier = variable < comp_criterion[1] | variable > comp_criterion[2]
    ),
    by = SampleID]

    # Check if significant
    test_data <- tietjen_moore_ad_hoc(test_data = test_data,
                                      variable = variable,
                                      level = level)


    test_data <- test_data[, list(
      variable = variable,
      outlier = ifelse(is.na(outlier & conclusion), FALSE, outlier & conclusion)
    ),
    by = SampleID]



    return(test_data)

  }

  if (method == "qrange") {
    outlier_test_results <- data[, do_qrange_test(data = .SD,
                                                  level = level),
                                 by = comparison]
  }

  else if (method == "burnett") {
    outlier_test_results <- data[, do_burnett_test(data = .SD,
                                                   variable = variable),
                                 by = comparison]
  }

  else if (method == "chauvenet") {
    outlier_test_results <- data[, do_chauvenet_test(data = .SD,
                                                     variable = variable,
                                                     level = level),
                                 by = comparison]
  }

  else if (method == "iqr") {
    outlier_test_results <- data[, do_iqr_test(data = .SD,
                                               variable = variable,
                                               level = level),
                                 by = comparison]
  }

  return(outlier_test_results)
}

#' Transform Raw Outlier Test Summary to Desired Format
#'
#' @description
#' This function is used within \code{do_outlier_analysis()} to process raw
#' output to the desired format. This function should not be used by end-users.
#'
#' @param raw_output A \code{data.table}. Output from \code{outlier_tests()}.
#' @param method A \code{character} string. See \code{?do_outlier_analysis()}.
#' @param output A \code{character} string. See \code{?do_outlier_analysis()}.
#'
#' @return
#' A \code{data.table}. The processed output with the desired format.
#' @keywords internal
process_output_doa <- function(raw_output, method, output = "visual") {

  # Bind global variables
  comparison <- SampleID <- MP_A <- MP_B <- outlier <- outliers <- conclusion <- NULL

  # Required local functions
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

  visual_SampleID_level_outlier_summary <- function(processed_output, method) {


    # Avoid modifying original raw_output
    processed_output <- copy(processed_output)

    # Get pretty name
    processed_method <- switch(method,
                               "burnett" = "Burnett's Test",
                               "chauvenet" = "Chauvenet's Criterion",
                               "iqr" = "IQR Test")

    # Get presentable table
    processed_output <- processed_output[, list(
      method = processed_method,
      outliers = ifelse(test = sum(outlier, na.rm = TRUE) >= 1,
                        yes = paste0(SampleID[outlier[!is.na(outlier)]], collapse = ", "),
                        no = "none")
    ),
    by = comparison]

    # Reformat
    processed_output[, outliers := replace_repeats(x = outliers,
                                                   repeat_string = ".")]
    processed_output[, method := replace_repeats(x = method,
                                                 repeat_string = ".")]


    return(processed_output)

  }

  visual_ReplicateID_level_outlier_summary <- function(processed_output, method) {

    # Avoid modifying the original data
    processed_output <- copy(processed_output)

    # Split comparison vector
    processed_output[, `:=`(MP_A = stri_split(comparison, fixed = " - ")[[1]][1],
                            MP_B = stri_split(comparison, fixed = " - ")[[1]][2]),
                     by = .I]

    # Get separate parts
    processed_output_A <- processed_output[, -c("comparison", "outlier_B", "MP_B")]
    processed_output_B <- processed_output[, -c("comparison", "outlier_A", "MP_A")]

    # Convert each part to wide-format
    part_1 <- dcast.data.table(data = processed_output_A,
                               formula = SampleID ~ MP_A,
                               value.var = "outlier_A",
                               fun.aggregate = function(x) x[1])
    part_2 <- dcast.data.table(data = processed_output_B,
                               formula = SampleID ~ MP_B,
                               value.var = "outlier_B",
                               fun.aggregate = function(x) x[1])

    # Get potential extra information hidden in part_2
    extra_columns_in_part_2 <- setdiff(names(part_2), names(part_1))

    # If there is extra information in part_2 not part of part_1, include it.
    if (length(extra_columns_in_part_2) > 0) {
      additional_information_from_part_2 <- part_2[, c("SampleID",
                                                       extra_columns_in_part_2),
                                                   with = FALSE]
      processed_output <- merge(x = part_1,
                                y = additional_information_from_part_2,
                                by = c("SampleID"),
                                sort = FALSE)
    }
    else {
      processed_output <- part_1
    }

    numeric_cols <- setdiff(names(processed_output), "SampleID")
    processed_output <- processed_output[, lapply(X = .SD,
                                                  FUN = function(cell) {
                                                    ifelse(cell, "yes", "no")
                                                  }),
                                         .SDcols = numeric_cols,
                                         by = "SampleID"]

    processed_output <- processed_output[, lapply(
      X = .SD,
      FUN = replace_repeats,
      repeat_string = "."
    )]


    return(processed_output)


  }

  # Avoid modifying original raw_output
  processed_output <- copy(raw_output)

  if (output == "visual") {
    if (any(method == c("iqr", "chauvenet", "burnett"))) {
      output_summary <- visual_SampleID_level_outlier_summary(processed_output, method)
      names(output_summary) <- c("IVD-MD Comparison",
                                 "Outlier Detection Method",
                                 "Outliers")
      return(output_summary)
    }
    else if (method == "qrange") {
      return(visual_ReplicateID_level_outlier_summary(processed_output, method))
    }
  }
  else {
    return(processed_output)
  }

}

#' @title
#' Outlier Analysis Based on Clinical Sample Data
#'
#' @param data A \code{data.table}, \code{list} or \code{data frame}. The
#'             clinical sample measurement data. Must contain the following
#'             variables:
#'             \itemize{
#'                \item \code{comparison: } A \code{character} vector. The
#'                      comparison identifiers. Typically on the form
#'                      \code{'MP_A - MP_B'}.
#'                \item \code{SampleID: }  A \code{character} vector. The sample
#'                      identifiers for the clinical samples.
#'                \item \code{ReplicateID: } A \code{character} vector. The
#'                      replicated measurement identifiers.
#'                \item \code{MP_A: } A \code{numeric} vector. The observed
#'                      measurement results from IVD-MD \code{MP_A} (response).
#'                \item \code{MP_B: } A \code{numeric} vector. The observed
#'                      measurement results from IVD-MD \code{MP_A} (predictor).
#'             }
#' @param method A \code{character} string. The desired outlier detection method
#'               to apply to \code{data}. Options include:
#'               \itemize{
#'                  \item \code{qrange: } Studentized Q-range test. Will be
#'                        performed on \code{ReplicateID}-level.
#'                  \item \code{burnett: } Burnetts method. Default. Will be
#'                        performed on \code{SampleID}-level.
#'                  \item \code{chauvenet: } Chauvenet's Criterion. Will be
#'                        performed on \code{SampleID}-level. Only statistically
#'                        significant outliers according to the Tietjen-Moore
#'                        test will be considered outliers among the
#'                        \code{chauvenet} candidates.
#'                  \item \code{iqr: } Inter-Quartile Range test. Will be
#'                        performed on \code{SampleID}-level. Only statistically
#'                        significant outliers according to the Tietjen-Moore
#'                        test will be considered outliers among the
#'                        \code{iqr} candidates.
#'              }
#' @param variable A \code{character} string. The variable to be assessed when
#'                 looking for outliers. Possible choices are:
#'                 \itemize{
#'                    \item \code{influence: } Uses studentized residuals
#'                          from a particular non-linear model. Default.
#'                    \item \code{raw_difference: } Uses the raw differences
#'                          between \code{MP_A} and \code{MP_B}.
#'                          That is, \eqn{d_i = y_i - x_i}. Not recommended.
#'                    \item \code{absolute_difference: } Uses the absolute
#'                          difference between \code{MP_A} and \code{MP_B}.
#'                          That is, \eqn{d_i = |y_i - x_i|}. Not recommended.
#'                    \item \code{relative_difference: } Uses the relative
#'                          difference between \code{MP_A} and \code{MP_B}
#'                          measurements. That is,
#'                          \eqn{d_i = \frac{|y_i - x_i|}{|x_i|} \cdot 100\%}.
#'                    \item \code{log_difference: } Uses the log difference
#'                          between \code{MP_A} and \code{MP_B}. That is,
#'                          \eqn{d_i = \log(y_i) - \log(x_i)}.
#'                 }
#'                 Ignored if \code{method = 'qrange'}.
#'
#' @param level A \code{double}. Must be between \code{0} and \code{1}. The
#'              desired confidence level. Defaults to \code{0.99}
#'              (\eqn{99\%}).
#'
#' @param output A \code{character} string. The output type.
#'
#' @description
#' Performs outlier tests that is meant to check for multiple outliers at once.
#'
#' @details
#' For IQR (\code{method = 'iqr'}) and Chauvenet (\code{method = 'chauvenet'})
#' outlier detection methods, candidate outliers are further assessed using the
#' Tietjen-Moore test at a significance level of \code{1 - level}.
#' This additional check helps mitigate the sensitivity of these methods, which
#' can sometimes flag too many data points as outliers.
#'
#' Burnett's method (\code{method = 'burnett'}) does not typically suffer from
#' the same sensitivity issue, so no additional tests are applied.
#'
#' Important Note: Outliers identified by any of these methods should not
#' be automatically removed without further investigation. In some cases,
#' flagged data points may represent meaningful variations in the underlying
#' distribution. Careful consideration should be given before deciding to
#' exclude them.
#'
#' The format of the function output is controlled by the \code{output}
#' argument:
#' \itemize{
#'    \item \code{'visual': } Use to presen results concisely with a focus on
#'          interpretability.
#'    \item \code{'raw': } Use when further calculations or custom processing
#'          of the detected outliers is required.
#' }
#'
#' @return
#' a \code{data.table}. The dimensions depend on \code{method}, \code{variable}
#' and \code{output}.
#' @export
#'
#' @examples
#' # Required packages
#' library(smooth.commutability)
#' library(data.table)
#'
#' # Use example data from smooth.commutability package
#' test_cs_data <- copy(crp_cs_data)
#'
#' # Create an extreme value
#' test_cs_data$MP_A[1:3] <- test_cs_data$MP_A[1:3] + 26
#'
#' # Outlier test using standardized residuals and Burnett's method
#' do_outlier_analysis(
#'  data = test_cs_data,
#'  method = "burnett",
#'  variable = "influence",
#'  level = 0.95,
#'  output = "visual"
#' )
#'

do_outlier_analysis <- function(data,
                                method = "qrange",
                                variable = "influence",
                                level = 0.99,
                                output = "visual"){


  # Bind global variables
  MP_A <- MP_B <- SampleID <- NULL

  # Validate data
  data <- validate_data(data = data,
                        argument_name = "data",
                        required_columns = c("comparison",
                                             "SampleID",
                                             "ReplicateID",
                                             "MP_A",
                                             "MP_B"))

  # Validate method
  method <- validate_character(x = method,
                               argument_name = "method",
                               defaults_to = "qrange",
                               valid_inputs = c("qrange",
                                                "burnett",
                                                "chauvenet",
                                                "iqr"),
                               valid_length = 1L,
                               NA_valid = FALSE)

  # Validate variable
  variable <- validate_character(x = variable,
                                 argument_name = "variable",
                                 defaults_to = "influence",
                                 valid_inputs = c("influence",
                                                  "raw_difference",
                                                  "absolute_difference",
                                                  "relative_difference",
                                                  "log_difference"),
                                 valid_length = 1L,
                                 NA_valid = FALSE)

  # Generate Raw Outlier Test Data
  raw_output <- outlier_tests(data = data,
                              method = method,
                              variable = variable,
                              level = level)

  # Clean Outlier Test Data if Desired
  processed_output <- process_output_doa(raw_output = raw_output,
                                         output = output,
                                         method = method)

  # Copy to avoid invisibility
  processed_output <- copy(processed_output)


  # Return Outlier Analysis Results
  return(processed_output)

}
