#' All calculations regarding imprecision
#'
#' @param data \code{list}, \code{data frame} or \code{data table} that is grouped by \code{"comparison"}. Furthermore, must contain the following columns: \code{comparison}, \code{SampleID}, \code{ReplicateID}, \code{MP_A} and \code{MP_B}.
#' @param B \code{Integer} - Number of bootstrap replicates used to estimate bootstrap confidence intervals. The default is 2000, which is the typical for bootstrap confidence intervals. Note that if you have more than five unique IVD-MDs in \code{data}, it may take approximately two seconds to run the resampling.
#' @param type \code{Character} - Type of bootstrap confidence interval. There are four options:
#' \itemize{
#'   \item{\code{normal}: }{Standard normal bootstrap confidence intervals}
#'   \item{\code{basic}: }{Basic bootstrap confidence intervals}
#'   \item{\code{percentile}: }{Percentile bootstrap confidence intervals}
#'   \item{\code{BCa}: }{Bias- and skewness-corrected bootstrap confidence intervals}
#' }
#' @param level \code{Numeric} - Confidence level of the bootstrap confidence intervals. A 95 percent confidence level is the default.
#' @param invalid_NA A \code{logical} value. If set to \code{TRUE}, the function will return \code{NA} for zeta when encountering invalid input or computation errors instead of raising an error. While this is not generally recommended due to the risk of masking potential issues, it can be useful in certain scenarios where uninterrupted execution is a priority.
#' @description Obtain all necessary information on imprecision data for each unique pair of IVD-MDs in your data. The output is on the form required by \code{merge_results}(), making it very useful.
#'
#' @return A \code{data table} with entries \code{CV_A}, \code{CV_A_lwr}, \code{CV_A_upr}, \code{CV_B}, \code{CV_B_lwr}, \code{CV_B_upr}, \code{lambda}, \code{lambda_lwr}, \code{lambda_upr}, \code{Var_A}, \code{Var_A_lwr}, \code{Var_A_upr}, \code{Var_B}, \code{Var_B_lwr}, \code{Var_B_upr}
#' @export
#'
#' @examples print(1)

estimate_imprecision_data <- function(data, B = 2e3L, type = "percentile", level = 0.95, invalid_NA = FALSE){

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

  type_numeric <- switch(type, "percentile" = 3, "basic" = 2, "normal" = 1, "BCa" = 4, "bca" = 4, 3)
  bootstrap_ci <- BCa_bootstrap_ci <- global_precision_estimates <- leave_one_out <- resample_samples <- SampleID <- ReplicateID <- comparison <- NULL

  # Checks if 'SampleID' and 'ReplicateID' are character vectors
  # If they are not, they will be converted to character vectors
  if(isFALSE(is.character(data$SampleID))){
    data[, SampleID := as.character(SampleID)]
  }
  if(isFALSE(is.character(data$ReplicateID))){
    data[, ReplicateID := as.character(ReplicateID)]
  }

  data_list <- split(data, by = "comparison", keep.by = FALSE)
  orig_imps <- data[, global_precision_estimates(.SD), by = comparison]

  # Checks if 'B' is 'NULL' or 'NA'. If this is the case, we do not return estimated bootstrap confidence intervals
  if(is.null(B) || length(B) == 0 || is.na(B)){
    return(orig_imps)
  }

  # Checks if 'B' is a character string. If this is the case, we return NA or throw an error depending on the 'invalid_NA' input
  if(isTRUE(is.character(B))){
    if(isTRUE(invalid_NA)){
      orig_imps$Var_A <- NA_real_
      orig_imps$Var_B <- NA_real_
      orig_imps$CV_A <- NA_real_
      orig_imps$CV_B <- NA_real_
      orig_imps$lambda <- NA_real_
      return(orig_imps)
    }
    else{
      stop(sprintf("'B' ['%s'] is a character string. Make sure it is a integer equal to or larger than 50 and try again.
                 \nIf you do not wish to estimate bootstrap confidence intervals for the IVD-MD imprecision estimates, set 'B' to either 'NULL' or 'NA'.",
                 B))
    }
  }

  # Checks if 'B' is positive. If this is not the case, return NA or throw an error depending on the 'invalid_NA' input
  B_is_positive <- B >= 0
  if(isFALSE(B_is_positive)){
    if(isTRUE(invalid_NA)){
      orig_imps$Var_A <- NA_real_
      orig_imps$Var_B <- NA_real_
      orig_imps$CV_A <- NA_real_
      orig_imps$CV_B <- NA_real_
      orig_imps$lambda <- NA_real_
      return(orig_imps)
    }
    else{
      stop(sprintf("'B' [%d] is a negative value. Make sure it is a integer equal to or larger than 50 and try again.
                 \nIf you do not wish to estimate bootstrap confidence intervals for the IVD-MD imprecision estimates, set 'B' to either 'NULL' or 'NA'.",
                 B))
    }
  }

  # Checks if 'B' is an integer. If this is not the case, round it to the nearest integer
  B_is_integer <- abs(B - round(B)) < .Machine$double.eps ** 0.5
  if(isFALSE(B_is_integer)){
    B <- round(B)
  }

  if(B < 50L && B >= 1L){
    warning(sprintf("Ideally, 'B' should exceed 2,000 to ensure stable bootstrap confidence intervals for IVD-MD imprecision estimates.
                 \nWith the current 'B' value [%d], which is less than 50, the confidence intervals might be quite unstable. Due to the low 'B' value, no bootstrap confidence intervals are estimated. Increase 'B' above 50 to enable confidence interval estimation.",
                 B), immediate. = TRUE)

    return(orig_imps)

  }

  else if(B < 1L){
    return(orig_imps)
  }

  # Estimation of confidence intervals of IVD-MD imprecision estimates depending on the input of 'type' and 'level'
  orig_imps_list <- split(orig_imps, by = "comparison", keep.by = FALSE)
  resampled_data <- lapply(X = data_list, FUN = function(x) replicate(n = B, expr = resample_samples(data = x, silence = 1), simplify = FALSE))
  bootstrapped_imps <- lapply(X = resampled_data, FUN = function(x) lapply(x, global_precision_estimates))
  bootstrapped_imps <- lapply(X = bootstrapped_imps, FUN = rbindlist)

  if(type_numeric == 4){
    loo_data <- lapply(X = data_list,
                       FUN = function(x) sapply(X = 1:length(unique(x$SampleID)),
                                                FUN = function(y) leave_one_out(x, y),
                                                simplify = FALSE))
    loo_imps <- lapply(X = loo_data, FUN = function(x) lapply(x, global_precision_estimates))
    loo_imps <- lapply(X = loo_imps, FUN = rbindlist)

    bootstrapped_cis <- as.list(1:length(bootstrapped_imps))
    names(bootstrapped_cis) <- names(bootstrapped_imps)

    for(i in 1:length(bootstrapped_imps)){
      bootstrapped_cis[[i]] <- mapply(FUN = function(x, y, z) BCa_bootstrap_ci(x, y, z, level),
                                   bootstrapped_imps[[i]],
                                   loo_imps[[i]],
                                   orig_imps_list[[i]],
                                   SIMPLIFY = FALSE)
    }

    bootstrapped_cis_tabled <- lapply(X = bootstrapped_cis,
                                   FUN = function(x) list("Var_A_lwr" = if(x$Var_A[1] < 0 | is.na(x$Var_A[1])){NA}else{max(.Machine$double.eps, x$Var_A[1])},
                                                          "Var_A_upr" = if(x$Var_A[2] < 0 | is.na(x$Var_A[2])){NA}else if(x$Var_A[2] < x$Var_A[1]){NA}else{max(.Machine$double.eps, x$Var_A[2])},
                                                          "Var_B_lwr" = if(x$Var_B[1] < 0 | is.na(x$Var_B[1])){NA}else{max(.Machine$double.eps, x$Var_B[1])},
                                                          "Var_B_upr" = if(x$Var_B[2] < 0 | is.na(x$Var_B[2])){NA}else if(x$Var_B[2] < x$Var_B[1]){NA}else{max(.Machine$double.eps, x$Var_B[2])},
                                                          "CV_A_lwr" = if(x$CV_A[1] < 0 | is.na(x$CV_A[1])){NA}else{max(.Machine$double.eps, x$CV_A[1])},
                                                          "CV_A_upr" = if(x$CV_A[2] < 0 | is.na(x$CV_A[2])){NA}else if(x$CV_A[2] < x$CV_A[1]){NA}else{max(.Machine$double.eps, x$CV_A[2])},
                                                          "CV_B_lwr" = if(x$CV_B[1] < 0 | is.na(x$CV_B[1])){NA}else{max(.Machine$double.eps, x$CV_B[1])},
                                                          "CV_B_upr" = if(x$CV_B[2] < 0 | is.na(x$CV_B[2])){NA}else if(x$CV_B[2] < x$CV_B[1]){NA}else{max(.Machine$double.eps, x$CV_B[2])},
                                                          "lambda_lwr" = if(x$lambda[1] < 0 | is.na(x$lambda[1])){NA}else{max(.Machine$double.eps, x$lambda[1])},
                                                          "lambda_upr" = if(x$lambda[2] < 0 | is.na(x$lambda[2])){NA}else if(x$lambda[2] < x$lambda[1]){NA}else{max(.Machine$double.eps, x$lambda[2])})) |>
      lapply(setDT) |> rbindlist(idcol = "comparison")

    out <- merge(orig_imps, bootstrapped_cis_tabled, by = "comparison", sort = FALSE)
    setcolorder(x = out, neworder = c("comparison","CV_A", "CV_A_lwr", "CV_A_upr", "CV_B", "CV_B_lwr", "CV_B_upr", "lambda", "lambda_lwr", "lambda_upr", "Var_A", "Var_A_lwr", "Var_A_upr", "Var_B", "Var_B_lwr", "Var_B_upr"))
    return(out)
  }

  bootstrapped_cis <- as.list(1:length(bootstrapped_imps))
  names(bootstrapped_cis) <- names(bootstrapped_imps)

  for(i in 1:length(bootstrapped_imps)){
    bootstrapped_cis[[i]] <- mapply(FUN = function(boot, orig) bootstrap_ci(boot, orig, type_numeric, level = level),
                                 bootstrapped_imps[[i]],
                                 orig_imps_list[[i]],
                                 SIMPLIFY = FALSE)
  }
  bootstrapped_cis_tabled <- lapply(X = bootstrapped_cis,
                                 FUN = function(x) list("Var_A_lwr" = if(x$Var_A[1] < 0 | is.na(x$Var_A[1])){NA}else{max(.Machine$double.eps, x$Var_A[1])},
                                                        "Var_A_upr" = if(x$Var_A[2] < 0 | is.na(x$Var_A[2])){NA}else if(x$Var_A[2] < x$Var_A[1]){NA}else{max(.Machine$double.eps, x$Var_A[2])},
                                                        "Var_B_lwr" = if(x$Var_B[1] < 0 | is.na(x$Var_B[1])){NA}else{max(.Machine$double.eps, x$Var_B[1])},
                                                        "Var_B_upr" = if(x$Var_B[2] < 0 | is.na(x$Var_B[2])){NA}else if(x$Var_B[2] < x$Var_B[1]){NA}else{max(.Machine$double.eps, x$Var_B[2])},
                                                        "CV_A_lwr" = if(x$CV_A[1] < 0 | is.na(x$CV_A[1])){NA}else{max(.Machine$double.eps, x$CV_A[1])},
                                                        "CV_A_upr" = if(x$CV_A[2] < 0 | is.na(x$CV_A[2])){NA}else if(x$CV_A[2] < x$CV_A[1]){NA}else{max(.Machine$double.eps, x$CV_A[2])},
                                                        "CV_B_lwr" = if(x$CV_B[1] < 0 | is.na(x$CV_B[1])){NA}else{max(.Machine$double.eps, x$CV_B[1])},
                                                        "CV_B_upr" = if(x$CV_B[2] < 0 | is.na(x$CV_B[2])){NA}else if(x$CV_B[2] < x$CV_B[1]){NA}else{max(.Machine$double.eps, x$CV_B[2])},
                                                        "lambda_lwr" = if(x$lambda[1] < 0 | is.na(x$lambda[1])){NA}else{max(.Machine$double.eps, x$lambda[1])},
                                                        "lambda_upr" = if(x$lambda[2] < 0 | is.na(x$lambda[2])){NA}else if(x$lambda[2] < x$lambda[1]){NA}else{max(.Machine$double.eps, x$lambda[2])})) |>
    lapply(setDT) |> rbindlist(idcol = "comparison")

  out <- merge(orig_imps, bootstrapped_cis_tabled, by = "comparison", sort = FALSE)
  setcolorder(x = out, neworder = c("comparison","CV_A", "CV_A_lwr", "CV_A_upr", "CV_B", "CV_B_lwr", "CV_B_upr", "lambda", "lambda_lwr", "lambda_upr", "Var_A", "Var_A_lwr", "Var_A_upr", "Var_B", "Var_B_lwr", "Var_B_upr"))

  return(out)
}

