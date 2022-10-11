#' All calculations regarding imprecision
#'
#' @param data \code{list}, \code{data frame} or \code{data table} that is grouped by \code{"comparison"}. Furthermore, must contain the following columns: \code{comparison}, \code{SampleID}, \code{ReplicateID}, \code{MP_A} and \code{MP_B}
#' @param B Number of bootstrap replicates used to estimate bootstrap confidence intervals. The default is 2000, which is the typical for bootstrap confidence intervals. Note that if you have more than five unique IVD-MDs in \code{data}, it may take approximately two seconds to run the resampling
#' @param type Type of bootstrap confidence interval. There are four options:
#' \itemize{
#'   \item{\code{normal}: }{Standard normal bootstrap confidence intervals}
#'   \item{\code{basic}: }{Basic bootstrap confidence intervals}
#'   \item{\code{percentile}: }{Percentile bootstrap confidence intervals}
#'   \item{\code{BCa}: }{Bias- and skewness-corrected bootstrap confidence intervals}
#' }
#' @param level Confidence level of bootstrap confidence interval. A 95 percent confidence level is the default.
#'
#' @description Obtain all necessary information on imprecision data for each unique pair of IVD-MDs in your data. The output is on the form required by \code{merge_results}(), making it very useful.
#'
#' @return A \code{data table} with entries \code{CV_A}, \code{CV_A_lwr}, \code{CV_A_upr}, \code{CV_B}, \code{CV_B_lwr}, \code{CV_B_upr}, \code{lambda}, \code{lambda_lwr}, \code{lambda_upr}, \code{Var_A}, \code{Var_A_lwr}, \code{Var_A_upr}, \code{Var_B}, \code{Var_B_lwr}, \code{Var_B_upr}
#' @export
#'
#' @examples print(1)

estimate_imprecision_data <- function(data, B = 2e3, type = "percentile", level = 0.95){

  if(!is.data.table(data)){

    if(is.data.frame(data)){
      data <- as.data.table(data)
    }
    else if(is.list(data)){
      setDT(data)
    }
    else{
      stop("data is not of class data table, data frame or list! Make sure this is the case for data")
    }
  }

  type_numeric <- if(type=="percentile"){3}else if(type=="basic"){2}else if(type=="normal"){1}else if(type=="BCa" | type=="bca"){4}else{3}
  bootstrap_ci <- BCa_bootstrap_ci <- global_precision_estimates <- leave_one_out <- resample_samples <- NULL
  if(!is.character(data$SampleID)){
    data$SampleID <- as.character(data$SampleID)
  }
  if(!is.character(data$ReplicateID)){
    data$ReplicateID <- as.character(data$ReplicateID)
  }
  if(!any("comparison" == names(data))){
    stop("comparison was not found in data. Make sure it is included. Are you sure data is on long format?")
  }

  data_list <- split(data, by = "comparison", keep.by = FALSE)
  orig_imps <- lapply(X = data_list, FUN = global_precision_estimates)
  rsml_data <- lapply(X = data_list, function(x) replicate(n = B,
                                                           expr = setDT(resample_samples(data = x)),
                                                           simplify = FALSE))
  boot_imps <- lapply(X = rsml_data, FUN = function(x) lapply(x, global_precision_estimates)) |>
    lapply(FUN = function(x) lapply(x, setDT)) |>
    lapply(FUN = rbindlist, idcol = FALSE) |>
    lapply(FUN = as.list)

  if(type_numeric == 4){

    loo_data <- lapply(X = data_list,
                       FUN = function(x) sapply(X = 1:length(unique(x$SampleID)),
                                                FUN = function(y) leave_one_out(x, y),
                                                simplify = FALSE)) |>
      lapply(FUN = function(x) lapply(x, FUN = setDT))

    loo_imps <- lapply(X = loo_data, FUN = function(x) lapply(x, function(y) global_precision_estimates(y, silence = 1L) |> setDT())) |>
      lapply(FUN = rbindlist, idcol = FALSE) |>
      lapply(FUN = as.list)

    boot_imps_cis <- as.list(1:length(boot_imps))
    names(boot_imps_cis) <- names(boot_imps)

    for(i in 1:length(boot_imps)){
      boot_imps_cis[[i]] <- mapply(FUN = function(x, y, z) BCa_bootstrap_ci(x, y, z, level),
                                   boot_imps[[i]],
                                   loo_imps[[i]],
                                   orig_imps[[i]],
                                   SIMPLIFY = FALSE)
    }

    boot_imps_cis_tabled <- lapply(X = boot_imps_cis,
                                   FUN = function(x) list("Var_A_lwr" = if(x$Var_A[1] < 0 | is.na(x$Var_A[1])){NA}else{max(1e-12, x$Var_A[1])},
                                                          "Var_A_upr" = if(x$Var_A[2] < 0 | is.na(x$Var_A[2])){NA}else if(x$Var_A[2] < x$Var_A[1]){NA}else{max(1e-12, x$Var_A[2])},
                                                          "Var_B_lwr" = if(x$Var_B[1] < 0 | is.na(x$Var_B[1])){NA}else{max(1e-12, x$Var_B[1])},
                                                          "Var_B_upr" = if(x$Var_B[2] < 0 | is.na(x$Var_B[2])){NA}else if(x$Var_B[2] < x$Var_B[1]){NA}else{max(1e-12, x$Var_B[2])},
                                                          "CV_A_lwr" = if(x$CV_A[1] < 0 | is.na(x$CV_A[1])){NA}else{max(1e-12, x$CV_A[1])},
                                                          "CV_A_upr" = if(x$CV_A[2] < 0 | is.na(x$CV_A[2])){NA}else if(x$CV_A[2] < x$CV_A[1]){NA}else{max(1e-12, x$CV_A[2])},
                                                          "CV_B_lwr" = if(x$CV_B[1] < 0 | is.na(x$CV_B[1])){NA}else{max(1e-12, x$CV_B[1])},
                                                          "CV_B_upr" = if(x$CV_B[2] < 0 | is.na(x$CV_B[2])){NA}else if(x$CV_B[2] < x$CV_B[1]){NA}else{max(1e-12, x$CV_B[2])},
                                                          "lambda_lwr" = if(x$lambda[1] < 0 | is.na(x$lambda[1])){NA}else{max(1e-12, x$lambda[1])},
                                                          "lambda_upr" = if(x$lambda[2] < 0 | is.na(x$lambda[2])){NA}else if(x$lambda[2] < x$lambda[1]){NA}else{max(1e-12, x$lambda[1])})) |>
      lapply(setDT) |> rbindlist(idcol = "comparison")
    orig_imps <- lapply(orig_imps, setDT) |> rbindlist(idcol = "comparison")
    out <- merge(orig_imps, boot_imps_cis_tabled, by = "comparison", sort = FALSE)
    setcolorder(x = out, neworder = c("comparison","CV_A", "CV_A_lwr", "CV_A_upr", "CV_B", "CV_B_lwr", "CV_B_upr", "lambda", "lambda_lwr", "lambda_upr", "Var_A", "Var_A_lwr", "Var_A_upr", "Var_B", "Var_B_lwr", "Var_B_upr"))
    return(out)
  }

  boot_imps_cis <- as.list(1:length(boot_imps))
  names(boot_imps_cis) <- names(boot_imps)
  for(i in 1:length(boot_imps)){
    boot_imps_cis[[i]] <- mapply(FUN = function(boot, orig) bootstrap_ci(boot, orig, type_numeric, level = level),
                                 boot_imps[[i]],
                                 orig_imps[[i]],
                                 SIMPLIFY = FALSE)
  }
  boot_imps_cis_tabled <- lapply(X = boot_imps_cis,
                                 FUN = function(x) list("Var_A_lwr" = if(x$Var_A[1] < 0 | is.na(x$Var_A[1])){NA}else{max(1e-12, x$Var_A[1])},
                                                        "Var_A_upr" = if(x$Var_A[2] < 0 | is.na(x$Var_A[2])){NA}else if(x$Var_A[2] < x$Var_A[1]){NA}else{max(1e-12, x$Var_A[2])},
                                                        "Var_B_lwr" = if(x$Var_B[1] < 0 | is.na(x$Var_B[1])){NA}else{max(1e-12, x$Var_B[1])},
                                                        "Var_B_upr" = if(x$Var_B[2] < 0 | is.na(x$Var_B[2])){NA}else if(x$Var_B[2] < x$Var_B[1]){NA}else{max(1e-12, x$Var_B[2])},
                                                        "CV_A_lwr" = if(x$CV_A[1] < 0 | is.na(x$CV_A[1])){NA}else{max(1e-12, x$CV_A[1])},
                                                        "CV_A_upr" = if(x$CV_A[2] < 0 | is.na(x$CV_A[2])){NA}else if(x$CV_A[2] < x$CV_A[1]){NA}else{max(1e-12, x$CV_A[2])},
                                                        "CV_B_lwr" = if(x$CV_B[1] < 0 | is.na(x$CV_B[1])){NA}else{max(1e-12, x$CV_B[1])},
                                                        "CV_B_upr" = if(x$CV_B[2] < 0 | is.na(x$CV_B[2])){NA}else if(x$CV_B[2] < x$CV_B[1]){NA}else{max(1e-12, x$CV_B[2])},
                                                        "lambda_lwr" = if(x$lambda[1] < 0 | is.na(x$lambda[1])){NA}else{max(1e-12, x$lambda[1])},
                                                        "lambda_upr" = if(x$lambda[2] < 0 | is.na(x$lambda[2])){NA}else if(x$lambda[2] < x$lambda[1]){NA}else{max(1e-12, x$lambda[1])})) |>
    lapply(setDT) |> rbindlist(idcol = "comparison")
  orig_imps <- lapply(orig_imps, setDT) |> rbindlist(idcol = "comparison")
  out <- merge(orig_imps, boot_imps_cis_tabled, by = "comparison", sort = FALSE)

  setcolorder(x = out, neworder = c("comparison","CV_A", "CV_A_lwr", "CV_A_upr", "CV_B", "CV_B_lwr", "CV_B_upr", "lambda", "lambda_lwr", "lambda_upr", "Var_A", "Var_A_lwr", "Var_A_upr", "Var_B", "Var_B_lwr", "Var_B_upr"))

  return(out)
}

