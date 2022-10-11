#' Perform linear model assessment tests
#'
#' @param data \code{data table}, \code{list} or \code{data frame} - Clinical sample data grouped by 'comparison'. This data must include all replicated measurements, or else imprecision estimates may not be calculated.
#' @param B \code{integer} - The number of bootstrap replicates used to calculate the rejection rates of the assessment tests
#' @param method \code{character} - Which method should be used to calculate the linear model residuals used in the normality tests. Possible inputs are \code{'fg'} (default), \code{'clsi'} and \code{'ols'}. \code{'fg'} uses Deming regression defined by Fuller & Gillard. \code{'clsi'} uses Deming regression defined by CLSI EP14. \code{'ols'} uses ordinary least squared linear regression.
#' @param level \code{double} - A value between 0 and 1, that corresponds with the significance level of the assessment tests. Default level is \code{0.95}.
#' @param include_rejection_rates \code{logical} - \code{TRUE} or \code{FALSE}, where the former is the default. Setting this to \code{TRUE} ensures that assessment test rejection rates are included in the output. Setting this to \code{FALSE}, disables any bootstrap estimation and exclude rejection rates from the output.
#' @param simultaneous_testing \code{logical} - \code{TRUE} or \code{FALSE}, where the former is the default. Setting this to \code{TRUE}, entails that correction for multiple testing is performed. On the contratry, setting this to \code{FALSE}, will use 1 - level for all assessment tests.
#' @param silence \code{integer} - Should verbose be allowed? if silence is larger or equal to \code{1}, verbose is disabled. If silence is smaller than \code{1}, verbose is enabled.
#'
#' @return A \code{data table} with results concerning the performed assessment tests
#' @export
#'
#' @examples print(1)

perform_assessment_tests <- function(data, B = 2e3L, method = "fg", level = 0.95, include_rejection_rates = TRUE, simultaneous_testing = TRUE, silence = 1L){

  if(!is.data.table(data)){
    if(is.data.frame(data)){
      data <- as.data.table(data)
    }
    else if(is.list(data)){
      setDT(data)
    }
    else{
      stop("data is not of class data table, data frame or list. Make sure this is the case before trying again.")
    }
  }

  if(!is.character(data$SampleID)){
    data$SampleID <- as.character(data$SampleID)
  }

  if(!is.character(data$ReplicateID)){
    data$ReplicateID <- as.character(data$ReplicateID)
  }

  if(!any("comparison" == names(data))){
    stop("comparison was not found in data. Are you sure your data is on long-format?")
  }

  data_list <- split(x = data, by = "comparison", keep.by = FALSE)
  data_list_mor <- lapply(X = data_list, FUN = fun_of_replicates)
  impr_list <- lapply(X = data_list, FUN = global_precision_estimates)
  simultaneous_testing_alpha <- if(simultaneous_testing){
    if(silence < 1L){
      cat("We will correct for simultaneous testing", "\n", sep = "")
    }
    (1 - level) / length(data_list)
  }
  else if(!simultaneous_testing){
    if(silence < 1L){
      cat("We will NOT correct for simultaneous testing", "\n", sep = "")
    }
    1 - level
  }
  else if(!is.logical()){
    warning(paste0("simultaneous_testing should be a boolean value, not ", class(simultaneous_testing), "!"))
    1 - level
  }
  else{
    1 - level
  }

  if(B >= 1 & include_rejection_rates){

    if(silence < 1L){
      cat("Rejection rates are to be included as part of output (1)", "\n", sep = "")
    }

    resampled_data <- lapply(X = data_list, FUN = function(x) replicate(n = B, expr = resample_samples(x), simplify = FALSE))
    resampled_residuals_vs_fitted <- lapply(X = resampled_data,
                                            FUN = function(x)
                                              lapply(X = x, FUN = function(y)
                                                residuals_eqa(data = fun_of_replicates(y),
                                                               imprecision_estimates = global_precision_estimates(y),
                                                                method = method)))

    resampled_norm_tests <- lapply(X = resampled_residuals_vs_fitted,
                                   FUN = function(x)
                                     lapply(X = x,
                                            FUN = function(y)
                                              shapiro.test(y$residuals)$p.value))

    resampled_homo_tests <- lapply(X = resampled_data,
                                   FUN = function(x)
                                     lapply(X = x,
                                            FUN = function(y)
                                              breusch_pagan(list("X" = cbind(1, fun_of_replicates(y)$MP_B),
                                                                 "y" = fun_of_replicates(y)$MP_A))$p.value))

    norm_tests_rejection <- lapply(X = resampled_norm_tests, FUN = unlist) |>
      lapply(FUN = function(x) x < simultaneous_testing_alpha) |>
      lapply(FUN = function(x) data.table("rejection_rate" = mean(x))) |>
      rbindlist(idcol = "comparison")

    homo_tests_rejection <- lapply(X = resampled_homo_tests, FUN = unlist) |>
      lapply(FUN = function(x) x < simultaneous_testing_alpha) |>
      lapply(FUN = function(x) data.table("rejection_rate" = mean(x))) |>
      rbindlist(idcol = "comparison")
  }

  orig_norm_tests <- mapply(FUN = function(x, y)
                              residuals_eqa(data = x, imprecision_estimates = y, method = method),
                            data_list_mor,
                            impr_list,
                            SIMPLIFY = FALSE) |>
    lapply(FUN = function(x)
      data.table("test_name" = "Shapiro-Wilk", "test" = "normality" ,"p.value" = shapiro.test(x$residuals)$p.value)) |>
    rbindlist(idcol = "comparison")
  orig_norm_tests$conclusion <- ifelse(orig_norm_tests$p.value < simultaneous_testing_alpha,
                                       "normality rejected",
                                       "normality not rejected")
  orig_homo_tests <- lapply(X = data_list_mor,
                            FUN = function(x)
                              data.table("test_name" = "Breusch-Pagan",
                                         "test" = "variance homogeinity",
                                         "p.value" = breusch_pagan(list("X" = cbind(1, x$MP_B), "y" = x$MP_A))$p.value)) |>
    rbindlist(idcol = "comparison")
  orig_homo_tests$conclusion <- ifelse(orig_homo_tests$p.value < simultaneous_testing_alpha,
                                       "variance homogeinity rejected",
                                       "variance homogeinity not rejected")

  if(B >= 1 & include_rejection_rates){
    if(silence < 1L){
      cat("Rejection rates are to be included as part of output (2)", "\n", sep = "")
    }
    norm_tests <- merge(orig_norm_tests, norm_tests_rejection, by = "comparison")
    homo_tests <- merge(orig_homo_tests, homo_tests_rejection, by = "comparison")
  }
  else{
    norm_tests <- orig_norm_tests
    homo_tests <- orig_homo_tests
  }

  out <- rbind(norm_tests, homo_tests)
  out$p.value <- ifelse(out$p.value < 0.001, 0, out$p.value)
  out$p.value <- round(out$p.value, 3L)
  out$p.value <- ifelse(out$p.value == 0, "< 0.001", as.character(out$p.value))
  return(out)
}



