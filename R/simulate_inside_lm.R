#' Simulate Clinical Sample Data and External Quality Assessment Data and Check Whether Inside PI
#'
#' @param parameters A \code{list}, \code{data.table} or \code{data.frame} with column names representing the parameters to be included in the simulations.
#'                   Must in general include \code{n}, \code{R}, \code{cvx} and \code{cvy}. If \code{type} is included, data will
#'                   be simulated from custom non-linear functions. Note that \code{cil} and \code{ciu} also must be included if \code{type} is included.
#'                   Otherwise, if \code{type} is not included, \code{simulate_eqa_data()} from the \code{fasteqa} package will be triggered, with all relevant
#'                   parameters accepted.
#' @param m A \code{integer} larger than or equal to \code{1L}. How many external quality assessment materials should be simulated?
#' @param level A \code{double} that is larger than 0, but smaller than 1. Represents the confidence level of the estimated prediction intervals for the linear regression fit.
#' @param method A \code{character} vector of length 1. The method to be used for the prediction interval estimation. Can be one of the following: \code{"fg"}, \code{"clsi"}, \code{"ols"}.
#'
#' @details It is important that \code{parameters} holds exactly one value for each simulation parameter. Thus, if \code{parameters} is a \code{data.table} or a \code{data.frame},
#'          it should only have one row. Note that this function only gives one simulation replication. To repeat the simulations multiple times, you can either use the native \code{replicate()}
#'          function, or something similar. Alternatively, you can use the \code{simulate_confidence_level()} function of this package to do the same.
#'
#'
#' @return A \code{m}-dimensional \code{integer} vector with ones and zeros. \code{1} signify that an external quality assessment material is inside the estimated prediction interval, whereas \code{0} signify that it is outside.
#' @export
#'
#' @examples simulate_inside_lm(list(n = 20, R = 3, cvx = 0.04, cvy = 0.01, df = 5))
simulate_inside_lm <- function(parameters, m = 1, level = 0.95, method = c("fg", "clsi", "ols")){
  method <- method[1]
  SampleID <- MP_B <- NULL

  if(!any("n" == names(parameters))){
    stop("n is missing. It must be part of parameters!")
  }

  if(!is.numeric(m)){
    stop("m is not numeric. m should be an integer larger than or equal to 1.")
  }
  else if(is.numeric(m)){
    if(abs(round(m) - m) > 1e-6){
      m <- round(m)
      warning("m is not an integer. It will be rounded to its nearest integer.")
    }
  }
  if(!is.numeric(level)){
    stop("level is not numeric. level should be a numeric value in the continuous real interval (0, 1).")
  }
  else if(is.numeric(m)){
    if(level <= 0 | level >= 1){
      stop("level should not exceed 1 or be below 0.")
    }
  }

  parameters$n <- parameters$n + m
  if(any("type" == names(parameters))){
    type <- parameters$type
    if(!is.numeric(type)){
      stop("type in parameters is not numeric. It must be an integer and one of the following values: 1, 2, 3.")
    }
    else if(is.numeric(type)){
      if(abs(round(type) - type) > 1e-6){
        type <- round(type)
        if(type > 3L){
          type <- 3
        }
        else if(type < 1){
          type <- 1
        }
        warning("type is not an integer. It will be rounded to its nearest valid integer.")
      }
    }
    if(!any("cve" == names(parameters))){
      parameters$cve <- 0
    }
    simulated_cs_data <- simulate_eqa_data_custom_cpp(parameters = parameters, type = type, AR = TRUE) |> setDT()
    simulated_eq_data <- simulated_cs_data[SampleID %in% sample(x = SampleID[MP_B > min(MP_B) & MP_B < max(MP_B)], size = m, replace = FALSE),]
    simulated_cs_data <- simulated_cs_data[!SampleID %in% simulated_eq_data$SampleID]
    cs_imprecsion <- global_precision_estimates(data = simulated_cs_data)
    simulated_cs_data <- fun_of_replicates(data = simulated_cs_data, fun = "mean") |> setDT()
    simulated_eq_data <- fun_of_replicates(data = simulated_eq_data, fun = "mean") |> setDT()
    if(sum(is.na(simulated_cs_data$MP_B)) + sum(is.na(simulated_cs_data$MP_A)) > 0){
      simulated_cs_data <- simulate_eqa_data_custom_cpp(parameters = parameters, type = type, AR = TRUE) |> setDT()
      simulated_eq_data <- simulated_cs_data[SampleID %in% sample(x = SampleID[MP_B > min(MP_B) & MP_B < max(MP_B)], size = m, replace = FALSE),]
      simulated_cs_data <- simulated_cs_data[!SampleID %in% simulated_eq_data$SampleID]
      cs_imprecsion <- global_precision_estimates(data = simulated_cs_data)
      simulated_cs_data <- fun_of_replicates(data = simulated_cs_data, fun = "mean") |> setDT()
      simulated_eq_data <- fun_of_replicates(data = simulated_eq_data, fun = "mean") |> setDT()
      if(sum(is.na(simulated_cs_data$MP_B)) + sum(is.na(simulated_cs_data$MP_A)) > 0){
        return(NA_integer_)
      }
    }
    return(predict_eqa(data = simulated_cs_data, new_data = simulated_eq_data, imprecision_estimates = cs_imprecsion, R = parameters$R, R_ratio = 1, method = method, level = level, rounding = 3L)$inside)
  }
  else{
    simulated_cs_data <- simulate_eqa_data(parameters = parameters) |> setDT()
    simulated_eq_data <- simulated_cs_data[SampleID %in% sample(x = SampleID[MP_B > min(MP_B) & MP_B < max(MP_B)], size = m, replace = FALSE),]
    simulated_cs_data <- simulated_cs_data[!SampleID %in% simulated_eq_data$SampleID]
    cs_imprecsion <- global_precision_estimates(data = simulated_cs_data)
    simulated_cs_data <- fun_of_replicates(data = simulated_cs_data, fun = "mean") |> setDT()
    simulated_eq_data <- fun_of_replicates(data = simulated_eq_data, fun = "mean") |> setDT()
    if(sum(is.na(simulated_cs_data$MP_B)) + sum(is.na(simulated_cs_data$MP_A)) > 0){
      simulated_cs_data <- simulate_eqa_data(parameters = parameters) |> setDT()
      simulated_eq_data <- simulated_cs_data[SampleID %in% sample(x = SampleID[MP_B > min(MP_B) & MP_B < max(MP_B)], size = m, replace = FALSE),]
      simulated_cs_data <- simulated_cs_data[!SampleID %in% simulated_eq_data$SampleID]
      cs_imprecsion <- global_precision_estimates(data = simulated_cs_data)
      simulated_cs_data <- fun_of_replicates(data = simulated_cs_data, fun = "mean") |> setDT()
      simulated_eq_data <- fun_of_replicates(data = simulated_eq_data, fun = "mean") |> setDT()
      if(sum(is.na(simulated_cs_data$MP_B)) + sum(is.na(simulated_cs_data$MP_A)) > 0){
        return(NA_integer_)
      }
    }
    return(predict_eqa(data = simulated_cs_data, new_data = simulated_eq_data, imprecision_estimates = cs_imprecsion, R = parameters$R, R_ratio = 1, method = method, level = level, rounding = 3L)$inside)
  }
}

#' Simulate Clinical Sample Data and External Quality Assessment Data and Check Whether Inside PIs
#'
#' @param N An \code{integer} that represents the number of replicated inside checks.
#' @param m An \code{integer} larger than or equal to \code{1L}. How many external quality assessment materials should be simulated?
#' @param x Typically an \code{integer}, but not necessarily. Represents the identifier name for the \code{N} inside simulations.
#' @param parameters A \code{list}, \code{data.table} or \code{data.frame} with column names representing the parameters to be included in the simulations.
#'                   Must in general include \code{n}, \code{R}, \code{cvx} and \code{cvy}. If \code{type} is included, data will
#'                   be simulated from custom non-linear functions. Note that \code{cil} and \code{ciu} also must be included if \code{type} is included.
#'                   Otherwise, if \code{type} is not included, \code{simulate_eqa_data()} from the \code{fasteqa} package will be triggered, with all its choices
#'                   of parameters accepted.
#' @param level A \code{double} that is larger than 0, but smaller than 1. Represents the confidence level of the estimated prediction intervals for the linear regression model fit.
#' @param method A \code{character} vector of length 1. The method to be used for the prediction interval estimation. Can be one of the following: \code{"fg"}, \code{"clsi"}, \code{"ols"}.
#' @param attach A non-missing \code{logical} value (\code{TRUE} / \code{FALSE}). If set to \code{TRUE}, the relevant simulation parameters are attached to the output for reference.
#' @param replication_id A non-missing \code{logical} value (\code{TRUE} / \code{FALSE}). If set to \code{TRUE}, inside check replications for each \code{1:N} is given an identifier in the output. Particularly relevant if \code{m > 1}.
#'
#' @return A \code{data.table} containing the inside replications for the inputs.
#' @export
#'
#' @examples simulate_insides(parameters = list(n = 25, R = 3, cvx = 0.01,
#'  cvy = 0.04, qran = 0.3, qpos = 1, mmax = 5, df = 5))
simulate_insides_lm <- function(N = 1e2L, m = 1, x = 1, parameters, level = 0.95, method = c("fg", "clsi", "ols"), attach = TRUE, replication_id = TRUE){
  method <- method[1]
  out <- replicate(n = N, expr = simulate_inside_lm(parameters = parameters, m = m, level = level), simplify = FALSE)
  if(m <= 1){
    if(replication_id){
      out <- lapply(out, function(i) data.table(replication = x, inside = i))
    }
    else{
      out <- lapply(out, function(i) data.table(inside = i))
    }

  }
  else{
    if(replication_id){
      out <- sapply(1:length(out), function(i) data.table(replication = x, id = rep(i, length(out[[i]])), SampleID = 1:length(out[[i]]), inside = out[[i]]), simplify = FALSE)
    }
    else{
      out <- sapply(1:length(out), function(i) data.table(SampleID = 1:length(out[[i]]), inside = out[[i]]), simplify = FALSE)
    }
  }
  out <- rbindlist(out)
  if(attach){
    parameters_extended <- lapply(parameters, function(x) rep(x, nrow(out))) |> setDT()
    out <- cbind(parameters_extended, out)
  }
  else{
    return(out)
  }
  return(out)
}
