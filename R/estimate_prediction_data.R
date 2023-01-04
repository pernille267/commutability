#' Estimate prediction intervals for Deming regression based on EQA data
#'
#' @param data \code{list} - \code{data table} or \code{data frame} - Data containing clinical samples' measurements
#' @param new_data \code{list} - \code{data table} or \code{data frame} - Data containing either external quality assessment materials' measurements or new values for \code{MP_B}. Including \code{SampleID}, \code{MP_A}, \code{MP_B} will create commutability evaluation data based on control material measurements, whereas having only \code{MP_B} yields prediction band data
#' @param B \code{integer} - How many bootstrap resamples should be used to estimate the inside rates for the external quality assessment materials? Set \code{B = NULL} for not including "inside-pi" rates.
#' @param method \code{character} - 1 x 1 character signifying which method should be applied for estimating the prediction intervals
#' @param level \code{float} - Number between 0 and 1, that is the confidence level of the estimated prediction intervals
#' @param rounding \code{integer} - How many decimals should be returned in the output. Default is \code{3}
#'
#' @return \code{data table} containing commutability evaluation data or prediction band data depending on the given new_data
#' @export
#'
#' @examples print(1)
estimate_prediction_data <- function(data, new_data = NULL, B = NULL, method = "fg", level = 0.99, rounding = 3L){

  if(!is.data.table(data)){
    if(is.data.frame(data) | is.list(data)){
      setDT(data)
    }
    else{
      stop("data is not a data table, list or data frame. Calculations are terminated")
    }
  }

  if(is.null(new_data)){
    new_data <- lapply(X = split(data, by = "comparison", keep.by = FALSE),
                            FUN = function(x) list("MP_B" = x$MP_B) |> setDT()) |>
      rbindlist(idcol = "comparison")
  }
  if(is.character(new_data)){
    converted_string <- stri_split(new_data, regex = "[:punct:]")[[1]][which(stri_split(new_data, regex = "[:punct:]")[[1]] != "")]
    if(length(converted_string) != 2){
      stop("if new_data is passed as character, it must not be divided into something other than two elements. Calculations are terminated")
    }
    if(converted_string[1] == "gen"){
      n_gens <- converted_string[2]
      if(stri_detect(str = n_gens, regex = "[:digit:]")){
        n_gens <- round(as.numeric(n_gens))
        if(n_gens >= 1e4){
          n_gens <- 1e4
        }
      }
      else{
        warning("gen[:punct:]* was not a valid number")
        n_gens <- 1e2
      }
      new_data <- lapply(X = split(data, by = "comparison", keep.by = FALSE),
                         FUN = function(x) list("MP_B" = seq(min(na.omit(x$MP_B)), max(na.omit(x$MP_B)), length.out = n_gens)) |> setDT()) |>
        rbindlist(idcol = "comparison")
    }
    else if(converted_string[2] == "gen"){
      n_gens <- converted_string[1]
      if(stri_detect(str = n_gens, regex = "[:digit:]")){
        n_gens <- round(as.numeric(n_gens))
        if(n_gens >= 1e4){
          n_gens <- 1e4
        }
      }
      else{
        warning("gen[:punct:]#: # was not a valid number, and 100 is used by default")
        n_gens <- 1e2
      }
      new_data <- lapply(X = split(data, by = "comparison", keep.by = FALSE),
                         FUN = function(x) list("MP_B" = seq(min(x$MP_B), max(x$MP_B), length.out = n_gens)) |> setDT()) |>
        rbindlist(idcol = "comparison")
    }
    else{
      stop("If new_data is passed as character, it must be on the form gen[:punct:]#, where # is a whole number. This was not the case and calculations are terminated")
    }
  }

  if(!is.data.table(new_data)){
    if(is.data.frame(new_data) | is.list(new_data)){
      setDT(new_data)
    }
    else{
      stop("new_data is not a data table, list or data frame. Calculations are terminated")
    }
  }
  if(!any("ReplicateID" == names(data))){
    stop("ReplicateID was not found in data. Calculations are accordingly terminated")
  }
  if(!any("comparison" == names(data))){
    stop("comparison was not found in data. Calculations are accordingly terminated")
  }

  if(!any("comparison" == names(new_data))){
    stop("comparison was not found in new_data. Calculations are accordingly terminated")
  }

  data_list <- split(x = data, by = "comparison", keep.by = FALSE)
  new_data_list <- split(x = new_data, by = "comparison", keep.by = FALSE)
  impr_data <- lapply(X = data_list, FUN = function(x) global_precision_estimates(data = x) |> setDT())

  uniq_data <- lapply(X = data_list, FUN = function(x) (unique(x$ReplicateID[which(!is.na(x$ReplicateID))])))
  R_cs_list <- lapply(X = data_list, FUN = function(x) table(x$ReplicateID) / length(x$ReplicateID)) |>
    mapply(FUN = function(x, y) cumsum(sort(x / (1 / length(y)), decreasing = TRUE)), uniq_data, SIMPLIFY = FALSE) |>
    mapply(FUN = function(x, y) which(x > length(y) * (0.5 ** (1 / length(y))))[1], uniq_data, SIMPLIFY = FALSE) |>
    lapply(FUN = function(x) unname(as.vector(x)))
  copy_data_list <- data_list
  data_list <- lapply(X = data_list, FUN = fun_of_replicates)
  if(any("ReplicateID" == names(new_data))){
    R_eq_list <- lapply(X = new_data_list, FUN = function(x) table(x$ReplicateID) / length(x$ReplicateID)) |>
      mapply(FUN = function(x, y) cumsum(sort(x / (1/length(y)), decreasing = TRUE)), uniq_data, SIMPLIFY = FALSE) |>
      mapply(FUN = function(x, y) which(x > length(y) * (0.5 ** (1 / length(y))))[1], uniq_data, SIMPLIFY = FALSE) |>
      lapply(FUN = function(x) unname(as.vector(x)))

    if(any("SampleID" == names(new_data))){
      new_data_list <- lapply(X = new_data_list, FUN = fun_of_replicates)

      if(!is.null(B)){
        resample_data_list <- lapply(X = copy_data_list, FUN = function(x) replicate(n = B, expr = resample_samples(x), simplify = FALSE))
        resample_impr_data_list <- lapply(X = resample_data_list, FUN = function(x) lapply(X = x, FUN = function(y) global_precision_estimates(y) |> setDT()))
        resample_fun_data_list <- lapply(X = resample_data_list, FUN = function(x) lapply(X = x, FUN = function(y) fun_of_replicates(y)))

        inside_rates <- mapply(FUN = function(x, y, z, w)
          mapply(FUN = function(a, b) predict_eqa(data = a,
                                                  new_data = y,
                                                  imprecision_estimates = b,
                                                  R = w), x, z, SIMPLIFY = FALSE),
          resample_fun_data_list, new_data_list, resample_impr_data_list, R_eq_list, SIMPLIFY = FALSE) |>
          lapply(FUN = function(x) lapply(x, FUN = function(y) setDT(y)) |> rbindlist()) |>
          lapply(FUN = function(x) lapply(split(x, by = "SampleID", keep.by = FALSE),
                                          FUN = function(y) list("inside_rate" = mean(y$inside)) |>
                                            setDT()) |> rbindlist(idcol = "SampleID")) |>
          rbindlist(idcol = "comparison")
      }




    }
    else{
      stop("ReplicateID was found, but SampleID was not. Both or none must be given in new_data")
    }
  }

  is_ce <- lapply(X = new_data_list, FUN = function(x) all(c("SampleID", "MP_A", "MP_B") %in% names(x))) |> unlist() |> all()
  if(!is_ce){
    is_pb <- lapply(X = new_data_list, FUN = function(x) any("MP_B" == names(x))) |> unlist() |> all()
  }

  if(is_ce){
    out <- mapply(FUN = function(x, y, z, w) setDT(predict_eqa(data = x,
                                                               new_data = y,
                                                               imprecision_estimates = z,
                                                               R = w,
                                                               method = method,
                                                               level = level,
                                                               rounding = rounding)),
                  data_list, new_data_list, impr_data, R_eq_list, SIMPLIFY = FALSE) |>
      rbindlist(idcol = "comparison")
    out <- list("comparison" = out$comparison, "SampleID" = out$SampleID,
                "MP_B" = out$MP_B, "MP_A" = out$MP_A, "prediction" = out$prediction,
                "lwr" = out$lwr, "upr" = out$upr, "inside" = out$inside) |> setDT()

    if(!is.null(B)){
      out <- merge(out, inside_rates, by = c("comparison","SampleID"))
    }
    return(out)

  }

  # Checks if at least MP_B is present if not all MP_A, MP_B, SampleID are present
  else if(is_pb){
    new_data_list <- lapply(X = new_data_list, FUN = function(x) list("MP_B" = x$MP_B))
    out <- mapply(FUN = function(x, y, z, w) setDT(predict_eqa(data = x,
                                                               new_data = y,
                                                               imprecision_estimates = z,
                                                               R = w,
                                                               method = method,
                                                               level = level,
                                                               rounding = rounding)),
                  data_list, new_data_list, impr_data, R_cs_list, SIMPLIFY = FALSE) |>
      rbindlist(idcol = "comparison")
    out <- list("comparison" = out$comparison, "predictor" = out$MP_B, "prediction" = out$prediction, "lwr" = out$lwr, "upr" = out$upr) |>
      setDT()
    return(out)
  }
  else{
    stop("new_data is not on the correct form")
  }
}

