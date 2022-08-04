#' Get MS-wise imprecison estimates
#'
#' @param imprecision_data \code{list}, \code{data table} or \code{data frame} - Imprecision estimates including bootstrap confidence intervals for CV data, variance data, or both. Must include a grouping column named \code{comparison}.
#' @param mode character - How should the output be expressed. There are two possibilities:
#' \itemize{
#'   \item{\code{visual}: }{Creates presentable MS-wise imprecision estimates, that neatly combines MS-wise imprecision estimates into a maximum of three columns. The first column is of course \code{MS}, and the remaining potential two is all imprecision estimates for CVs and SDs}
#'   \item{\code{exact}: }{Creates MS-wise imprecision estimates that could be used for further calculations if needed. Includes a minimum of four columns (and a maxium of 7), where the latter six rows are the imprecision estimates for each MS}
#' }
#' @return \code{data table} with MS-wise imprecision estimates, that include point estimates and bootstrap confidence intervals
#' @export
#'
#' @examples print(1)

MS_wise_imprecision <- function(imprecision_data, mode = c("visual", "exact")){

  CV <- CV_lwr <- CV_upr <- MS <- SD <- SD_lwr <- SD_upr <- NULL
  if(all(mode == c("visual", "exact"))){
    mode <- mode[1]
  }
  else if(length(mode) > 1){
    warning("mode should only be a singly character. The first element of the passed mode is used")
    mode <- mode[1]
  }
  if(!is.data.table(imprecision_data)){
    if(is.data.frame(imprecision_data) | is.list(imprecision_data)){
      setDT(imprecision_data)
    }
    else{
      stop(paste0("Expected list, data table, or data frame for imprecision_data. Got ","'",class(imprecision_data),"'", ". Calculations are terminated"))
    }
  }

  cvs_exist <- all(c("CV_A","CV_A_lwr","CV_A_upr", "CV_B","CV_B_lwr","CV_B_upr") %in% names(imprecision_data))
  var_exist <- all(c("Var_A","Var_A_lwr","Var_A_upr", "Var_B","Var_B_lwr","Var_B_upr") %in% names(imprecision_data))
  com_exist <- any("comparison" == names(imprecision_data))

  if(!any(cvs_exist, var_exist)){
    stop("None of CV data and variance data was not found in imprecision_data. Calculations are terminated")
  }

  if(com_exist){

    if(cvs_exist){
      cv_data <- imprecision_data[,c("comparison","CV_A","CV_A_lwr","CV_A_upr", "CV_B","CV_B_lwr","CV_B_upr")] |>
        lapply(FUN = function(x) if(is.numeric(x)){x * 100}else{x}) |> setDT()
      cv_last_row <- cv_data[nrow(cv_data),] |>
        setnames(old = c("CV_A","CV_A_lwr","CV_A_upr", "CV_B","CV_B_lwr","CV_B_upr"),
                 new = c("CV_B","CV_B_lwr","CV_B_upr", "CV_A","CV_A_lwr","CV_A_upr"),
                 skip_absent = TRUE) |>
        setcolorder(c("comparison","CV_A","CV_A_lwr","CV_A_upr", "CV_B", "CV_B_lwr", "CV_B_upr"))
      cv_last_row$comparison <- stri_split(cv_last_row$comparison, fixed = " - ") |>
        unlist() |> rev() |> stri_c(collapse = " - ")
      cv_data <- rbind(cv_data, cv_last_row)[,-c("CV_B", "CV_B_lwr", "CV_B_upr")]
      cv_data$comparison <- lapply(X = stri_split(cv_data$comparison, fixed = " - "), FUN = function(x) x[1]) |> unlist()
      cv_data <- lapply(X = split(cv_data, by = "comparison", keep.by = FALSE), FUN = function(x) list("CV" = mean(x$CV_A), "CV_lwr" = mean(x$CV_A_lwr), "CV_upr" = mean(x$CV_A_upr)) |>
                          setDT()) |> rbindlist(idcol = "MS")
      if(mode == "visual"){
        cv_data <- cv_data[,list(`%CV (lwr, upr)` = paste0(if(is.na(CV)){warning(paste0("CV for ",MS," is a NA value")); "     "}else if(CV >= 1e-2){round(CV, 2L)}else{"< 0.01"}," (",
                                                           if(is.na(CV_lwr)){warning(paste0("CV_lwr for ",MS," is a NA value"));"     "}else if(CV_lwr >= 1e-2){round(CV_lwr, 2L)}else{"< 0.01"},", ",
                                                           if(is.na(CV_upr)){warning(paste0("CV_upr for ",MS," is a NA value"));"     "}else if(CV_upr >= 1e-2){round(CV_upr, 2L)}else{"< 0.01"},")")),
                           by = list(MS)]
      }

      if(!var_exist){
        return(cv_data)
      }
    }

    if(var_exist){
      sd_data <- imprecision_data[, c("comparison","Var_A","Var_A_lwr","Var_A_upr", "Var_B","Var_B_lwr","Var_B_upr")] |>
        lapply(FUN = function(x) if(is.numeric(x)){sqrt(x)}else{x}) |> setDT() |>
        setnames(old = c("Var_A","Var_A_lwr","Var_A_upr", "Var_B","Var_B_lwr","Var_B_upr"),
                 new = c("SD_A","SD_A_lwr","SD_A_upr", "SD_B","SD_B_lwr","SD_B_upr"),
                 skip_absent = TRUE)

      sd_last_row <- sd_data[nrow(sd_data),] |> setnames(old = c("SD_A","SD_A_lwr","SD_A_upr", "SD_B","SD_B_lwr","SD_B_upr"),
                                                         new = c("SD_B","SD_B_lwr","SD_B_upr", "SD_A","SD_A_lwr","SD_A_upr"),
                                                         skip_absent = TRUE) |>
        setcolorder(c("comparison","SD_A","SD_A_lwr","SD_A_upr", "SD_B", "SD_B_lwr", "SD_B_upr"))

      sd_last_row$comparison <- stri_split(sd_last_row$comparison, fixed = " - ") |> unlist() |> rev() |> stri_c(collapse = " - ")

      sd_data <- rbind(sd_data, sd_last_row)[,-c("SD_B", "SD_B_lwr", "SD_B_upr")]
      sd_data$comparison <- lapply(X = stri_split(sd_data$comparison, fixed = " - "), FUN = function(x) x[1]) |> unlist()
      sd_data <- lapply(X = split(sd_data, by = "comparison", keep.by = FALSE), FUN = function(x) list("SD" = mean(x$SD_A), "SD_lwr" = mean(x$SD_A_lwr), "SD_upr" = mean(x$SD_A_upr)) |>
                          setDT()) |> rbindlist(idcol = "MS")
      if(mode == "visual"){
        sd_data <- sd_data[, list(`SD (lwr, upr)` = paste0(if(is.na(SD)){warning(paste0("SD for ",MS," is a NA value")); "     "}else if(SD >= 1e-3){round(SD, 3L)}else{"< 0.001"}," (",
                                                           if(is.na(SD_lwr)){warning(paste0("SD_lwr for ",MS," is a NA value"));"     "}else if(SD_lwr >= 1e-3){round(SD_lwr, 3L)}else{"< 0.001"},", ",
                                                           if(is.na(SD_upr)){warning(paste0("SD_upr for ",MS," is a NA value"));"     "}else if(SD_upr >= 1e-3){round(SD_upr, 3L)}else{"< 0.001"},")")),
                           by = list(MS)]
      }

      if(!cvs_exist){
        return(sd_data)
      }
    }

    out <- merge(cv_data, sd_data, by = "MS")
    return(out)
  }
  else{
    stop("comparison was not found in imprecision_data. Calculations are terminated")
  }
}
