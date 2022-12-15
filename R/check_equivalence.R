#' Checking for equivalent forms of clinical sample data and external quality assessment material data
#'
#' @param cs_data data table, data frame or list - dataset containing clinical samples' measurements.
#' @param eq_data data table, data frame or list - dataset containing external quality assessment material samples' measurements
#' @param silence integer - Should verbose be allowed? Additional potentially helpful text are printed if silence is set to smaller than 1L
#'
#' @return A list with three elements with information regarding equivalence status
#' @export
#'
#' @examples print(1)
check_equivalence <- function(cs_data, eq_data, silence = 1L){

  if(!is.data.table(cs_data)){
    if(is.data.frame(cs_data)){
      cs_data <- as.data.table(cs_data)
    }
    else if(is.list(cs_data)){
      setDT(cs_data)
    }
    else{
      warning("cs_data was not a data table, data frame or list, but rather '", class(cs_data), "'.", "\n",
              "equivalence is therefore not possible to check!")
      return(list("equivalent_id_columns" = FALSE,
                  "equivalent_numeric_column_names" = FALSE,
                  "equivalent_numeric_column_order" = FALSE))
    }
  }

  if(!is.data.table(eq_data)){
    if(is.data.frame(eq_data)){
      eq_data <- as.data.table(eq_data)
    }
    else if(is.list(eq_data)){
      setDT(eq_data)
    }
    else{
      warning("eq_data was not a data table, data frame or list, but rather '", class(eq_data), "'.", "\n",
              "equivalence is therefore not possible to check!")
      return(list("equivalent_id_columns" = FALSE,
                  "equivalent_numeric_column_names" = FALSE,
                  "equivalent_numeric_column_order" = FALSE))
    }
  }

  id_columns <- c("SampleID", "ReplicateID")
  nu_columns_cs <- setdiff(names(cs_data), id_columns)
  nu_columns_eq <- setdiff(names(eq_data), id_columns)
  id_columns_exist_cs <- TRUE
  id_columns_exist_eq <- TRUE
  equivalent_column_order <- TRUE
  equivalent_column_names <- TRUE

  if(!all(id_columns %in% names(cs_data))){
    id_columns_exist_cs <- FALSE
    if(silence < 1L){
      message("Clinical sample data do not have the required id columns.", "\n", sep = "")
    }
    return(list("equivalent_id_columns" = FALSE,
                "equivalent_numeric_column_names" = FALSE,
                "equivalent_numeric_column_order" = FALSE))
  }
  if(!all(id_columns %in% names(eq_data))){
    id_columns_exist_eq <- FALSE
    if(silence < 1L){
      message("External quality assessment material data do not have the required id columns.", "\n", sep = "")
    }
    return(list("equivalent_id_columns" = FALSE,
                "equivalent_numeric_column_names" = FALSE,
                "equivalent_numeric_column_order" = FALSE))
  }
  if(isFALSE(length(nu_columns_cs) == length(nu_columns_eq))){
    if(length(nu_columns_cs) > length(nu_columns_eq)){
      equivalent_column_names <- all(nu_columns_cs %in% nu_columns_eq)
      if(silence < 1L){
        message("The clinical sample data have more columns than the EQAM data. Indeed, [", nu_columns_cs[which(!nu_columns_cs %in% nu_columns_eq)], "]", "\n",
            "is part of the clinical sample data, but not part of the EQAM data!", sep = "")
      }
      nu_columns_cs <- nu_columns_cs[which(nu_columns_cs %in% nu_columns_eq)]

    }
    else if(length(nu_columns_cs) < length(nu_columns_eq)){
      equivalent_column_names <- all(nu_columns_eq %in% nu_columns_cs)
      if(silence < 1L){
        message("The EQAM data have more columns than the clinical sample data. Indeed, [", nu_columns_eq[which(!nu_columns_eq %in% nu_columns_cs)], "]", "\n",
            "is part of the EQAM data, but not part of the clinical sample data!", sep = "")
      }
      nu_columns_eq <- nu_columns_eq[which(nu_columns_eq %in% nu_columns_cs)]
    }
    else{
      return(list("equivalent_id_columns" = all(id_columns_exist_cs, id_columns_exist_eq),
                  "equivalent_numeric_column_names" = FALSE,
                  "equivalent_numeric_column_order" = FALSE))
    }
  }

  if(any(order(nu_columns_cs) != order(nu_columns_eq))){
    equivalent_column_order <- FALSE
    if(silence < 1L){
      message("Clinical sample data and external quality assessment material data does not have the same column name order.", "\n", sep = "")
    }
  }
  return(list("equivalent_id_columns" = all(id_columns_exist_cs, id_columns_exist_eq),
              "equivalent_numeric_column_names" = equivalent_column_names,
              "equivalent_numeric_column_order" = equivalent_column_order))
}
