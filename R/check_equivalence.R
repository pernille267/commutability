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
      cat("Clinical sample data do not have the required id columns.", "\n", sep = "")
    }
  }
  if(!all(id_columns %in% names(eq_data))){
    id_columns_exist_eq <- FALSE
    if(silence < 1L){
      cat("External quality assessment material data do not have the required id columns.", "\n", sep = "")
    }
  }
  if(!all(nu_columns_cs %in% nu_columns_eq)){
    equivalent_column_names <- FALSE
    if(silence < 1L){
      cat("Clinical sample data and external quality assessment material data does not have the same column names.", "\n", sep = "")
    }
  }
  if(any(order(nu_columns_cs) != order(nu_columns_eq))){
    equivalent_column_order <- FALSE
    if(silence < 1L){
      cat("Clinical sample data and external quality assessment material data does not have the same column name order.", "\n", sep = "")
    }
  }
  return(list("equivalent_id_columns" = all(id_columns_exist_cs, id_columns_exist_eq),
              "equivalent_numeric_column_names" = equivalent_column_names,
              "equivalent_numeric_column_order" = equivalent_column_order))
}
