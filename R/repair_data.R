#' Repair a potentially broken commutability data set
#'
#' @param data A \code{list}, \code{data table} or \code{data frame} - data to be attempted repaired
#' @param data_check \code{list} - The output from \code{check_data()}. A repair should only be attempted if \code{check_data()} says \code{data} is perfect, acceptable or questionable. For the latter alternative there is no guarantee for possible repair
#' @param silence \code{integer} - Should progress reports be printed. \code{1} signify no, which is the default. \code{0} signify yes.
#' @return A \code{data table} that is the repaired (or unrepaired if not able to repair) data
#' @export
#'
#' @examples print(1)

repair_data <- function(data, data_check, silence = 1L){
  global_status <- data_check$for_human$`validity of input data is :`[1]
  if(global_status == "not acceptable"){
    warning("Data is not possible to repair. Input data is returned as output")
    return(data)
  }
  if(!is.data.table(data)){
    if(is.data.frame(data)){
      data <- as.data.table(data)
    }
    else if(is.list(data)){
      setDT(data)
    }
    else{
      warning("Data is not a list, data table or data frame. Tread carefully, dear user")
    }
  }

  id_cols <- c("SampleID", "ReplicateID")
  wb <- typo_suggestions()
  id_col_status <- data_check$for_computer_checks$valid_mandatory_id_columns
  nu_col_status <- data_check$for_computer_checks$valid_numeric_columns
  na_col_status <- data_check$for_computer_checks$valid_number_nas
  exclude_these_cols <- data_check$for_computer_information$exclude_these_numeric_columns
  exclude_these_rows <- data_check$for_computer_information$NA_indices_must_exclude
  if(id_col_status){
    allowable_typos_SampleID <- wb$SampleID
    allowable_typos_ReplicateID <- wb$ReplicateID
    id_cnds <- stri_replace_all(str = names(data), regex = "[^[:alpha:]]", replacement = "") |> tolower()
    id_SampleID <- which(id_cnds %in% allowable_typos_SampleID)[1]
    id_ReplicateID <- which(id_cnds %in% allowable_typos_ReplicateID)[1]
    if(any(length(id_SampleID) < 1 | length(id_ReplicateID) < 1)){
      warning("check_data says that data is repairable, but it is not! How?")
      return(data)
    }

    names(data)[id_SampleID] <- "SampleID"
    names(data)[id_ReplicateID] <- "ReplicateID"
    ss_dt_id <- data[,id_cols,with=FALSE]
    ss_dt_nu <- data[,-id_cols,with=FALSE]
    setcolorder(ss_dt_nu, order(names(ss_dt_nu)))
    data <- cbind(ss_dt_id, ss_dt_nu)
    if(silence == 0L){
      cat("ID column names were repaired and sorted alphabetically in numeric columns!", "\n", sep = "")
    }
  }
  data$SampleID <- as.character(data$SampleID)
  data$ReplicateID <- as.character(data$ReplicateID)
  if(!nu_col_status){
    id_invalid_nu_cols <- lapply(X = data[, -id_cols, with = FALSE], FUN = function(x) if(is.numeric(x)){FALSE}else{TRUE}) |>
      unlist() |> unname() |> which()
    data[, (id_invalid_nu_cols + 2)] <-
      lapply(data[, (id_invalid_nu_cols + 2), with = FALSE],
             FUN = function(x) ifelse(stri_detect(str = x, regex = "[^[:digit:]/.]"), NA, x)) |>
      lapply(as.numeric)
    if(silence == 0L){
      cat("Non-numeric cells are replaced by NA-values!", "\n", sep = "")
    }

  }
  if(!nu_col_status & na_col_status){
    re_fr_na_cols <- unlist(lapply(data[, -id_cols, with = FALSE], FUN = function(x) sum(is.na(x)) / length(x)))
    must_exclude_cols <- names(data[, -id_cols, with = FALSE])[which(re_fr_na_cols > 0.25)]
    more_na_cols <- FALSE
    if(!is.na(exclude_these_cols) & length(must_exclude_cols) > 0){
      all_exclude_cols <- union(must_exclude_cols, exclude_these_cols)
      more_na_cols <- !all(all_exclude_cols %in% exclude_these_cols)
      if(more_na_cols){
        exclude_these_cols <- all_exclude_cols
        if(silence == 0L){
          cat("After replacing non-numeric cells with NA-values, additional columns are invalid", "\n", sep = "")
        }
      }
    }
    if(length(must_exclude_cols) > 0 & is.na(exclude_these_cols)){
      exclude_these_cols <- must_exclude_cols
    }
  }
  exclude_rows <- !any(is.na(exclude_these_rows))
  exclude_cols <- !any(is.na(exclude_these_cols))
  exclude_some <- any(c(exclude_rows, exclude_cols))
  exclude_both <- all(c(exclude_rows, exclude_cols))
  more_na_than_expected <- sum(c(is.na(exclude_these_rows), is.na(exclude_these_cols))) > 2
  if(more_na_than_expected){
    warning("More than two NA values are found in check_data's output. This should not be possible")
  }

  if(exclude_both){
    out <- data[exclude_these_rows, -exclude_these_cols, with = FALSE]
    return(out)
  }
  else if(exclude_some){
    if(exclude_rows & !exclude_cols){
      out <- data[exclude_these_rows, ]
      return(out)
    }
    else if(!exclude_rows & exclude_cols){
      out <- data[, -exclude_these_cols, with = FALSE]
      return(out)
    }
  }
  return(data)
}

