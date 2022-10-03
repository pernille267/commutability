#' check data
#'
#' @param data \code{list} or \code{data table} that is the data that we wish to validate
#' @param silence \code{integer} - results be printed. Default is \code{1}, which signifies no
#' @param type \code{character} - Is the data clinical sample data or external quality assessment material data. Set \code{type = "cs"} (default) for the former and \code{type = "eqam"} for the latter
#' @return A \code{list} of validation results
#' @export
#'
#' @examples print(1)

check_data <- function(data, silence = 1, type = "cs"){

  numeric_column_names <- NULL

  if(!is.data.table(data)){
    if(is.data.frame(data)){
      data <- as.data.table(data)
    }
    else if(is.list(data)){
      setDT(data)
    }
  }

  data_contain <- names(data)
  expected_id_cols <- c("SampleID", "ReplicateID")

  id_cnds <- stri_replace_all(str = data_contain, regex = "[:space:]", replacement = "")
  id_cnds <- stri_replace_all(str = id_cnds, regex = "[:punct:]", replacement = "")
  id_cnds <- tolower(id_cnds)

  word_book <- typo_suggestions()
  valid_SampleIDs <- word_book$SampleID
  valid_ReplicateIDs <- word_book$ReplicateID
  SampleID_exists <- FALSE
  ReplicateID_exists <- FALSE
  SampleID_found <- any(id_cnds %in% valid_SampleIDs)
  ReplicateID_found <- any(id_cnds %in% valid_ReplicateIDs)
  valid_mandatory_id_columns <- TRUE
  valid_numeric_columns <- TRUE
  valid_number_nas <- TRUE

  if(SampleID_found){
    match_id <- which(id_cnds %in% valid_SampleIDs)[1]
    SampleID_exists <- TRUE
    names(data)[match_id] <- "SampleID"
  }
  if(ReplicateID_found){
    match_id <- which(id_cnds %in% valid_ReplicateIDs)[1]
    ReplicateID_exists <- TRUE
    names(data)[match_id] <- "ReplicateID"
  }

  if(!all(SampleID_found, ReplicateID_found)){
    valid_mandatory_id_columns <- FALSE
  }

  if(valid_mandatory_id_columns){

    numeric_column_names <- setdiff(names(data), expected_id_cols)
    numeric_column_ids <- which(names(data) %in% numeric_column_names)
    stripped_data <- copy(data)
    stripped_data <- stripped_data[ , names(data) %in% numeric_column_names, with = FALSE]
    column_wise_numeric <- unlist(lapply(X = as.list(stripped_data), FUN = is.numeric))
    if(!all(column_wise_numeric)){
      valid_numeric_columns <- FALSE
    }
    column_wise_number_nas <- unlist(lapply(X = as.list(stripped_data), FUN = function(x) sum(is.na(x))))
    column_wise_fraction_nas <- unlist(lapply(X = as.list(stripped_data), FUN = function(x) sum(is.na(x)) / length(x)))
    small_amount_nas <- which(column_wise_fraction_nas > 0 & column_wise_fraction_nas <= 0.05)
    moderate_amount_nas <- which(column_wise_fraction_nas > 0.05 & column_wise_fraction_nas <= 0.15)
    large_amount_nas <- which(column_wise_fraction_nas > 0.15 & column_wise_fraction_nas <= 0.25)
    insane_amount_nas <- which(column_wise_fraction_nas > 0.25)

    na_affected_numeric_columns <- NULL
    remaining_numeric_columns <- length(column_wise_numeric)
    valid_number_remaining_numeric_columns <- TRUE


    if(length(small_amount_nas) > 0){
      which_small_amount_nas <- names(stripped_data)[small_amount_nas]
    }
    if(length(moderate_amount_nas) > 0){
      which_moderate_amount_nas <- names(stripped_data)[moderate_amount_nas]
    }
    if(length(large_amount_nas) > 0){
      which_large_amount_nas <- names(data)[large_amount_nas]
      if(silence == 0){
        cat("--------------------- Note ------------------------------", "\n",
            "## Warning: Remove ", paste(names(stripped_data)[large_amount_nas], collapse = ", "), " to get more reliable results, " ,"\n",
            "## because between 15% and 25% of the relevant values were NA-values in this(these)", "\n",
            "--------------------- Note end--------------------------------", "\n", "\n" ,sep = "")
      }
    }
    if(length(insane_amount_nas) > 0){
      which_insane_amount_nas <- names(stripped_data)[insane_amount_nas]
      which_insane_amount_numeric <- which(names(stripped_data) %in% which_insane_amount_nas)
      na_affected_numeric_columns <- names(stripped_data)[which_insane_amount_numeric]
      if(length(na_affected_numeric_columns) > 0){
        remaining_numeric_columns <-  remaining_numeric_columns - length(na_affected_numeric_columns)
        if(remaining_numeric_columns < 2){
          valid_number_remaining_numeric_columns <- FALSE
        }
      }
      else{
        remaining_numeric_columns <-  remaining_numeric_columns
      }
      valid_number_nas <- FALSE
      if(silence == 0){
        cat("--------------------- Note ------------------------------", "\n",
            "## Serious warning: Remove ", paste(names(stripped_data)[insane_amount_nas], collapse = ", "), " to get valid or any results at all, " ,"\n",
            "## because more than 25% of the relevant values were NA-values in this(these) columns!", "\n",
            "--------------------- Note end--------------------------------", "\n", "\n", sep = "")
      }
    }
    column_wise_number_nas <- unlist(lapply(X = as.list(data), FUN = function(x) sum(is.na(x))))
    column_wise_fraction_nas <- unlist(lapply(X = as.list(data), FUN = function(x) sum(is.na(x)) / length(x)))

    na_id_SampleID <- which(is.na(data$SampleID))
    na_id_ReplicateID <- which(is.na(data$ReplicateID))
    na_fraction_SampleID <- length(na_id_SampleID) / length(data$SampleID)
    na_fraction_ReplicateID <- length(na_id_ReplicateID) / length(data$ReplicateID)
    number_unique_after_na_exclusion_SampleID <- length(unique(na.omit(data$SampleID)))
    number_unique_before_na_exclusion_SampleID <- length(unique(data$SampleID))

    acceptable_number_nas_SampleID <- TRUE
    acceptable_number_nas_ReplicateID <- TRUE
    perfect_number_nas_SampleID <- TRUE
    perfect_number_nas_ReplicateID <- TRUE

    if(na_fraction_SampleID > 0.25 & type == "cs"){
      perfect_number_nas_SampleID <- FALSE
      if(silence == 0){
        cat("----------- Note -----------------", "\n",
            "## More than 25% of the SampleID values are NA-values!", "\n", "\n",
            "----------------------------------", sep = "")
      }
    }
    if(number_unique_after_na_exclusion_SampleID < 20 & type == "cs"){
      perfect_number_nas_SampleID <- FALSE
      if(number_unique_after_na_exclusion_SampleID < 15){
        acceptable_number_nas_SampleID <- TRUE
      }
      if(silence == 0){
        cat("----------- Note -----------------", "\n",
            "## The number of NA-values in SampleID results in inappopriate study design!", "\n",
            "## number of unique SampleIDs: ", number_unique_before_na_exclusion_SampleID, "\n",
            "## Minimum study design requires number of unique SampleIDs > 20 for CS data, which is not the case here", "\n",
            "----------------------------------", "\n", "\n", sep = "")
      }
    }
    if(number_unique_after_na_exclusion_SampleID < 1 & type == "eqam"){
      perfect_number_nas_SampleID <- FALSE
      acceptable_number_nas_SampleID <- FALSE
      if(silence == 0){
        cat("----------- Note -----------------", "\n",
            "## The number of NA-values in SampleID results in inappopriate study design!", "\n",
            "## number of unique SampleIDs after exclusion of NA-values : ", "none", "\n",
            "## Minimum study design requires number of unique SampleIDs > 1 for EQAM data, which is not the case here", "\n",
            "----------------------------------", "\n", "\n", sep = "")
      }

    }

    if(na_fraction_ReplicateID > 0.25 & type == "cs"){
      perfect_number_nas_ReplicateID <- FALSE
      if(silence == 0){
        cat("----------- Note -----------------", "\n",
            "## More than 25% of the ReplicateID values are NA-values!", "\n", "\n",
            "----------------------------------", sep = "")
      }
    }

    id_cols_na_exclusions <- sort(union(na_id_SampleID, na_id_ReplicateID))
    na_fraction_id_cols <- length(id_cols_na_exclusions) / length(data$SampleID)
    id_na_nu_cols <- lapply(stripped_data, FUN = function(x) which(is.na(x)))
    joint_id_na_nu_cols <- lapply(id_na_nu_cols, FUN = union, id_cols_na_exclusions)
    effe_samp_size <- lapply(joint_id_na_nu_cols, FUN = function(x) if(length(x)>0){length(unique(data[-x, ]$SampleID))}else{length(unique(data$SampleID))})

    if(na_fraction_id_cols > 0.05 & type == "cs"){
      if(na_fraction_id_cols > 0.25){
        perfect_number_nas_ReplicateID <- FALSE
        perfect_number_nas_SampleID <- FALSE
        if(silence == 0){
          cat("----------- Note -----------------", "\n",
              "## More than 25% of the combined ReplicateID values and SampleID values are NA-values!", "\n", "\n",
              "----------------------------------", "\n", "\n", sep = "")
        }

      }
      number_unique_after_na_exclusion_SampleID_and_ReplicateID <- length(unique(data[-id_cols_na_exclusions,]$SampleID))
      if(number_unique_after_na_exclusion_SampleID_and_ReplicateID < 20){
        perfect_number_nas_ReplicateID <- FALSE
        perfect_number_nas_SampleID <- FALSE
        if(number_unique_after_na_exclusion_SampleID_and_ReplicateID < 15){
          acceptable_number_nas_ReplicateID <- FALSE
          acceptable_number_nas_SampleID <- FALSE
        }
        if(silence == 0){
          cat("----------- Note -----------------", "\n",
              "## The number of NA-values in SampleID and ReplicateID, together, results in serious data-loss!", "\n",
              "## Number of unique SampleIDs after exclusion of NA-values : ", number_unique_after_na_exclusion_SampleID_and_ReplicateID, "\n",
              "## Minimum study design requires number of unique SampleIDs > 20 for CS data, which is not the case here", "\n",
              "----------------------------------", "\n", "\n", sep = "")
        }

      }

    }

    if(na_fraction_id_cols > 0 & type == "eqam"){
      if(na_fraction_id_cols > 0.25){
        if(silence == 0){
          cat("----------- Note -----------------", "\n",
              "## More than 25% of the combined ReplicateID values and SampleID values are NA-values!", "\n", "\n",
              "----------------------------------", "\n", "\n", sep = "")
        }

      }
      number_unique_after_na_exclusion_SampleID_and_ReplicateID <- length(unique(data[-id_cols_na_exclusions,]$SampleID))
      if(number_unique_after_na_exclusion_SampleID_and_ReplicateID < 1){
        acceptable_number_nas_ReplicateID <- FALSE
        acceptable_number_nas_SampleID <- FALSE
        if(silence == 0){
          cat("----------- Note -----------------", "\n",
              "## The number of NA-values in SampleID and ReplicateID, together, results in serious data-loss!", "\n",
              "## Number of unique SampleIDs before exclusion of NA-values : ", number_unique_before_na_exclusion_SampleID, "\n",
              "## Number of unique SampleIDs after exclusion of NA-values : ", number_unique_after_na_exclusion_SampleID_and_ReplicateID, "\n",
              "## Minimum study design requires number of unique SampleIDs > 1 for EQAM data, which is not the case here", "\n",
              "----------------------------------", "\n", "\n", sep = "")
        }
      }
    }

    for_computer_checks <- list("valid_mandatory_id_columns" = valid_mandatory_id_columns,
                                "valid_numeric_columns" = valid_numeric_columns,
                                "valid_number_nas" = valid_number_nas,
                                "valid_number_remaining_numeric" = valid_number_remaining_numeric_columns,
                                "perfect_number_nas_SampleID" = perfect_number_nas_SampleID,
                                "perfect_number_nas_ReplicateID" = perfect_number_nas_ReplicateID,
                                "acceptable_number_nas_SampleID" = acceptable_number_nas_SampleID,
                                "acceptable_number_nas_ReplicateID" = acceptable_number_nas_ReplicateID)

    for_computer_information <- list("exclude_these_numeric_columns" = if(!valid_number_nas & valid_number_remaining_numeric_columns){na_affected_numeric_columns}else{NA},
                                     "NA_indices_of_SampleID" = if(length(na_id_SampleID) > 0){na_id_SampleID}else{NA},
                                     "NA_indices_of_ReplicateID" = if(length(na_id_ReplicateID) > 0){na_id_ReplicateID}else{NA},
                                     "NA_indices_must_exclude" = if(length(id_cols_na_exclusions) > 0){id_cols_na_exclusions}else{NA})

    minimal_data <- all(valid_mandatory_id_columns, valid_number_remaining_numeric_columns,
                        acceptable_number_nas_ReplicateID, acceptable_number_nas_SampleID)

    if(!valid_numeric_columns | !valid_number_nas){
      questionable_data <- minimal_data
    }

    acceptable_data <- all(valid_mandatory_id_columns, valid_numeric_columns,
                           valid_number_nas, valid_number_remaining_numeric_columns,
                           acceptable_number_nas_SampleID, acceptable_number_nas_ReplicateID)

    perfect_data <- all(valid_mandatory_id_columns, valid_numeric_columns,
                        valid_number_nas, valid_number_remaining_numeric_columns,
                        perfect_number_nas_SampleID, perfect_number_nas_ReplicateID)

    for_human <- list("validity of input data is :" = if(perfect_data){"perfect"}else if(acceptable_data){"acceptable"}else if(questionable_data){"questionable"}else{"not acceptable"},
                      "column name" = names(column_wise_fraction_nas),
                      "number NAs" = unname(column_wise_number_nas),
                      "fraction NAs" = unname(round(column_wise_fraction_nas, 3)),
                      "effective unique CSs" = unname(unlist(effe_samp_size)))

    for_human$`validity of input data is :` <- c(for_human$`validity of input data is :`,
                                              rep(NA, length(column_wise_fraction_nas) - 1))
    for_human$`effective unique CSs` <- c(rep(NA, 2), for_human$`effective unique CSs`)

    out <- list("for_computer_checks" = for_computer_checks,
                "for_computer_information" = for_computer_information,
                "for_human" = setDT(for_human))

    return(out)
  }
  case <- if(!SampleID_found){"SampleID"}else{""}
  case <- if(!ReplicateID_found){"ReplicateID"}else{case}
  case <- if(all(!SampleID_found, !ReplicateID_found)){"SampleID and ReplicateID"}else{case}

  if(silence == 0){
    cat("----------- Information ------------------------------", "\n",
        "## ", case,  ", or any close matches were not found in data", "\n",
        "------------------------------------------------------", "\n",
        "\n",
        "----- Your data contained the following columns: -----", "\n",
        "## ", paste(names(data), collapse = ", "), ", ","\n",
        "## and here we were not able to locate ", case , "\n",
        "------------------------------------------------------", "\n", sep = "")
  }


  for_computer_checks <- list("valid_mandatory_id_columns" = valid_mandatory_id_columns,
                              "valid_numeric_columns" = NA,
                              "valid_number_nas" = NA,
                              "valid_number_remaining_numeric" = NA,
                              "perfect_number_nas_SampleID" = NA,
                              "perfect_number_nas_ReplicateID" = NA,
                              "acceptable_number_nas_SampleID" = NA,
                              "acceptable_number_nas_ReplicateID" = NA)

  for_computer_information <- list("exclude_these_numeric_columns" = NA,
                                   "NA_indices_of_SampleID" = NA,
                                   "NA_indices_of_ReplicateID" = NA,
                                   "NA_indices_must_exclude" = NA)

  for_human <- list("validity of input data is :" = "not acceptable",
                    "column name" = NA,
                    "number NAs" = NA,
                    "fraction NAs" = NA,
                    "effective unique CSs" = NA)

  out <- list("for_computer_checks" = for_computer_checks,
              "for_computer_information" = for_computer_information,
              "for_human" = setDT(for_human))

  return(out)

}


