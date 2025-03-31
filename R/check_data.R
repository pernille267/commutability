#' Convert \code{data} to \code{data.table} Format
#'
#' @description
#' This function attempts to convert the input \code{data} to a
#' \code{data.table} format. It handles \code{data.frame} and \code{list}
#' inputs, converting them to \code{data.table} if possible.
#'
#' @param data A \code{data.table}, \code{list}, or \code{data.frame} to be
#'             converted.
#'
#' @return
#' A \code{list} containing:
#' \itemize{
#'  \item \code{data: } The converted \code{data.table} object, or the original
#'        input if conversion failed.
#'  \item \code{can_convert_data: } A \code{logical} value. Indicates whether
#'        the conversion was successful.
#' }
#' @keywords internal
convert_data_cd <- function(data){

  # Avoid modifying original input
  data <- copy(data)

  # Can convert indicator
  can_convert_data <- FALSE

  # Convert data iff possible.
  if (!is.data.table(data)) {
    if (is.data.frame(data)) {
      data <- as.data.table(data)
      can_convert_data <- TRUE
    }
    else if (is.list(data)) {
      setDT(data)
      can_convert_data <- TRUE
    }
  }
  else {
    can_convert_data <- TRUE
  }

  return(list(data = data,
              can_convert_data = can_convert_data))

}

#' Check for Mandatory ID Columns in \code{data}
#'
#' @description
#' This function checks for the presence of mandatory ID columns
#' (\code{SampleID} and \code{ReplicateID}) in the input \code{data}. It
#' attempts to identify these columns even if they have slight naming
#' variations.
#'
#' @param data A \code{data.table} to be checked.
#'
#' @return
#' A \code{list} containing:
#' \itemize{
#'  \item \code{pass: } A \code{logical} value. Indicates whether both
#'        mandatory ID columns were found.
#'  \item \code{data: } The input \code{data} with standardized column names
#'        for \code{SampleID} and \code{ReplicateID}, if found.
#' }
#' @keywords internal
check_mandatory_id_columns <- function(data){

  # Avoid modifying original data
  data <- copy(data)

  # Acceptable ID column typos
  acceptable_ID_column_typo <- typo_suggestions()
  acceptable_SampleID_typos <- unique(acceptable_ID_column_typo$SampleID)
  acceptable_ReplicateID_typos <- unique(acceptable_ID_column_typo$ReplicateID)

  # Get names of 'data'
  data_names <- names(data)

  # Remove all empty spaces, punctionations and use lower case of data_names
  ID_column_candidates <- stri_replace_all(
    str = data_names,
    regex = "[:space:]",
    replacement = ""
  )
  ID_column_candidates <- stri_replace_all(
    str = ID_column_candidates,
    regex = "[:punct:]",
    replacement = ""
  )
  ID_column_candidates <- tolower(
    x = ID_column_candidates
  )


  # Search among the acceptable typos
  SampleID_exists <- any(ID_column_candidates %in% acceptable_SampleID_typos)
  ReplicateID_exists <- any(ID_column_candidates %in% acceptable_ReplicateID_typos)

  if(SampleID_exists){
    match_id <- which(ID_column_candidates %in% acceptable_SampleID_typos)[1]
    names(data)[match_id] <- "SampleID"
  }
  if(ReplicateID_exists){
    match_id <- which(ID_column_candidates %in% acceptable_ReplicateID_typos)[1]
    names(data)[match_id] <- "ReplicateID"
  }

  return(list(pass = SampleID_exists && ReplicateID_exists,
              data = data))


}

#' Check Validity of Numeric Columns in \code{data}
#'
#' @description
#' This function assesses the \code{numeric} columns in the input \code{data},
#' checking for their validity and potential for conversion to \code{numeric}
#' format if they are not already.
#'
#' @param data A \code{data.table} containing the data to be checked.
#'
#' A \code{list} containing:
#' \itemize{
#'  \item \code{pass: } A \code{logical} value. Indicates whether the
#'        \code{numeric} columns meet the validity criteria.
#'  \item \code{numeric_columns: } A \code{character} vector of column names
#'        identified as \code{numeric}.
#'  \item \code{convert_these_numeric_columns: } A \code{character} vector of
#'        column names that should be converted to numeric.
#' }
#'
#' @details
#' The function considers a column "numeric enough" if at least 90% of its
#' non-NA values can be converted to \code{numeric}.
#'
#' @keywords internal
check_numeric_columns <- function(data){

  # Avoid modifying original data
  data <- copy(data)

  # Number of elements not recongnized as numbers
  requirement_1 <- 0.10

  # Check if numeric enough
  numeric_enough <- function(x, tol = 0.90) {

    should_convert <- FALSE

    # Check if not numeric
    if (!is.numeric(x)) {
      if (is.character(x)) {
        cannot_be_converted_to_numeric <- stri_detect_regex(
          str = x,
          pattern = "[^0-9.]"
        )

        # Get those elements that can be converted to numeric
        can_be_converted_to_numeric <- !cannot_be_converted_to_numeric
        x_convertable <- x[can_be_converted_to_numeric]

        # Attempt to convert those only containing digits and '.'
        x_as_numeric <- tryCatch(
          expr = {
            as.numeric(x_convertable)
          },
          error = function(e) NULL,
          warning = function(w) NULL
        )

        # x_as_numeric should run without warning / error
        if (!is.null(x_as_numeric)) {
          fraction_valid_numbers <- length(x_as_numeric) / length(x)

          # Check if fraction of valid numeric is unacceptable
          if (fraction_valid_numbers < tol) {
            should_convert <- FALSE
            return(list(
              is_numeric = FALSE,
              should_convert = should_convert
            ))
          }
          # Fraction of valid numeric is acceptable
          should_convert <- TRUE
          return(list(
            is_numeric = FALSE,
            should_convert = should_convert
          ))
        }
        # x_as_numeric results in warning / error
        should_convert <- FALSE
        return(list(
          is_numeric = FALSE,
          should_convert = should_convert
        ))

      }
      # If not numeric or character:
      should_convert <- FALSE
      return(list(
        is_numeric = FALSE,
        should_convert = should_convert
      ))
    }

    # If numeric:
    return(list(
      is_numeric = TRUE,
      should_convert = FALSE
    ))
  }

  # Get numeric columns from data
  numeric_columns <- data[, -c("SampleID", "ReplicateID")]

  # Check number of numeric columns
  valid_number_of_numeric_columns <- ncol(numeric_columns) >= 2

  # Columns that can and should be converted to numeric

  # Check if numeric columns are numeric
  are_numeric_columns_actually_numeric <- sapply(
    X = numeric_columns,
    FUN = function(numeric_col) {
      numeric_enough(numeric_col, 1 - requirement_1)$is_numeric
    },
    simplify = TRUE
  )

  # Check if non-numeric columns should be converted to numeric
  can_nonnumeric_columns_be_converted <- sapply(
    X = numeric_columns,
    FUN = function(numeric_col) {
      numeric_enough(numeric_col, 1 - requirement_1)$should_convert
    },
    simplify = TRUE
  )

  # Check if any of the numeric columns should be attempted converted
  if (sum(can_nonnumeric_columns_be_converted) > 0) {
    convert_these_numeric_columns <- names(numeric_columns)[can_nonnumeric_columns_be_converted]
  }
  else {
    convert_these_numeric_columns <- NULL
  }

  # Check if all numeric columns are numeric or can be converted to numeric
  numeric_columns_are_numeric_or_can_be_repaired <- all(
    are_numeric_columns_actually_numeric | can_nonnumeric_columns_be_converted
  )

  return(list(
    "pass" = valid_number_of_numeric_columns &&
      numeric_columns_are_numeric_or_can_be_repaired,
    "numeric_columns" = names(numeric_columns),
    "convert_these_numeric_columns" = convert_these_numeric_columns
  ))

}

#' Check Validity of Number of Missing Values in \code{data}
#'
#' @description
#' This function assesses the number of missing data points in the input
#' \code{data}, calculating various metrics related to sample size and data
#' completeness.
#'
#' @param data A \code{data.table} containing the data to be checked.
#' @param type A \code{character} string. Either \code{'cs'} for clinical
#'             samples or \code{'eqam'} for evaluated materials.
#'
#' @return
#' A \code{list} containing:
#' \itemize{
#'  \item \code{pass: } A \code{logical} indicating if the data passes the
#'        \code{NA} fraction check.
#'  \item \code{number_of_NAs_numeric_data: } Number of NAs for each
#'        \code{numeric} column.
#'  \item \code{fraction_of_NAs_numeric_data: } Fraction of NAs for each
#'        \code{numeric} column.
#'  \item \code{amount_of_NAs_numeric_data: } Categorical classification of NA
#'        amount.
#'  \item \code{effective_numbers_of_samples: } Number of non-NA samples for
#'        each column.
#'  \item \code{average_number_of_replicates: } Average number of replicates
#'        per sample.
#'  \item \code{influence_number_of_samples: } Relative influence of each
#'        column's sample size.
#'  \item \code{minimum_number_of_samples: } Minimum effective number of
#'        samples across columns.
#'  \item \code{remove_these_methods: } Columns that should be removed due to
#'        low quality.
#'  \item \code{keep_these_ID_column_ids: } IDs of rows with valid SampleID and
#'        ReplicateID
#'  \item \code{keep_these_numeric_column_ids: } IDs of rows with valid numeric
#'        data after excluding invalid SampleID and ReplicateID rows.
#' }
#'
#' @keywords internal
check_na_fraction <- function(data, type = "cs"){

  # Avoid modifying original data
  data <- copy(data)

  # Minimum requirement for minimum effective number of samples after NA omit
  minimum_requirement_1 <- if(type == "cs") {15} else {1}
  # Minimum requirement for average number of replicates after NA omit
  minimum_requirement_2 <- 3/2
  # Minimum requirement for difference between
  minimum_requirement_3 <- 3/4

  # Get numeric columns
  numeric_data <- data[, -c("SampleID", "ReplicateID")]

  # Calculate number of NA values for each IVD-MD
  number_of_NAs_numeric_data <- numeric_data[, lapply(
    X = .SD,
    FUN = function(numeric_col) {
      sum(is.na(numeric_col))
    }
    )]

  # Calculate fraction of NA values for each IVD-MD
  fraction_NAs_numeric_data <- numeric_data[, lapply(
    X = .SD,
    FUN = function(numeric_col) {
      sum(is.na(numeric_col)) / length(numeric_col)
    }
    )]

  # Classify fraction of NA values for each IVD-MD
  amount_NAs_numeric_data <- fraction_NAs_numeric_data[, lapply(
    X = .SD,
    FUN = function(fraction) {
      if(is.na(fraction)){
        NA_character_
      }
      else if(fraction >= 0 & fraction <= 0.05) {
        "negligible"
      }
      else if(fraction > 0.05 & fraction <= 0.10) {
        "small"
      }
      else if(fraction > 0.10 & fraction <= 0.25) {
        "moderate"
      }
      else {
        "large"
      }
    }
    )]

  # Remove NA values in SampleID and ReplicateID
  valid_ids_SampleID <- which(!is.na(data$SampleID))
  valid_ids_ReplicateID <- which(!is.na(data$ReplicateID))
  valid_ids_ID_columns <- intersect(x = valid_ids_SampleID,
                                    y = valid_ids_ReplicateID)

  # All non-NA SampleIDs and ReplicateIDs
  all_SampleIDs <- data$SampleID[valid_ids_ID_columns]
  all_ReplicateIDs <- data$ReplicateID[valid_ids_ID_columns]

  # Calculate NA indicies for each IVD-MD
  MS_wise_na_ids  <- lapply(
    X = numeric_data,
    FUN = function(numeric_col) {
      which(!is.na(numeric_col[valid_ids_ID_columns]))
    }
  )

  # Calculate effective number of samples for each IVD-MD
  effective_numbers_of_samples <- sapply(
    X = seq_len(ncol(numeric_data)),
    FUN = function(numeric_col) {
      non_na_ids <- MS_wise_na_ids[[numeric_col]]
      valid_SampleIDs <- all_SampleIDs[non_na_ids]
      length(unique(valid_SampleIDs))
    },
    simplify = TRUE
  )

  # Calculate effective number of replicates for each IVD-MD
  average_number_of_replicates <- sapply(
    X = seq_len(ncol(numeric_data)),
    FUN = function(numeric_col) {
      non_na_ids <- MS_wise_na_ids[[numeric_col]]
      valid_SampleIDs <- all_SampleIDs[non_na_ids]
      number_of_replicates <- unname(
        tapply(
          X = valid_SampleIDs,
          FUN = length,
          INDEX = valid_SampleIDs,
          simplify = TRUE
        )
      )
      mean(number_of_replicates, na.rm = TRUE)
   },
   simplify = TRUE
  )

  # Calculate influence for each IVD-MD
  influence_number_of_samples <- effective_numbers_of_samples /
    median(effective_numbers_of_samples, na.rm = TRUE)
  if (length(influence_number_of_samples) == 2) {
    influence_number_of_samples <- effective_numbers_of_samples /
      max(effective_numbers_of_samples, na.rm = TRUE)
  }

  # Calculate minimum number of effective samples
  minimum_number_of_samples <- min(effective_numbers_of_samples, na.rm = TRUE)

  # Check if any of the IVD-MDs should be removed to ensure quality
  remove_these_methods <- names(numeric_data)[which(
    effective_numbers_of_samples < minimum_requirement_1 |
      average_number_of_replicates < minimum_requirement_2 |
        influence_number_of_samples < minimum_requirement_3
  )]

  # If no IVD-MDs should be considered for exlusion:
  if (length(remove_these_methods) == 0) {
    remove_these_methods <- NULL
  }

  # General test
  valid_number_nas <- minimum_number_of_samples >= minimum_requirement_1

  out <- list(
    "pass" = valid_number_nas,
    "number_of_NAs_numeric_data" = number_of_NAs_numeric_data,
    "fraction_NAs_numeric_data" = fraction_NAs_numeric_data,
    "amount_NAs_numeric_data" = amount_NAs_numeric_data,
    "effective_numbers_of_samples" = effective_numbers_of_samples,
    "average_number_of_replicates" = average_number_of_replicates,
    "influence_number_of_samples" = influence_number_of_samples,
    "minimum_number_of_samples" = minimum_number_of_samples,
    "remove_these_methods" = remove_these_methods,
    "keep_these_ID_column_ids" = valid_ids_ID_columns,
    "keep_these_numeric_column_ids" = unique(unlist(MS_wise_na_ids))
  )

  return(out)

}

#' Calculate Data Quality Score of \code{data}
#'
#' @description
#' This function computes a quality score for the dataset based on various
#' metrics such as sample size, replicate numbers, and missing data.
#'
#' @param effective_number_of_samples An \code{integer} vector. Number of
#'                                    effective samples for each column.
#' @param influence_number_of_samples A \code{numeric} vector. Relative
#'                                    influence of each column's sample size.
#' @param average_number_of_replicates A \code{numeric} vector. Average number
#'                                     of replicates for each column.
#' @param fraction_of_NAs A \code{list}. Fraction of NAs for each column.
#' @param type A \code{character} string. Either \code{'cs'} for clinical
#'             samples or \code{'eqam'} for evaluated materials.
#'
#' @details
#' The function uses a point system based on four main criteria:
#' \itemize{
#'  \item Minimum number of effective clinical samples
#'  \item Minimum average number of replicates
#'  \item Similarity of effective clinical samples across columns
#'  \item Largest NA fraction among columns
#' }
#'
#' @return
#' An \code{integer}. The quality score. The calculated quality score (0-9).
#' @keywords internal
calculate_data_quality_score <- function(effective_number_of_samples,
                                         influence_number_of_samples,
                                         average_number_of_replicates,
                                         fraction_of_NAs,
                                         type = "cs") {

  # Setting default scores
  cs_score <- 0
  eq_score <- 9

  # Get Indicators
  minimum_effective_number_of_samples <- min(
    effective_number_of_samples,
    na.rm = TRUE
  )
  minimum_average_number_of_replicates <- min(
    average_number_of_replicates,
    na.rm = TRUE
  )
  similarity_number_of_effective_samples <- sum(
    abs(influence_number_of_samples - 1),
    na.rm = TRUE
  )
  maximum_fraction_of_NAs <- max(
    unlist(
      x = fraction_of_NAs,
      use.names = FALSE
    ),
    na.rm = TRUE
  )

  # Scoring based on minimum_effective_number_of_samples
  if (minimum_effective_number_of_samples >= 40) {
    cs_score <- cs_score + 4
    eq_score <- eq_score - 2
  }
  else if (minimum_effective_number_of_samples >= 25) {
    cs_score <- cs_score + 3
    eq_score <- eq_score - 1
  }
  else if (minimum_effective_number_of_samples >= 20) {
    cs_score <- cs_score + 2
  }
  else if (minimum_effective_number_of_samples >= 15) {
    cs_score <- cs_score
  }
  else {
    cs_score <- cs_score - 200
    if (minimum_effective_number_of_samples <= 6) {
      eq_score <- eq_score + 1
    }
  }

  # Scoring based on minimum_avarage_number_of_replicates
  if (minimum_average_number_of_replicates >= 6) {
    cs_score <- cs_score + 4
    eq_score <- eq_score + 1
  }
  else if(minimum_average_number_of_replicates >= 5) {
    cs_score <- cs_score + 3
  }
  else if(minimum_average_number_of_replicates >= 2.75) {
    cs_score <- cs_score + 2
    eq_score <- eq_score - 1
  }
  else if(minimum_average_number_of_replicates >= 2) {
    cs_score <- cs_score + 1
    eq_score <- eq_score - 2
  }
  else {
    eq_score <- eq_score - 3
  }

  # Scoring based on similarity_number_of_effective_samples
  if (similarity_number_of_effective_samples == 0) {
    cs_score <- cs_score + 3
    eq_score <- eq_score + 1
  }
  else if (similarity_number_of_effective_samples <= 0.10) {
    cs_score <- cs_score + 2
  }
  else if (similarity_number_of_effective_samples <= 0.25) {
    cs_score <- cs_score + 1
    eq_score <- eq_score - 1
  }
  else if (similarity_number_of_effective_samples <= 0.50) {
    eq_score <- eq_score - 2
  }
  else {
    cs_score <- cs_score - 1
    eq_score <- eq_score - 3
  }

  # Scoring based on maximum_fraction_of_NAs
  if (maximum_fraction_of_NAs <= 0.05) {
    cs_score <- cs_score + 1
  }
  else if (maximum_fraction_of_NAs <= 0.10) {
    cs_score <- cs_score
  }
  else if (maximum_fraction_of_NAs <= 0.25) {
    cs_score <- cs_score - 1
  }
  else {
    cs_score <- cs_score - 2
    eq_score <- eq_score - 1
  }

  # Max raw score for cs: 12
  # Min raw score for cs: -3
  # Max raw score for eq: 12
  # Min raw score for eq: 0

  if (cs_score < - 10) {
    cs_score <- -1
  }
  else {
    cs_score <- max(min(cs_score, 9), 0)
  }

  eq_score <- max(min(eq_score, 9), 0)


  if (type == "cs") {
    return(cs_score)
  }
  else {
    return(eq_score)
  }
}

#' @title
#' Create Data Check Summary
#'
#' @description
#' This function generates a comprehensive summary of the data quality check
#' results, including validity indicators, quality metrics, and suggested data
#' repairs.
#'
#' @param validity_indicators A \code{list}. Contains boolean indicators for
#'                            various validity checks.
#' @param minimum_number_of_samples An \code{integer}. The minimum number of
#'                                  effective samples across all IVD-MDs.
#' @param effective_number_of_samples An \code{integer} vector. Number of
#'                                    effective samples for each IVD-MD.
#' @param influence_number_of_samples A \code{numeric} vector. Relative
#'                                    influence of each IVD-MD's effective
#'                                    sample size.
#' @param average_number_of_replicates A \code{numeric} vector. Average number
#'                                     of replicates for each IVD-MD.
#' @param remove_these_methods A \code{character} vector. that should be
#'                             removed due to quality issues.
#' @param convert_these_methods_to_numeric A \code{character} vector. IVD-MDs
#'                                         that should be converted to numeric.
#' @param keep_these_ID_columns_ids An \code{integer} vector. Row IDs with
#'                                  valid identifier columns.
#' @param keep_these_numeric_columns_ids An \code{integer} vector. Row IDs with
#'                                       valid IVD-MD measurements.
#' @param number_of_NAs A \code{list}. Number of \code{NA}s for each IVD-MD.
#' @param fraction_of_NAs A \code{list}. Fraction of \code{NA}s for each IVD-MD.
#' @param amount_of_NAs A \code{list}. Categorical classification of
#'                      \code{NA} fractions.
#' @param type A \code{character} string. Either \code{'cs'} for clinical
#'             samples or \code{'eqam'} for evaluated materials.
#'
#' @return
#' A \code{list}. Contains the complete data check summary, including validity,
#' quality, badge, score, and repair suggestions.
#' @keywords internal
create_data_check_summary <- function(validity_indicators,
                                      minimum_number_of_samples,
                                      effective_number_of_samples,
                                      influence_number_of_samples,
                                      average_number_of_replicates,
                                      remove_these_methods,
                                      convert_these_methods_to_numeric,
                                      keep_these_ID_columns_ids,
                                      keep_these_numeric_columns_ids,
                                      number_of_NAs,
                                      fraction_of_NAs,
                                      amount_of_NAs,
                                      type = "cs"){

  # Initiate badge
  badge <- "perfect"
  out <- NULL

  # Minimum requirement check
  mandatory_id_columns <- validity_indicators$valid_mandatory_id_columns
  valid_numeric_columns <- validity_indicators$valid_numeric_columns

  # If minimium requirements fails:
  if ((!mandatory_id_columns) | (!valid_numeric_columns)) {
    badge <- "not acceptable"
    out <- list(
      "validity" = list(
        "valid_mandatory_id_columns" = mandatory_id_columns,
        "valid_numeric_columns" = valid_numeric_columns,
        "valid_number_nas" = NA,
        "valid_number_remaining_numeric" = NA
      ),
      "quality" = list(
        "minimum_number_of_samples" = minimum_number_of_samples,
        "effective_number_of_samples" = effective_number_of_samples,
        "average_number_of_replicates" = average_number_of_replicates,
        "number_of_NAs" = number_of_NAs,
        "fraction_of_NAs" = fraction_of_NAs,
        "amount_of_NAs" = amount_of_NAs
      ),
      "badge" = badge,
      "score" = NA_integer_,
      "repair" = list(
        "remove_these_methods" = remove_these_methods,
        "convert_these_methods_to_numeric" = convert_these_methods_to_numeric,
        "keep_these_ID_columns_ids" = keep_these_ID_columns_ids,
        "keep_these_numeric_columns_ids" = keep_these_numeric_columns_ids
      )
    )
    return(out)
  }

  # Calculate quality score
  score <- calculate_data_quality_score(
    effective_number_of_samples = effective_number_of_samples,
    influence_number_of_samples = influence_number_of_samples,
    average_number_of_replicates = average_number_of_replicates,
    fraction_of_NAs = fraction_of_NAs,
    type = type
  )

  # Determine badge based on score
  badge <- if (!validity_indicators$valid_number_remaining_numeric || score < 0) {
    "not acceptable"
  } else {
    switch(as.character(min(score, 8)),
           "0" = "extremely poor",
           "1" = "questionable",
           "2" = "questionable",
           "3" = "questionable",
           "4" = "acceptable",
           "5" = "acceptable",
           "6" = "acceptable",
           "7" = "acceptable",
           "8" = "perfect",
           "perfect" # Default case for scores > 8
    )
  }

  # Adjust score if necessary
  if (badge == "not acceptable") {
    score <- NA_integer_
  }

  out <- list(
    "validity" = list(
      "valid_mandatory_id_columns" = mandatory_id_columns,
      "valid_numeric_columns" = valid_numeric_columns,
      "valid_number_nas" = validity_indicators$valid_number_nas,
      "valid_number_remaining_numeric" = validity_indicators$valid_number_remaining_numeric
    ),
    "quality" = list(
      "minimum_number_of_samples" = minimum_number_of_samples,
      "effective_number_of_samples" = effective_number_of_samples,
      "influence_number_of_samples" = influence_number_of_samples,
      "average_number_of_replicates" = average_number_of_replicates,
      "number_of_NAs" = number_of_NAs,
      "fraction_of_NAs" = fraction_of_NAs,
      "amount_of_NAs" = amount_of_NAs
    ),
    "badge" = badge,
    "score" = score,
    "repair" = list(
      "remove_these_methods" = remove_these_methods,
      "convert_these_methods_to_numeric" = convert_these_methods_to_numeric,
      "keep_these_ID_columns_ids" = keep_these_ID_columns_ids,
      "keep_these_numeric_columns_ids" = keep_these_numeric_columns_ids
    )
  )
  return(out)
}

#' @title
#' Check if Data is Suitable for Further Use in This Package
#'
#' @param data A \code{data.table}, \code{list} or \code{data.frame}. The
#'             dataset to be validated.
#' @param type A \code{character} string. The type of \code{data} must be
#'             one of the following:
#'             \itemize{
#'                \item \code{'cs': } \code{data} is treated as clinical sample
#'                      data.
#'                \item \code{'eqam': } \code{data} is treated as evaluated
#'                      material data.
#'             }
#'             The suitability requirements for \code{data} will depend on
#'             \code{type}. For example, there are different requirements for
#'             clinical sample data and external quality assessment material
#'             data.
#' @return
#' A \code{list} of length \code{3}.
#'
#' @export
#'
#' @examples print(1)

check_data <- function(data, type = "cs"){

  # Mandatory ID columns
  expected_id_cols <- c("SampleID", "ReplicateID")

  # Initialize data column names
  numeric_column_names <- NULL

  # Validity Indicators
  valid_mandatory_id_columns <- FALSE
  valid_numeric_columns <- NA
  valid_number_nas <- NA
  valid_number_remaining_numeric <- NA
  validity_indicators <- list(
    "valid_mandatory_id_columns" = valid_mandatory_id_columns,
    "valid_numeric_columns" = valid_numeric_columns,
    "valid_number_nas" = valid_number_nas,
    "valid_number_remaining_numeric" = valid_number_remaining_numeric
  )

  # Quality Indicators
  minimum_number_of_samples <- NA_integer_
  effective_number_of_samples <- NA_integer_
  influence_number_of_samples <- NA_real_
  average_number_of_replicates <- NA_real_

  # Repair Indicators
  remove_these_methods <- NULL
  convert_these_methods_to_numeric <- NULL
  keep_these_ID_columns_ids <- NULL
  keep_these_numeric_columns_ids <- NULL

  # Descriptive statistics
  number_of_NAs <- NA
  fraction_of_NAs <- NA
  amount_of_NAs <- NA

  # Attempt to convert data to data.table
  data_conversion <- convert_data_cd(data)
  if(!data_conversion$can_convert_data){
    stop(
      "'data' could not be converted to any supported format."
    )
  }

  # The resulting data.table
  data <- data_conversion$data

  # Check for mandatory ID columns
  mandatory_id_columns_check <- check_mandatory_id_columns(data)

  # Check if the mandatory ID columns test pass
  validity_indicators$valid_mandatory_id_columns <- mandatory_id_columns_check$pass

  # If it does not pass, exit early
  if (!mandatory_id_columns_check$pass) {
    return(create_data_check_summary(
      "validity_indicators" = validity_indicators,
      "minimum_number_of_samples" = minimum_number_of_samples,
      "effective_number_of_samples" = effective_number_of_samples,
      "average_number_of_replicates" = average_number_of_replicates,
      "remove_these_methods" = remove_these_methods,
      "convert_these_methods_to_numeric" = convert_these_methods_to_numeric,
      "keep_these_ID_columns_ids" = keep_these_ID_columns_ids,
      "keep_these_numeric_columns_ids" = keep_these_numeric_columns_ids,
      "number_of_NAs" = number_of_NAs,
      "fraction_of_NAs" = fraction_of_NAs,
      "amount_of_NAs" = amount_of_NAs,
      "type" = type))
  }

  # Get data with corrected ID columns
  data <- mandatory_id_columns_check$data

  # Check numeric columns
  numeric_columns_check <- check_numeric_columns(data)

  # Check if the numeris columns pass test
  validity_indicators$valid_numeric_columns <- numeric_columns_check$pass

  # If it does not pass, exit early
  if (!numeric_columns_check$pass) {
    return(create_data_check_summary(
      "validity_indicators" = validity_indicators,
      "minimum_number_of_samples" = minimum_number_of_samples,
      "effective_number_of_samples" = effective_number_of_samples,
      "influence_number_of_samples" = influence_number_of_samples,
      "average_number_of_replicates" = average_number_of_replicates,
      "remove_these_methods" = remove_these_methods,
      "convert_these_methods_to_numeric" = convert_these_methods_to_numeric,
      "keep_these_ID_columns_ids" = keep_these_ID_columns_ids,
      "keep_these_numeric_columns_ids" = keep_these_numeric_columns_ids,
      "number_of_NAs" = number_of_NAs,
      "fraction_of_NAs" = fraction_of_NAs,
      "amount_of_NAs" = amount_of_NAs,
      "type" = type
      ))
  }

  # Get names of numeric columns
  numeric_column_names <- numeric_columns_check$numeric_columns
  convert_these_methods_to_numeric <- numeric_columns_check$convert_these_numeric_columns
  # Check number of NA values
  number_of_na_values_check <- check_na_fraction(data, type)
  validity_indicators$valid_number_nas <- number_of_na_values_check$pass

  # Other indicators from check_na_fraction(data, type)
  remove_these_methods <- number_of_na_values_check$remove_these_methods

  # Check if number of numeric columns still is larger than 2 after exclusion
  if (!is.null(remove_these_methods)) {
    remove_ids <- which(names(data) %in% c("SampleID", "ReplicateID", remove_these_methods))
    remaining_numeric_columns <- names(data)[-remove_ids]
    if (length(remaining_numeric_columns) < 2) {
      validity_indicators$valid_number_remaining_numeric <- FALSE
    }
    else {
      validity_indicators$valid_number_remaining_numeric <- TRUE
    }
  }
  else {
    validity_indicators$valid_number_remaining_numeric <- TRUE
  }

  return(create_data_check_summary(
    "validity_indicators" = validity_indicators,
    "minimum_number_of_samples" = number_of_na_values_check$minimum_number_of_samples,
    "effective_number_of_samples" = number_of_na_values_check$effective_numbers_of_samples,
    "influence_number_of_samples" = number_of_na_values_check$influence_number_of_samples,
    "average_number_of_replicates" = number_of_na_values_check$average_number_of_replicates,
    "convert_these_methods_to_numeric" = convert_these_methods_to_numeric,
    "remove_these_methods" = remove_these_methods,
    "keep_these_ID_columns_ids" = number_of_na_values_check$keep_these_ID_column_ids,
    "keep_these_numeric_columns_ids" = number_of_na_values_check$keep_these_numeric_column_ids,
    "number_of_NAs" = number_of_na_values_check$number_of_NAs_numeric_data,
    "fraction_of_NAs" = number_of_na_values_check$fraction_NAs_numeric_data,
    "amount_of_NAs" = number_of_na_values_check$amount_NAs_numeric_data,
    "type" = type
    )
  )

}

#' @title
#' Repair a Method Comparison Dataset
#'
#' @description
#' This function attempts to repair and standardize the input dataset for use
#' in method comparison analyses. It performs various checks and corrections to
#' ensure data integrity and compatibility with other functions in the package.
#'
#' @param data A \code{data.table}, \code{list} or \code{data.frame}. The data
#'             that is desired to repair.
#' @param type A \code{character} string. The type of \code{data} must be
#'             one of the following:
#'             \itemize{
#'                \item \code{'cs': } \code{data} is treated as clinical sample
#'                      data.
#'                \item \code{'eqam': } \code{data} is treated as evaluated
#'                      material data.
#'             }
#'             The suitability requirements for \code{data} will depend on
#'             \code{type}. For example, there are different requirements for
#'             clinical sample data and external quality assessment material
#'             data.
#'
#' @param remove_invalid_methods A \code{logical} value. If \code{TRUE}
#'                               (default), IVD-MDs deemed invalid will be
#'                               removed during the repair process.
#' @param include_repair_summary A \code{logical} value. If \code{TRUE}, a
#'                               summary of the repair process will be included
#'                               in the output.
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'  \item Checks the input data using the \code{check_data()} function.
#'  \item Converts the data to a \code{data.table} if possible.
#'  \item Standardizes ID column names: \code{SampleID} and \code{ReplicateID}.
#'  \item Converts methods to \code{numeric} format if necessary.
#'  \item Reorders \code{numeric} columns alphabetically.
#'  \item Removes invalid \code{numeric} columns if specified.
#'  \item Removes rows with \code{NA} values in ID columns.
#'  \item Sorts the data by \code{SampleID} and then \code{ReplicateID}.
#'  \item Converts \code{SampleID} and \code{ReplicateID} to \code{character}.
#' }
#'
#' @return
#' If \code{include_repair_summary} is \code{FALSE}, returns a repaired
#' \code{data.table}. If \code{TRUE}, returns a \code{list} containing:
#' \itemize{
#'  \item \code{repaired_data: } A \code{data.table}. The repaired \code{data}.
#'  \item \code{repair_summary: } A \code{data.table} summarizing the repair
#'        process.
#' }
#'
#' @export
#'
#' @examples
#' # Required packages
#' library(smooth.commutability)
#' library(data.table)
#'
#' # Get example data from smooth.commutability package
#' test_cs_data <- reverse_comparison_data(copy(crp_cs_data))
#' test_eq_data <- reverse_comparison_data(copy(crp_eqam_data))
#'
#' # Repaired clinical sample data (with repair summary)
#' repaired_test_cs_data <- repair_data(
#'  data = test_cs_data,
#'  type = "cs",
#'  remove_invalid_methods = TRUE,
#'  include_repair_summary = TRUE
#' )
#'
#' # The repaired data:
#' repaired_test_cs_data$repaired_data
#'
#' # the repair summary:
#' repaired_test_cs_data$repair_summary
#'
#' # Repaired external quality assessment material data (without repair summary)
#' repaired_test_eq_data <- repair_data(
#'  data = test_eq_data,
#'  type = "eqam",
#'  remove_invalid_methods = TRUE,
#'  include_repair_summary = TRUE
#' )
#'
#' # The repaired data
#' repaired_test_eq_data
#'

repair_data <- function(data, type = "cs", remove_invalid_methods = TRUE, include_repair_summary = FALSE){

  # Bind global variables
  SampleID <- ReplicateID <- NULL

  # Perform check on data
  data_check <- check_data(data, type = type)

  # Get quality badge from data_check()
  quality_indicator <- switch(data_check$badge,
                              "not acceptable" = 0,
                              "extremely poor" = 1,
                              "questionable" = 2,
                              "acceptable" = 3,
                              "perfect" = 4)

  # Check if data can be repaired
  can_repair <- quality_indicator > 0

  if (!can_repair) {
    warning(
      "Could not repair data because it was too far from expected ",
      "structure. Input data is returned."
    )
    return(data)
  }

  # Attempt to convert data to data.table
  data_conversion <- convert_data_cd(data)

  if (!data_conversion$can_convert_data) {
    warning(
      "Could not repair data. This is because data was not a data.table, list,",
      " or a data.frame.",
      " Input data is returned."
    )
    return(data)
  }
  data <- data_conversion$data

  # Change ID column names to SampleID and ReplicateID
  ID_columns_found_before <- all(
    c("SampleID", "ReplicateID") %in% names(data)
  )

  data <- check_mandatory_id_columns(
    data = data
  )$data

  ID_columns_found_after <- all(
    c("SampleID", "ReplicateID") %in% names(data)
  )

  # Convert methods to numeric iff necessary
  convert_these_methods_to_numeric_in_data <- data_check$repair$convert_these_methods_to_numeric
  if (!is.null(convert_these_methods_to_numeric_in_data)) {
    data <- data[, lapply(
      X = .SD,
      FUN = function(numeric_col) {
        not_convert <- stri_detect_regex(
          str = numeric_col,
          pattern = "[^0-9.]"
        )
        convert <- !not_convert
        if (!is.na(convert) & convert) {
          as.numeric(numeric_col)
        }
        else {
          NA_real_
        }
      }
    ),
    .SDcols = convert_these_methods_to_numeric_in_data,
    by = c(
      setdiff(
        x = names(data),
        y = convert_these_methods_to_numeric_in_data
      )
    )
  ]

  }

  # Change numeric column IDs to alphabetic, Precalcs
  mandatory_ID_columns <- c("SampleID", "ReplicateID")
  mandatory_ID_columns_IDs <- which(names(data) %in% mandatory_ID_columns)
  ID_data <- copy(data)[, mandatory_ID_columns_IDs,
                        with = FALSE]
  numeric_column_IDs <- setdiff(
    x = seq_len(ncol(data)),
    y = mandatory_ID_columns_IDs
  )
  numeric_data <- copy(data)[, numeric_column_IDs,
                             with = FALSE]

  # Change order, and record order before and after
  order_before <- order(
    names(numeric_data),
    decreasing = FALSE
  )
  setcolorder(
    x = numeric_data,
    neworder = order_before
  )
  order_post <- order(
    names(numeric_data),
    decreasing = FALSE
  )

  # Get data with numerical columns in alphabetic order
  data <- cbind(
    ID_data,
    numeric_data
  )

  # Remove methods iff necessary AND allowed
  remove_these_methods_from_data <- data_check$repair$remove_these_methods
  if (!is.null(remove_these_methods_from_data) & remove_invalid_methods) {
    remove_invalid_methods_IDs <- which(names(data) %in% remove_these_methods_from_data)
    data <- data[, -remove_invalid_methods_IDs,
                 with = FALSE]
  }

  # Remove ID columns NA values
  keep_these_ID_column_IDs <- data_check$repair$keep_these_ID_columns_ids
  data <- data[keep_these_ID_column_IDs, ]

  # Remove numeric columns NA values
  keep_these_numeric_columns_IDs <- data_check$repair$keep_these_numeric_columns_ids
  data <- data[keep_these_numeric_columns_IDs, ]

  # Fix order of SampleID and ReplicateID
  setorder(
    x = data,
    SampleID,
    ReplicateID,
    na.last = FALSE
  )

  # Convert SampleID and ReplicateID to character
  data[, SampleID := as.character(SampleID)]
  data[, ReplicateID := as.character(ReplicateID)]

  if (!include_repair_summary) {
    data <- copy(data)
    return(data)
  }

  # Perform new check
  data_check_post_repair <- check_data(
    data = data,
    type = type
  )

  # Get quality badge from data_check()
  quality_indicator_post_repair <- switch(data_check_post_repair$badge,
                                          "not acceptable" = 0,
                                          "extremely poor" = 1,
                                          "questionable" = 2,
                                          "acceptable" = 3,
                                          "perfect" = 4)

  # Get some general repair effects
  repair_effect <- quality_indicator_post_repair - quality_indicator
  score_effect <- data_check_post_repair$score - data_check$score

  # Get the repair summary
  repair_summary <- data.table(
    "information" = c(
      "before",
      "after",
      "repair effect"
    ),
    "ID columns" = c(
      if (ID_columns_found_before) {
        "correct"
      } else {
        "incorrect"
      },
      "correct",
      if (ID_columns_found_before) {
        "same as before"
      } else {
        "corrected"
      }
    ),
    "numeric column order" = c(
      if (any(order_before!=order_post)) {
        "not alphabetic"
      }
      else {
        "alphabetic"
      },
      "alphabetic",
      if (any(order_before!=order_post)) {
        "to alphabetic"
      }
      else {
        "same as before"
      }
    ),
    "score" = c(
      data_check$score,
      data_check_post_repair$score,
      score_effect
    ),
    "badge" = c(
      data_check$badge,
      data_check_post_repair$badge,
      if (score_effect > 0) {
        "improved"
      }
      else if (score_effect < 0) {
        "worsened"
      }
      else{
        "same as before"
      }
    )
  )

  return(list(
    repaired_data = data,
    repair_summary = repair_summary
  ))

}

#' @title
#' Check Structural Equivalence Between Clinical Sample and Evaluated Material Data
#'
#' @param cs_data A \code{data.table} containing clinical sample data. Must
#'                contain columns  compatible with the
#'                \code{repair_data(type = "cs")} preprocessing function.
#' @param eq_data A \code{data.table} containing evaluated material data. Must
#'                contain columns compatible with the
#'                \code{repair_data(type = "eqam")} preprocessing function.
#'
#' @description
#' Compares the structural compatibility of two datasets (clinical samples and
#' evaluated materials) by verifying identical column names and matching column
#' order. This validation is essential for ensuring dataset compatibility in
#' downstream analyses or merging operations.
#'
#' @details
#' Function Workflow:
#'
#' 1. Data Preprocessing:
#' \itemize{
#'  \item Attempts to repair both datasets using type-specific
#'        \code{repair_data} calls.
#'  \item Returns early with available information if either dataset fails
#'        preprocessing.
#' }
#'
#' 2. Name Comparison:
#' \itemize{
#'  \item Checks for exact match between column names using set operations.
#'  \item Identifies mismatches using \code{setdiff()}.
#'  \item Converts mismatches to comma-separated strings for readability.
#' }
#'
#' 3. Order Comparison:
#' \itemize{
#'  \item Only performed if column names are identical.
#'  \item Verifies identical column sequence using element-wise comparison.
#' }
#'
#' Use Cases:
#' \itemize{
#'  \item Validation step before dataset merging.
#'  \item Quality control in automated data pipelines.
#'  \item Preprocessing check for analytical functions requiring matched schemas.
#' }
#'
#' @return
#' A \code{list} with the following components:
#' \itemize{
#'   \item \code{equal_names: } A \code{logical} value. Indicates if both
#'         datasets have identical column names.
#'   \item \code{equal_order: } A \code{logical} value. Indicates if column
#'         order is identical (can only be \code{TRUE} if \code{equal_names} is
#'         \code{TRUE}).
#'   \item \code{names_in_cs_data_but_not_in_eq_data: } A \code{character}
#'          string. Names unique to \code{cs_data}.
#'   \item \code{names_in_eq_data_but_not_in_cs_data: } A \code{character}
#'          string. Names unique to \code{cs_data}.
#'   \item \code{order_cs_data: } A \code{character} string. Column order in
#'         \code{cs_data}.
#'   \item \code{order_eq_data: } A \code{character} string. Column order in
#'         \code{eq_data}.
#' }
#' @export
#'
#' @examples
#' # Required packages
#' library(smooth.commutability)
#' library(data.table)
#'
#' # Get example data from smooth.commutability package
#' test_cs_data <- reverse_comparison_data(copy(crp_cs_data))
#' test_eq_data <- reverse_comparison_data(copy(crp_eqam_data))
#'
#' # Check equivalence:
#' check_equivalence(
#'   cs_data = test_cs_data,
#'   eq_data = test_eq_data
#' )
#'
check_equivalence <- function(cs_data, eq_data){

  # Equivalence Indators
  equal_names <- NULL
  equal_order <- NULL
  names_in_cs_data_but_not_in_eq_data <- NULL
  names_in_eq_data_but_not_in_cs_data <- NULL
  order_cs_data <- NULL
  order_eq_data <- NULL

  # Attempt to repair clinical sample data
  cs_data <- tryCatch(
    expr = {
      repair_data(
        data = cs_data,
        type = "cs",
        remove_invalid_methods = TRUE,
        include_repair_summary = FALSE
      )
    },
    error = function(e) NULL,
    warning = function(w) NULL
  )

  # Attempt to repair evaluated material data
  eq_data <- tryCatch(
    expr = {
      repair_data(
        data = eq_data,
        type = "eqam",
        remove_invalid_methods = TRUE,
        include_repair_summary = FALSE
      )
    },
    error = function(e) NULL,
    warning = function(w) NULL
  )

  if (is.null(cs_data)) {
    warning(
      "cs_data could not be repaired. Check if the input cs_data is a valid ",
      "data.table object. Use check_data(data = cs_data, type = 'cs') to get ",
      "more information.",
      immediate. = TRUE
    )
    out <- list(
      "equal_names" = equal_names,
      "equal_order" = equal_order,
      "names_in_cs_data_but_not_in_eq_data" = names_in_cs_data_but_not_in_eq_data,
      "names_in_eq_data_but_not_in_cs_data" = names_in_eq_data_but_not_in_cs_data,
      "order_cs_data" = order_cs_data,
      "order_eq_data" = order_cs_data
    )
    return(out)
  }
  else if (is.null(eq_data)) {
    warning(
      "eq_data could not be repaired. Check if the input eq_data is a valid ",
      "data.table object. Use check_data(data = eq_data, type = 'eqam') to get ",
      "more information.",
      immediate. = TRUE
    )
    out <- list(
      "equal_names" = equal_names,
      "equal_order" = equal_order,
      "names_in_cs_data_but_not_in_eq_data" = names_in_cs_data_but_not_in_eq_data,
      "names_in_eq_data_but_not_in_cs_data" = names_in_eq_data_but_not_in_cs_data,
      "order_cs_data" = order_cs_data,
      "order_eq_data" = order_cs_data
    )
    return(out)
  }

  # Check for name dipracencies
  equal_names <- TRUE
  cs_data_names <- names(cs_data)
  eq_data_names <- names(eq_data)
  names_in_cs_data_but_not_in_eq_data <- setdiff(
    x = cs_data_names,
    y = eq_data_names
  )
  names_in_eq_data_but_not_in_cs_data <- setdiff(
    x = eq_data_names,
    y = cs_data_names
  )

  if (length(names_in_cs_data_but_not_in_eq_data) > 0) {
    equal_names <- FALSE
    names_in_cs_data_but_not_in_eq_data <- paste(
      names_in_cs_data_but_not_in_eq_data,
      collapse = ", "
    )
  }
  else {
    names_in_cs_data_but_not_in_eq_data <- NULL
  }
  if (length(names_in_eq_data_but_not_in_cs_data) > 0) {
    equal_names <- FALSE
    names_in_eq_data_but_not_in_cs_data <- paste(
      names_in_eq_data_but_not_in_cs_data,
      collapse = ", "
    )
  }
  else {
    names_in_eq_data_but_not_in_cs_data <- NULL
  }
  if (!equal_names) {
    out <- list(
      "equal_names" = equal_names,
      "equal_order" = equal_order,
      "names_in_cs_data_but_not_in_eq_data" = names_in_cs_data_but_not_in_eq_data,
      "names_in_eq_data_but_not_in_cs_data" = names_in_eq_data_but_not_in_cs_data,
      "order_cs_data" = paste(
        cs_data_names,
        collapse = ", "
      ),
      "order_eq_data" = paste(
        eq_data_names,
        collapse = ", "
      )
    )
    return(out)
  }

  # Check for order dipracancies
  equal_order <- tryCatch(
    expr = {
      all(cs_data_names == eq_data_names)
    },
    error = function(e) FALSE,
    warning = function(w) FALSE
  )

  if (!equal_order) {
    out <- list(
      "equal_names" = equal_names,
      "equal_order" = equal_order,
      "names_in_cs_data_but_not_in_eq_data" = names_in_cs_data_but_not_in_eq_data,
      "names_in_eq_data_but_not_in_cs_data" = names_in_eq_data_but_not_in_cs_data,
      "order_cs_data" = paste(
        cs_data_names,
        collapse = ", "
      ),
      "order_eq_data" = paste(
        eq_data_names,
        collapse = ", "
      )
    )
    return(out)
  }

  out <- list(
    "equal_names" = equal_names,
    "equal_order" = equal_order,
    "names_in_cs_data_but_not_in_eq_data" = names_in_cs_data_but_not_in_eq_data,
    "names_in_eq_data_but_not_in_cs_data" = names_in_eq_data_but_not_in_cs_data,
    "order_cs_data" = paste(
      cs_data_names,
      collapse = ", "
    ),
    "order_eq_data" = paste(
      eq_data_names,
      collapse = ", "
    )
  )

  return(out)

}


