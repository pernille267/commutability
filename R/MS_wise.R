#' @title
#' Get Unique IVD-MD Combinations
#'
#' @description
#' Get IVD-MD combinations among the IVD-MDs in \code{numeric_columns}.
#'
#'
#' @param numeric_columns A \code{character} vector. The names of the IVD-MDs
#'                        in the original dataset. Should ideally be sorted
#'                        in alphabetical order.
#' @param reference A \code{character} string. If not \code{NULL}, the name of
#'                  the IVD-MD among \code{numeric_columns} that should be used
#'                  as a reference IVD-MD. See details.
#'
#' @details
#' If \code{reference} is a \code{character} string among
#' \code{numeric_columns}, all other IVD-MDs in \code{numeric_columns} are
#' paired with the IVD-MD provided in \code{reference}. Otherwise, all unique
#' pairwise combinations of \code{numeric_columns} are used.
#'
#' This function should not be used by end-users directly. It is used
#' internally within the \code{get_comparison_data} function.
#'
#'
#' @returns
#' A \code{character} matrix.
#'
#' @keywords internal
get_combos <- function(numeric_columns, reference = NULL) {

  # Bind global variables
  combos <- NULL

  # If no IVD-MD is used as reference
  if (is.null(reference)) {
    combos <- combn(x = numeric_columns, m = 2L, simplify = TRUE)
    return(combos)
  }

  if (!is.character(reference)) {
    stop(
      "reference is expected to be a character string, not a ",
      class(reference),
      "."
    )
  }

  if (length(reference) > 1) {
    warning(
      "reference is expected to have length 1L. ",
      "The first element of reference is used.",
      immediate. = TRUE
    )
    reference <- reference[1]
  }

  if (!any(reference == numeric_columns)) {
    stop(
      "The given reference IVD-MD, ",
      reference,
      ", is not found among the IVD-MDs given in data."
    )
  }

  combos_row_1 <- numeric_columns[-match(reference, numeric_columns)]
  combos_row_2 <- rep(x = reference, times = length(combos_row_1))

  combos <- rbind(combos_row_1,
                  combos_row_2)

  return(combos)
}


#' @title
#' Transform IVD-MD data to IVD-MD Comparison-wise Data (Deprecated)
#'
#' @param data A \code{data.table} or \code{list}. Must contain:
#'             \itemize{
#'                \item \code{SampleID: } A \code{character} vector. The sample
#'                      identifiers.
#'                \item \code{ReplicateID: } A \code{character} vector. The
#'                      replicate measurement identifiers.
#'                \item \code{IVD-MD 1: } A \code{numeric} vector. The first
#'                      IVD-MD.
#'                \item \code{...}
#'                \item \code{IVD-MD n:} A \code{numeric} vector. The n-th
#'                      IVD-MD.
#'             }
#'
#' @description
#' Processes \code{data}, by grouping measurements based on every unique
#' combination of the IVD-MD measurement columns. Within each IVD-MD
#' combination, measurements are further grouped by \code{SampleID} and
#' \code{ReplicateID}.
#'
#' @details
#' To ensure optimal compatibility with other functions in this package,
#' it is recommended that the IVD-MD measurement columns in \code{data} are
#' sorted in alphabetical order. This assists in consistently maintaining the
#' 'comparison' column of the output for both clinical sample (CS) data and
#' evaluated material data.
#'
#' Note \code{repair_data()} can assist to convert \code{data} to the correct
#' format. As such, it is a good practice to preprocess \code{data} through
#' \code{repair_data()} prior to using this function.
#'
#' @return
#' A \code{data.table}. The long-formatted equivalent of the input \code{data}.
#' Will encompass the following columns in the specified order:
#' \itemize{
#'  \item \code{comparison: } A \code{character} vector. The comparison
#'        identifiers. It is on the form \code{'MP_A - MP_B'}.
#'  \item \code{SampleID: } A \code{character} vector. The sample identifiers.
#'  \item \code{ReplicateID: } A \code{character} vector. The replicated
#'        measurement identifiers.
#'  \item \code{MP_A: } A \code{numeric} vector. The observed measurement
#'        results from IVD-MD \code{MP_A} (response).
#'  \item \code{MP_B: } A \code{numeric} vector. The observed measurement
#'        results from IVD-MD \code{MP_B} (predictor).
#'
#' }
#' @export
#'
#' @examples print(1)

MS_wise <- function(data){

  # Bind Global Variables
  comparison <- NULL

  # Attempt to convert data to data.table
  attempt_convert_data <- convert_data_cd(data)

  # Check if cannot be converted
  if(!attempt_convert_data$can_convert) {
    stop(
      "Invalid input class for 'data'. Your input class: '",
      class(data)[1],
      "'.",
      " Expected input classes are data.table, list, or data.frame.",
      " Cannot proceed with transformation from wide-formatted to ",
      "long-formatted data for the provided input class.",
      " Please revise the 'data' input and try again."
    )
  }

  data <- attempt_convert_data$data

  # Check if comparison already is in data
  already_MS_wise <- any("comparison" == names(data))

  # Throw warning if comparison already is in data and return input data
  if(isTRUE(already_MS_wise)){
    warning(
      sprintf(
        paste("The input 'data' appears to already be in long format.",
              "This was determined because the 'comparison' column was found",
              "in 'data'. The 'comparison' column is the %d. column.",
              "Consider checking your data to ensure it is in the expected format."),
        which(names(data) == "comparison")
      ),
      immediate. = TRUE
    )
    return(data)
  }

  mandatory_ID_columns <- c("SampleID", "ReplicateID")

  # Attempt to locate mandatory ID columns
  attempt_find_mandatory_ID_columns <- check_mandatory_id_columns(data)

  # Throw error if mandatory ID columns are not located
  if(!attempt_find_mandatory_ID_columns$pass) {
    stop(
      sprintf(
        paste("The input 'data' must include both ID columns: '%s' and '%s'.",
              "Only these columns were found in your 'data' input: [%s].",
              "Please ensure both ID columns are present and try again."),
        mandatory_ID_columns[1],
        mandatory_ID_columns[2],
        paste(names(data), collapse = ", "))
    )
  }

  # Get data, that now have valid ID column names
  data <- attempt_find_mandatory_ID_columns$data

  # Get names of valid data
  data_names <- names(data)

  # Get IVD-MD comparison data
  numeric_columns <- setdiff(x = data_names, y = mandatory_ID_columns)
  combos <- combn(x = numeric_columns, m = 2L, simplify = TRUE)
  ivd_md_pairs <- apply(X = combos, MARGIN = 2, FUN = paste, collapse = " - ")
  out <- melt(data = data,
              measure.vars = list(combos[1L, ], combos[2L, ]),
              value.name = c("MP_A", "MP_B"),
              variable.name = "comparison")
  out[, comparison := as.character(`levels<-`(comparison, ivd_md_pairs))]
  out <- as.list(out)
  out <- data.table("comparison" = as.character(out$comparison),
                    "SampleID" = as.character(out$SampleID),
                    "ReplicateID" = as.character(out$ReplicateID),
                    "MP_A" = out$MP_A,
                    "MP_B" = out$MP_B)
  setDT(out)
  return(out)
}

#' @title
#' Transform IVD-MD data to IVD-MD Comparison-wise Data
#'
#' @param data A \code{data.table} or \code{list}. Must contain:
#'             \itemize{
#'                \item \code{SampleID: } A \code{character} vector. The sample
#'                      identifiers.
#'                \item \code{ReplicateID: } A \code{character} vector. The
#'                      replicate measurement identifiers.
#'                \item \code{IVD-MD 1: } A \code{numeric} vector. The first
#'                      IVD-MD.
#'                \item \code{...}
#'                \item \code{IVD-MD n:} A \code{numeric} vector. The n-th
#'                      IVD-MD.
#'             }
#'
#' @param reference A \code{character} string. The name of the IVD-MD that is
#'                  used as the reference IVD-MD. If \code{NULL} (default), no
#'                  IVD-MD is used as reference.
#'
#' @description
#' Processes \code{data}, by grouping measurements based on every unique
#' combination of the IVD-MD measurement columns. Within each IVD-MD
#' combination, measurements are further grouped by \code{SampleID} and
#' \code{ReplicateID}.
#'
#' @details
#' To ensure optimal compatibility with other functions in this package,
#' it is recommended that the IVD-MD measurement columns in \code{data} are
#' sorted in alphabetical order. This assists in consistently maintaining the
#' 'comparison' column of the output for both clinical sample (CS) data and
#' evaluated material data.
#'
#' Note \code{repair_data()} can assist to convert \code{data} to the correct
#' format. As such, it is a good practice to preprocess \code{data} through
#' \code{repair_data()} prior to using this function.
#'
#' @return
#' A \code{data.table}. The long-formatted equivalent of the input \code{data}.
#' Will encompass the following columns in the specified order:
#' \itemize{
#'  \item \code{comparison: } A \code{character} vector. The comparison
#'        identifiers. It is on the form \code{'MP_A - MP_B'}.
#'  \item \code{SampleID: } A \code{character} vector. The sample identifiers.
#'  \item \code{ReplicateID: } A \code{character} vector. The replicated
#'        measurement identifiers.
#'  \item \code{MP_A: } A \code{numeric} vector. The observed measurement
#'        results from IVD-MD \code{MP_A} (response).
#'  \item \code{MP_B: } A \code{numeric} vector. The observed measurement
#'        results from IVD-MD \code{MP_B} (predictor).
#'
#' }
#' @export
#'
#' @examples print(1)

get_comparison_data <- function(data, reference = NULL){

  # Bind Global Variables
  comparison <- NULL

  # Attempt to convert data to data.table
  attempt_convert_data <- convert_data_cd(data)

  # Check if cannot be converted
  if (!attempt_convert_data$can_convert) {
    stop(
      "Invalid input class for 'data'. Your input class: '",
      class(data)[1],
      "'.",
      " Expected input classes are data.table, list, or data.frame.",
      " Cannot proceed with transformation from wide-formatted to ",
      "long-formatted data for the provided input class.",
      " Please revise the 'data' input and try again."
    )
  }

  data <- attempt_convert_data$data

  # Check if comparison already is in data
  already_MS_wise <- any("comparison" == names(data))

  # Throw warning if comparison already is in data and return input data
  if(isTRUE(already_MS_wise)){
    warning(
      sprintf(
        paste("The input 'data' appears to already be in long format.",
              "This was determined because the 'comparison' column was found",
              "in 'data'. The 'comparison' column is the %d. column.",
              "Consider checking your data to ensure it is in the expected format."),
        which(names(data) == "comparison")
      ),
      immediate. = TRUE
    )
    return(data)
  }

  mandatory_ID_columns <- c("SampleID", "ReplicateID")

  # Attempt to locate mandatory ID columns
  attempt_find_mandatory_ID_columns <- check_mandatory_id_columns(data)

  # Throw error if mandatory ID columns are not located
  if(!attempt_find_mandatory_ID_columns$pass) {
    stop(
      sprintf(
        paste("The input 'data' must include both ID columns: '%s' and '%s'.",
              "Only these columns were found in your 'data' input: [%s].",
              "Please ensure both ID columns are present and try again."),
        mandatory_ID_columns[1],
        mandatory_ID_columns[2],
        paste(names(data), collapse = ", "))
    )
  }

  # Get data, that now have valid ID column names
  data <- attempt_find_mandatory_ID_columns$data

  # Get names of valid data
  data_names <- names(data)

  # Get IVD-MD comparison data
  numeric_columns <- setdiff(x = data_names, y = mandatory_ID_columns)
  combos <- get_combos(numeric_columns = numeric_columns,
                       reference = reference)
  ivd_md_pairs <- apply(X = combos, MARGIN = 2, FUN = paste, collapse = " - ")
  out <- melt(data = data,
              measure.vars = list(combos[1L, ], combos[2L, ]),
              value.name = c("MP_A", "MP_B"),
              variable.name = "comparison")
  out[, comparison := as.character(`levels<-`(comparison, ivd_md_pairs))]
  out <- as.list(out)
  out <- data.table("comparison" = as.character(out$comparison),
                    "SampleID" = as.character(out$SampleID),
                    "ReplicateID" = as.character(out$ReplicateID),
                    "MP_A" = out$MP_A,
                    "MP_B" = out$MP_B)
  setDT(out)
  return(out)

}

#' @title
#' Revert Output From \code{get_comparison_data} to Original Form
#'
#' @param data A \code{list}, \code{data.table} or \code{data.frame}. The
#'             IVD-MD comparison-wise data. Must contain the following
#'             variables:
#'             \itemize{
#'                \item \code{comparison: } A \code{character} vector. The
#'                      comparison identifiers. Typically on the form
#'                      \code{'MP_A - MP_B'}.
#'                \item \code{SampleID: } A \code{character} vector. The sample
#'                      identifiers for the clinical samples.
#'                \item \code{ReplicateID: }A \code{character} vector. The
#'                      replicated measurement identifiers.
#'                \item \code{MP_A: } A \code{numeric} vector. The observed
#'                      measurement results from IVD-MD \code{MP_A} (response).
#'                \item \code{MP_B: } A \code{numeric} vector. The observed
#'                      measurement results from IVD-MD \code{MP_A} (predictor).
#'             }
#'
#' @description
#' See title. Backtrack to the data before \code{get_comparison_data} is used
#' on it.
#'
#' @details
#' The numerical columns may not be in alphabetical order after this function
#' is used even if they were in alphabetical order before
#' \code{get_comparison_data} was applied. Use \code{repair_data()} on the
#' output to ensure that the newly reverted \code{data} is up to par.
#'
#' @returns
#' A \code{data.table}. The original data before \code{get_comparison_data} was
#' applied.
#' @export
#'
#' @examples
#' # Required packages
#' library(smooth.commutability)
#'
#' # Use example data from
#' cs_test_data <- crp_cs_data
#'
#' # Output
#' print(reverse_comparison_data(cs_test_data))
#'
reverse_comparison_data <- function(data) {

  # Bind global variables
  comparison <- NULL

  # Avoid modifying the original data
  data <- copy(data)

  # Initialize output
  output <- NULL

  # Split comparison vector
  data[, `:=`(MP_Y = stri_split(comparison, fixed = " - ")[[1]][1],
              MP_X = stri_split(comparison, fixed = " - ")[[1]][2]),
       by = .I]

  # Get separate parts
  MP_Y_data <- data[, -c("comparison", "MP_B", "MP_X")]
  MP_X_data <- data[, -c("comparison", "MP_A", "MP_Y")]

  # Convert each part to wide-format
  part_1 <- dcast.data.table(data = MP_Y_data,
                             formula = SampleID + ReplicateID ~ MP_Y,
                             value.var = "MP_A",
                             fun.aggregate = function(x) x[1])
  part_2 <- dcast.data.table(data = MP_X_data,
                             formula = SampleID + ReplicateID ~ MP_X,
                             value.var = "MP_B",
                             fun.aggregate = function(x) x[1])

  # Get potential extra information hidden in part_2
  extra_columns_in_part_2 <- setdiff(names(part_2), names(part_1))

  # If there is extra information in part_2 not part of part_1, include it.
  if (length(extra_columns_in_part_2) > 0) {
    additional_information_from_part_2 <- part_2[, c("SampleID",
                                                     "ReplicateID",
                                                     extra_columns_in_part_2),
                                                 with = FALSE]
    output <- merge(x = part_1,
                    y = additional_information_from_part_2,
                    by = c("SampleID", "ReplicateID"),
                    sort = FALSE)
  }
  else {
    output <- part_1
  }

  if (is.null(output)) {
    stop(
      "Could not revert data from comparison-wise format to original format."
    )
  }

  return(output)

}
