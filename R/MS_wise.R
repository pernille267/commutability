#' MS_wise
#'
#' @param data \code{list} or \code{data.table} including \code{SampleID}, \code{ReplicateID}, and at least two IVD-MD measurement result columns
#'
#' @description This function processes the provided \code{data}, grouping measurements based on every unique combination of the IVD-MD measurement columns. Within each IVD-MD combination, measurements are further grouped by \code{SampleID} and \code{ReplicateID}.
#'
#' @details To ensure optimal compatibility with other functions in this package, it is recommended that the IVD-MD measurement columns in \code{data} are sorted in alphabetical order. This assists in consistently maintaining the 'comparison' column of the output for both clinical sample (CS) data and external quality assessment material (EQAM) data.
#' However, take note that the \code{repair_data()} function automatically sorts the IVD-MD measurement columns. As such, it is a good practice to preprocess your CS data or EQAM data through \code{repair_data()} prior to using this function.
#'
#' @return Returns a \code{data.table} that represents the long-formatted equivalent of the input \code{data}. The resulting \code{data.table} will encompass the following columns in the specified order: 'comparison', 'SampleID', 'ReplicateID', 'MP_B', and 'MP_A'.
#' @export
#'
#' @examples print(1)

MS_wise <- function(data){


  if(!is.data.table(data)){
    if(is.data.frame(data) | is.list(data)){
      setDT(data)
    }
    else{
      stop("Invalid input class for 'data'. Your input class: '", class(data)[1], "'.
            \nExpected input classes are data.table, list, or data frame.
            \nCannot proceed with transformation from wide-formatted to long-formatted data for the provided input class.
            \nPlease revise your 'data' input and try again.")
    }
  }

  already_MS_wise <- any("comparison" == names(data))

  if(isTRUE(already_MS_wise)){
    warning(sprintf("The input 'data' appears to already be in long format. This was determined because the 'comparison' column was found in 'data'. The 'comparison' column is the %d. column. Consider checking your data to ensure it is in the expected format.",
                    which(names(data) == "comparison")), immediate. = TRUE)
    return(data)
  }



  comparison <- NULL
  id_cols = c("SampleID", "ReplicateID")
  data_contents <- names(data)
  id_cnds <- stri_replace_all(str = data_contents, regex = "[:space:]", replacement = "")
  id_cnds <- stri_replace_all(str = id_cnds, regex = "[:punct:]", replacement = "")
  id_cnds <- tolower(id_cnds)
  found_SampleID <- FALSE
  found_ReplicateID <- FALSE
  word_book <- typo_suggestions()
  valid_SampleIDs <- word_book$SampleID
  valid_ReplicateIDs <- word_book$ReplicateID
  if(any(id_cnds %in% valid_SampleIDs)){
    match_id <- which(id_cnds %in% valid_SampleIDs)[1]
    found_SampleID <- TRUE
    names(data)[match_id] <- "SampleID"
  }
  if(any(id_cnds %in% valid_ReplicateIDs)){
    match_id <- which(id_cnds %in% valid_ReplicateIDs)[1]
    found_ReplicateID <- TRUE
    names(data)[match_id] <- "ReplicateID"
  }

  data_contents <- names(data)

  if(all(c(found_SampleID, found_ReplicateID))){
    num_cols <- setdiff(data_contents, id_cols)
    combos <- combn(num_cols, 2L, simplify = TRUE)
    ms_pairs <- apply(combos, 2, paste, collapse = " - ")
    out <- melt(data, measure.vars = list(combos[1L, ], combos[2L, ]), value.name = c("MP_A", "MP_B"), variable.name = "comparison")
    out[, comparison := as.character(`levels<-`(comparison, ms_pairs))]
    out <- as.list(out)
    out <- data.table("comparison" = as.character(out$comparison), "SampleID" = as.character(out$SampleID), "ReplicateID" = as.character(out$ReplicateID), "MP_A" = out$MP_A, "MP_B" = out$MP_B)
    setDT(out)
    return(out)
  }
  else{
    stop(sprintf("The input 'data' must include both ID columns: '%s' and '%s'.
             \nOnly these columns were found in your 'data' input: [%s].
             \nPlease ensure both ID columns are present and try again.",
             id_cols[1], id_cols[2], paste(data_contents, collapse = ", ")))
  }

}
