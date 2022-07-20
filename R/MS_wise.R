#' MS_wise
#'
#' @param data \code{list} or \code{data table} containing \code{SampleID}, \code{ReplicateID}, and measurement result columns
#'
#' @return A \code{data table} containing \code{data} on long format
#' @export
#'
#' @examples print(1)

MS_wise <- function(data){
  setDT(data)
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
    stop("The given id_cols was not found in data")
  }

}
