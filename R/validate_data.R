#' Validate data
#'
#' @param data \code{list} or \code{data table} that is the data that we wish to validate
#'
#' @return A \code{list} of validation results
#' @export
#'
#' @examples print(1)

validate_data <- function(data){

  data_contain <- names(data)
  expected_order <- c("comparison", "SampleID", "ReplicateID", "MP_A", "MP_B")
  comparison_exists <- any("comparison" == data_contain)
  SampleID_exists <- any("SampleID" == data_contain)
  ReplicateID_exists <- any("ReplicateID" == data_contain)
  MP_A_exists <- any("MP_A" == data_contain)
  MP_B_exists <- any("MP_B" == data_contain)
  validation_results <- list("containment_test" = "passed",
                             "order_test" = "passed",
                             "type_test" = "passed",
                             "spelling_test" = "passed")

  if(!comparison_exists){
    cat("comparison does not exist", "\n", sep = "")
  }
  if(!SampleID_exists){
    cat("SampleID does not exist", "\n", sep = "")
  }
  if(!ReplicateID_exists){
    cat("ReplicateID does not exist", "\n", sep = "")
  }
  if(!MP_A_exists){
    cat("MP_A does not exist", "\n", sep = "")
  }
  if(!MP_B_exists){
    cat("MP_B does not exist", "\n", sep = "")
  }

  if(all(c(comparison_exists, SampleID_exists, ReplicateID_exists, MP_A_exists, MP_B_exists))){

    if(!all(data_contain == expected_order)){
      cat("All variables are found, but the order of names is wrong", "\n", sep = "")
      validation_results$order_test <- "failed"
    }
    if(!all(typeof(c(data$SampleID, data$ReplicateID)) == "character")){
      cat("All variables are found, but SampleID and/or ReplicateID is not of character type", "\n", sep = "")
      validation_results$type_test <- "failed"
    }
  }

  else if(!all(c(comparison_exists, SampleID_exists, ReplicateID_exists, MP_A_exists, MP_B_exists))){
    validation_results$containment_test <- "failed"
    if(length(data_contain) < 5){
      cat("Data contain misses some input", "\n", sep = "")
      validation_results$containment_test <- "failed"
    }
    cat("A spelling_check should be performed", "\n", sep = "")
    validation_results$spelling_test <- "failed"
  }
  return(validation_results)
}

