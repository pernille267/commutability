#' Transformation of EQA data
#'
#' @param data A \code{list}, \code{data table} or \code{data frame} that must contain \code{MP_A} and \code{MP_B}
#' @param transformation \code{character} - Either a string seperated by space or symbol so that we have transformation|seperator|value. For example, if the cube root is required, you e.g., write root#3 or root_3. Possible transformations are:
#' \itemize{
#'   \item{\code{log}: }{log-transformation, with values representing the base of the log-transformation. All values to be transformed must be positive}
#'   \item{\code{root}: }{root-transforomation, with values representing which root is taken, e.g., 2 for square root, 3 for cube root or other integers. All values to be transformed values must be non-negative}
#'   \item{\code{pow}: }{power-transformation, with values representing the power taken, e.g., 2 for squaring, 3 for cubing or other integers. If value < 1, all values to be transformed must be non-negative}
#'   \item{\code{inv}: }{inverse-transformation, with values representing the power of the denominator of the invserse transformation. If value < 1, all values transformed to be transformed must be non-negative}
#'   \item{\code{id}: }{indentity-transformation, which means no transformation at all}
#' }
#' @return The transformed \code{data} as a \code{data table}
#' @export
#'
#' @examples print(1)

transform_data <- function(data, transformation = "log"){
  if(!any(is.data.table(data), is.data.frame(data), is.list(data))){
    stop(paste0("'",class(data),"' class", " is not supported. data is required to be either a list, data table, or data frame."))
  }
  first_split <- stri_split_regex(str = transformation, pattern = "[:punct:]|[:blank:]|[:symbol:]", n = 2) |> unlist()
  value <- 0
  if(length(first_split) < 2){
    if(any(tolower(first_split) == c("lg","ln","log","logarithm"))){
      value <- exp(1)
      data$MP_A <- log(ifelse(data$MP_A <= 0, NA, data$MP_A), base = value)
      data$MP_B <- log(ifelse(data$MP_B <= 0, NA, data$MP_B), base = value)
    }
    else if(any(tolower(first_split) == c("sqrt","root","2ndroot","squareroot","2throot","secondroot","2root"))){
      value <- 2
      data$MP_A <- ifelse(data$MP_A < 0, NA, data$MP_A ** (1/value))
      data$MP_B <- ifelse(data$MP_B < 0, NA, data$MP_B ** (1/value))
    }
    else if(any(tolower(first_split) == c("identity","identical", "id"))){
      value <- 1
      data$MP_A <- data$MP_A ** value
      data$MP_B <- data$MP_B ** value
    }
    else if(any(tolower(first_split) == c("inverse", "inv"))){
      value <- 1
      data$MP_A <- 1/(data$MP_A ** value)
      data$MP_B <- 1/(data$MP_B ** value)
    }
    else if(any(tolower(first_split) == c("square", "2thpower", "2ndpower", "2power","2pow"))){
      value <- 2
      data$MP_A <- data$MP_A ** value
      data$MP_B <- data$MP_B ** value
    }
    else if(any(tolower(first_split) == c("cube", "3thpower", "3rdpower", "3power", "3pow"))){
      value <- 3
      data$MP_A <- data$MP_A ** value
      data$MP_B <- data$MP_B ** value
    }
    else if(any(tolower(first_split) == c("cuberoot","3rdroot","3throot","thirdroot","3root"))){
      value <- 3
      data$MP_A <- ifelse(data$MP_A < 0, NA, data$MP_A ** (1/value))
      data$MP_B <- ifelse(data$MP_B < 0, NA, data$MP_B ** (1/value))
    }
    else{
      warning(paste0("'",transformation,"'", " was not recongnized. Data is accordingly left untouched"))
      data$MP_A <- data$MP_A
      data$MP_B <- data$MP_B
    }
  }

  else if(length(first_split) == 2){
    second_split <- stri_replace_all(str = first_split[2], replacement = "", regex = "[^[:digit:].]") |> unlist()
    is_numeric <- stri_detect(str = second_split, regex = "[:digit:]")
    if(is_numeric){
      second_split <- as.numeric(second_split)
    }
    else{
      warning(paste0("'",first_split[2],"'", " was not recongnized as a number. Data is accordingly left untouched"))
      return(data)
    }
    value <- second_split
    if(any(tolower(first_split[1]) == c("lg","ln","log","logarithm"))){
      data$MP_A <- log(ifelse(data$MP_A <= 0, NA, data$MP_A), base = value)
      data$MP_B <- log(ifelse(data$MP_B <= 0, NA, data$MP_B), base = value)
    }
    else if(any(tolower(first_split[1]) == c("root","nroot"))){
      data$MP_A <- ifelse(data$MP_A < 0, NA, data$MP_A ** (1/value))
      data$MP_B <- ifelse(data$MP_B < 0, NA, data$MP_B ** (1/value))
    }
    else if(any(tolower(first_split[1]) == c("identity", "identical", "id"))){
      value <- 1
      data$MP_A <- data$MP_A ** value
      data$MP_B <- data$MP_B ** value
    }
    else if(any(tolower(first_split[1]) == c("inverse", "inv"))){
      data$MP_A <- 1/(data$MP_A ** value)
      data$MP_B <- 1/(data$MP_B ** value)
    }
    else if(any(tolower(first_split[1]) == c("pow", "power","npower","nthpower"))){
      data$MP_A <- data$MP_A ** value
      data$MP_B <- data$MP_B ** value
    }
    else{
      warning(paste0("'",transformation,"'", " was not recongnized. Data is accordingly left untouched"))
      data$MP_A <- data$MP_A
      data$MP_B <- data$MP_B
    }
  }
  else{
    warning(paste0("'",transformation,"'", " was not recongnized. Data is accordingly left untouched"))
    data$MP_A <- data$MP_A
    data$MP_B <- data$MP_B
  }
  return(data)
}


