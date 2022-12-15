
#' Run a complete outlier analysis based on CS data
#'
#' @param data \code{list}, \code{data table} or \code{data frame} - Clinical sample data (CS data) for the commutability assessment experiment
#' @param type \code{character} - Which type of outlier assessment test should be applied to the input data? Possible choices are \code{'burnett'} (default), \code{'qrange'}, \code{'3sd'} and \code{'5cv'}.
#' @param where \code{character} - Where should the function look for outliers? Possible values are \code{'replicates'} (default) and \code{'samples'}. The former searches for outliers within replicated measurements, whereas the latter searces for outliers between samples.
#' @param what \code{character} - What transformation should be applied before looking for outliers? Possible choices are \code{'raw'} (default), \code{'rd'}, \code{'ad'} and \code{'ld'}. The three latter choices are shorthand for relative difference, absolute difference and log difference, respectively.
#' @param simplify \code{logical} - Should the output be attempted to be simplified? If \code{TRUE}, the output will be a \code{data.table} object with 'comparison' as the ID column. In contrast, if \code{FALSE}, a list is returned.
#' @param silence \code{integer} - Should verbose and debugging messages be included in output? Set \code{silence > 0L} to suppress such messages.
#' @param testing \code{unknown} - Only for maintainer - Should the function provide test appropriate outputs. Default is \code{FALSE}.
#'
#' @return a \code{data.table} object that lists the outlier analysis results. The dimensions of the output depends on \code{type}, \code{where} and \code{what}.
#' @export
#'
#' @examples print(1)

do_outlier_analysis <- function(data, type = c("burnett", "qrange", "3sd", "5cv", "iqr"), where = c("replicates", "samples"), what = c("raw", "rd", "ad", "ld"), simplify = FALSE, silence = 1L, testing = FALSE){

  MP_A <- MP_B <- SampleID <- VAR <- NULL
  raw_type <- type
  raw_where <- where
  raw_what <- what
  type <- type[1]
  where <- where[1]
  what <- what[1]

  if(!is.data.table(x = data)){
    if(is.data.frame(x = data)){
      data <- as.data.table(data)
    }
    else if(is.list(x = data)){
      setDT(data)
    }
    else{
      stop("data is not a data.table, data.frame or list, but rather '", class(data), "' which is not accepted.", "\n",
           "Note that only data.table, data.frame or list is accepted for 'data'!")
    }
  }

  if(testing == "structure"){
    return(data)
  }

  if(sum(is.na(data)) > 0){
    data <- na.omit(data)
  }

  if(testing == "NA"){
    return(data)
  }

  # CHECKING TYPE
  if(is.na(type) |> isTRUE()){
    if(silence < 1L){
      # Message to console
      message("type = '", raw_type, "' is not accepted. type = 'burnett' is used instead.", "\n",
              "For the future, note that only 'burnett', 'qrange', '3sd' and '5cv' are valid inputs!", "\n", sep = "")
    }
    type <- "burnett"
  }
  else if(is.null(type) |> isTRUE()){
    if(silence < 1L){
      # Message to console
      message("type = 'NULL' is not accepted. type = 'burnett' is used instead.", "\n",
              "For the future, note that only 'burnett', 'qrange', '3sd' and '5cv' are valid inputs!", "\n", sep = "")
    }
    type <- "burnett"
  }
  else if(!any(type == c("burnett", "qrange", "3sd", "5cv", "iqr"))){
    if(silence < 1L){
      # Message to console
      message("type = '", raw_type, "' is not accepted. type = 'burnett' is used instead.", "\n",
              "For the future, note that only 'burnett', 'qrange', '3sd' and '5cv' are valid inputs!", "\n", sep = "")
    }
    type <- "burnett"
  }
  rm("raw_type")
  # CHECKING WHERE
  if(is.na(where) |> isTRUE()){
    if(silence < 1L){
      # Message to console
      message("where = '", raw_where, "' is not accepted. where = 'replicates' is used instead.", "\n",
              "For the future, note that only 'replicates' and 'samples' are valid inputs!", "\n", sep = "")
    }
    where <- "replicates"
  }
  else if(is.null(where) |> isTRUE()){
    if(silence < 1L){
      # Message to console
      message("where = 'NULL' is not accepted. where = 'replicates' is used instead.", "\n",
              "For the future, note that only 'replicates' and 'samples' are valid inputs!", "\n", sep = "")
    }
    where <- "replicates"
  }
  else if(!any(where == c("replicates", "samples"))){
    if(silence < 1L){
      # Message to console
      message("where = '", raw_where, "' is not accepted. where = 'replicates' is used instead.", "\n",
              "For the future, note that only 'replicates' and 'samples' are valid inputs!", "\n", sep = "")
    }
    where <- "replicates"
  }
  rm("raw_where")

  # CHECKING WHAT
  if(is.na(what) |> isTRUE()){
    if(silence < 1L){
      # Message to console
      message("what = '", raw_what, "' is not accepted. what = 'raw' is used instead.", "\n",
              "For the future, note that only 'raw', 'rd', 'ad' and 'ld' are valid inputs", "\n", sep = "")
    }
    what <- "replicates"
  }
  else if(is.null(what) |> isTRUE()){
    if(silence < 1L){
      # Message to console
      message("what = 'NULL' is not accepted. what = 'raw' is used instead.", "\n",
              "For the future, note that only 'raw', 'rd', 'ad' and 'ld' are valid inputs", "\n", sep = "")
    }
    what <- "replicates"
  }

  else if(!any(what == c("raw", "rd", "ad", "ld"))){
    if(silence < 1L){
      # Message to console
      message("what = '", raw_what, "' is not accepted. what = 'raw' is used instead.", "\n",
              "For the future, note that only 'raw', 'rd', 'ad' and 'ld' are valid inputs", "\n", sep = "")
    }
  }

  rm("raw_what")


  if(!any("comparison" == names(data))){

    # Debugging
    if(silence < 0L){
      cat("Debugging: These columns exists in input data:", "\n",
          names(data)[1:(length(names(data)) - 1)]," and ", names(data)[length(names(data))], "\n",
          "If 'comparison' is part of the listed names above, there is something very wrong with this function!",
          sep = "")
    }

    # Abort message to console
    stop("'comparison' was not found in data. 'comparison' must be part of the input data.", "\n",
         "Are you sure your data is on long format?")
  }

  if(!any("ReplicateID" == names(data))){

    # Debugging
    if(silence < 0L){
      cat("Debugging: These columns exists in input data:", "\n",
          names(data)[1:(length(names(data)) - 1)]," and ", names(data)[length(names(data))], "\n",
          "If 'ReplicateID' is part of the listed names above, there is something very wrong with this function!",
          sep = "")
    }

    # Abort message to console
    stop("'ReplicateID' was not found in data. 'ReplicateID' must be part of the input data.", "\n",
         "Are you sure your data contains all replicated measurements?")
  }

  if(!any("SampleID" == names(data))){

    # Debugging
    if(silence < 0L){
      cat("Debugging: These columns exists in input data:", "\n",
          names(data)[1:(length(names(data)) - 1)]," and ", names(data)[length(names(data))], "\n",
          "If 'SampleID' is part of the listed names above, there is something very wrong with this function!",
          sep = "")
    }

    # Abort message to console
    stop("'SampleID' was not found in data. 'SampleID' must be part of the input data.", "\n",
         "Are you sure your data contains IDs for your CSs' measurements?")
  }

  data_list <- split(x = data, by = "comparison", keep.by = FALSE)
  out_list <- data_list

  if(type == "qrange"){

    impr_list <- lapply(data_list, function(x) global_precision_estimates(data = x))

    if(where == "replicates"){
      if(isTRUE(testing)){
        return(list("type" = "qrange", "where" = "replicates"))
      }
      for(i in 1:length(data_list)){
        n <- data_list[[i]]$SampleID |> unique() |> length()
        R <- tapply(X = data_list[[i]]$ReplicateID,
                    INDEX = data_list[[i]]$SampleID |> as.factor(),
                    FUN = function(x) unique(x) |> length()) |> median() |> ceiling()

        if(what == "raw"){
          out_list[[i]] <- data_list[[i]][, list(outlier_A = diff(range(MP_A)) > qtukey(p = 0.99, nmeans = length(MP_A), df = n * (R - 1)) * sqrt(impr_list[[i]]$Var_A),
                                                 outlier_B = diff(range(MP_B)) > qtukey(p = 0.99, nmeans = length(MP_B), df = n * (R - 1)) * sqrt(impr_list[[i]]$Var_B)), by = list(SampleID)]
          out_list[[i]]$exclude <- ifelse(test = out_list[[i]]$outlier_A | out_list[[i]]$outlier_B,
                                          yes = "yes",
                                          no = "no")
        }
        else if(what == "rd"){
          SD <- out_list[[i]][, list(VAR = var(100 - 100 * MP_A / MP_B)), by = list(SampleID)][, list(SD = mean(VAR) |> sqrt())]$SD
          out_list[[i]] <- data_list[[i]][, list(outlier_RD = diff(range(100 - 100 * MP_A / MP_B)) > qtukey(p = 0.99, nmeans = length(MP_A), df = n * (R - 1)) * SD), by = list(SampleID)]
          out_list[[i]]$exclude <- ifelse(test = out_list[[i]]$outlier_RD,
                                          yes = "yes",
                                          no = "no")
        }
        else if(what == "ad"){
          SD <- out_list[[i]][, list(VAR = var(MP_A - MP_B)), by = list(SampleID)][, list(SD = mean(VAR) |> sqrt())]$SD
          out_list[[i]] <- data_list[[i]][, list(outlier_AD = diff(range(MP_A - MP_B)) > qtukey(p = 0.99, nmeans = length(MP_A), df = n * (R - 1)) * SD), by = list(SampleID)]
          out_list[[i]]$exclude <- ifelse(test = out_list[[i]]$outlier_AD,
                                          yes = "yes",
                                          no = "no")
        }
        else if(what == "ld"){
          SD <- out_list[[i]][, list(VAR = var(log(MP_A / MP_B))), by = list(SampleID)][, list(SD = mean(VAR) |> sqrt())]$SD
          out_list[[i]] <- data_list[[i]][, list(outlier_LD = diff(range(log(MP_A / MP_B))) > qtukey(p = 0.99, nmeans = length(MP_A), df = n * (R - 1)) * SD), by = list(SampleID)]
          out_list[[i]]$exclude <- ifelse(test = out_list[[i]]$outlier_LD,
                                          yes = "yes",
                                          no = "no")
        }
      }
    }

    else if(where == "samples"){
      if(isTRUE(testing)){
        return(list("type" = "qrange", "where" = "samples"))
      }
      for(i in 1:length(data_list)){
        if(silence < -1L){
          cat("Choosing where = 'samples' when type = 'qrange', will not yield sample-wise results.", "\n",
              "This is because the studentized range test consider the difference between maximum and minimum.", "\n",
              "For future Pernille: Does this make sense? Not sure (08.12.2022)", "\n", sep = "")
        }
        out_list[[i]] <- fun_of_replicates(data = out_list[[i]], silence = 1L) |> setDT()
        if(what == "raw"){
          out_list[[i]] <- out_list[[i]][, list(outlier_A = diff(range(MP_A)) > qtukey(p = 0.99, nmeans = length(MP_A), df = length(MP_A) - 2) * sd(MP_A),
                               outlier_B = diff(range(MP_B)) > qtukey(p = 0.99, nmeans = length(MP_B), df = length(MP_B) - 2) * sd(MP_B))]
          out_list[[i]]$exclude <- ifelse(test = out_list[[i]]$outlier_A | out_list[[i]]$outlier_B,
                                            yes = "At least one sample for either IVD-MD is considered an outlier",
                                            no = "No samples are considered to be outliers")
        }
        else if(what == "rd"){
          out_list[[i]] <- out_list[[i]][, list(outlier_RD = diff(range(100 - 100 * MP_A / MP_B)) > qtukey(p = 0.99, nmeans = length(MP_A), df = length(MP_A) - 2) * sd(100 - 100 * MP_A / MP_B)), ]
          out_list[[i]]$exclude <- ifelse(test = out_list[[i]]$outlier_RD,
                                          yes = "At least one sample concerning relative difference is considered an outlier",
                                          no = "No samples are considered to be outliers")
        }
        else if(what == "ad"){
          out_list[[i]] <- out_list[[i]][, list(outlier_AD = diff(range(MP_A - MP_B)) > qtukey(p = 0.99, nmeans = length(MP_A), df = length(MP_A) - 2) * sd(MP_A - MP_B)), ]
          out_list[[i]]$exclude <- ifelse(test = out_list[[i]]$outlier_AD,
                                          yes = "At least one sample concerning absolute difference is considered an outlier",
                                          no = "No samples are considered to be outliers")
        }
        else if(what == "ld"){
          out_list[[i]] <- out_list[[i]][, list(outlier_LD = diff(range(log(MP_A) - log(MP_B))) > qtukey(p = 0.99, nmeans = length(MP_A), df = length(MP_A) - 2) * sd(log(MP_A) - log(MP_B))), ]
          out_list[[i]]$exclude <- ifelse(test = out_list[[i]]$outlier_LD,
                                          yes = "At least one sample concerning log difference is considered an outlier",
                                          no = "No samples are considered to be outliers")
        }
      }
    }
  }

  else if(type == "burnett"){
    intercept <- 2.1685
    slope <- 0.2826
    impr_list <- lapply(data_list, function(x) global_precision_estimates(data = x))

    if(where == "replicates"){
      if(isTRUE(testing)){
        return(list("type" = "burnett", "where" = "replicates"))
      }
      for(i in 1:length(data_list)){
        if(what == "raw"){
          out_list[[i]] <- data_list[[i]][, list(outlier_A = any(MP_A < mean(MP_A, na.rm = TRUE) - (intercept + slope * sum(!is.na(MP_A), na.rm = TRUE)) * sqrt(impr_list[[i]]$Var_A) | MP_A > mean(MP_A, na.rm = TRUE) + (intercept + slope * sum(!is.na(MP_A))) * sqrt(impr_list[[i]]$Var_A)),
                                                 outlier_B = any(MP_B < mean(MP_B, na.rm = TRUE) - (intercept + slope * sum(!is.na(MP_B), na.rm = TRUE)) * sqrt(impr_list[[i]]$Var_B) | MP_B > mean(MP_B, na.rm = TRUE) + (intercept + slope * sum(!is.na(MP_B))) * sqrt(impr_list[[i]]$Var_B))), by = list(SampleID)]
          out_list[[i]]$exclude <- ifelse(test = out_list[[i]]$outlier_A | out_list[[i]]$outlier_B,
                                          yes = "yes",
                                          no = "no")
        }
        else if(what == "rd"){
          SD <- out_list[[i]][, list(VAR = var(100 - 100 * MP_A / MP_B)), by = list(SampleID)][, list(SD = mean(VAR) |> sqrt())]$SD
          out_list[[i]] <- data_list[[i]][, list(outlier_RD = any(100 - 100 * MP_A / MP_B < mean(100 - 100 * MP_A / MP_B, na.rm = TRUE) - (intercept + slope * sum(!is.na(MP_A), na.rm = TRUE)) * SD | 100 - 100 * MP_A / MP_B > mean(100 - 100 * MP_A / MP_B, na.rm = TRUE) + (intercept + slope * sum(!is.na(MP_A))) * SD)), by = list(SampleID)]
          out_list[[i]]$exclude <- ifelse(test = out_list[[i]]$outlier_RD,
                                          yes = "yes",
                                          no = "no")
        }
        else if(what == "ad"){
          SD <- out_list[[i]][, list(VAR = var(MP_A - MP_B)), by = list(SampleID)][, list(SD = mean(VAR) |> sqrt())]$SD
          out_list[[i]] <- data_list[[i]][, list(outlier_AD = any(MP_A - MP_B < mean(MP_A - MP_B, na.rm = TRUE) - (intercept + slope * sum(!is.na(MP_A), na.rm = TRUE)) * SD | MP_A - MP_B > mean(MP_A - MP_B, na.rm = TRUE) + (intercept + slope * sum(!is.na(MP_A))) * SD)), by = list(SampleID)]
          out_list[[i]]$exclude <- ifelse(test = out_list[[i]]$outlier_AD,
                                          yes = "yes",
                                          no = "no")
        }
        else if(what == "ld"){
          SD <- out_list[[i]][, list(VAR = var(log(MP_A / MP_B))), by = list(SampleID)][, list(SD = mean(VAR) |> sqrt())]$SD
          out_list[[i]] <- data_list[[i]][, list(outlier_LD = any(log(MP_A / MP_B) < mean(log(MP_A / MP_B), na.rm = TRUE) - (intercept + slope * sum(!is.na(MP_A), na.rm = TRUE)) * SD | log(MP_A / MP_B) > mean(log(MP_A / MP_B), na.rm = TRUE) + (intercept + slope * sum(!is.na(MP_A))) * SD)), by = list(SampleID)]
          out_list[[i]]$exclude <- ifelse(test = out_list[[i]]$outlier_LD,
                                          yes = "yes",
                                          no = "no")
        }
      }
    }

    else if(where == "samples"){
      if(isTRUE(testing)){
        return(list("type" = "burnett", "where" = "samples"))
      }
      for(i in 1:length(data_list)){
        out_list[[i]] <- fun_of_replicates(data = out_list[[i]], silence = 1L) |> setDT()
        if(what == "raw"){
          SD_A <- sd(out_list[[i]]$MP_A, na.rm = TRUE)
          AV_A <- mean(out_list[[i]]$MP_A, na.rm = TRUE)
          L_A <- length(out_list[[i]]$MP_A)
          SD_B <- sd(out_list[[i]]$MP_B, na.rm = TRUE)
          AV_B <- mean(out_list[[i]]$MP_B, na.rm = TRUE)
          L_B <- length(out_list[[i]]$MP_B)
          out_list[[i]] <- out_list[[i]][, list(outlier_A = MP_A < AV_A - (intercept + slope * log(L_A)) * SD_A | MP_A > AV_A + (intercept + slope * log(L_A)) * SD_A,
                                                outlier_B = MP_B < AV_B - (intercept + slope * log(L_B)) * SD_B | MP_B > AV_B + (intercept + slope * log(L_B)) * SD_B), by = list(SampleID)]
          out_list[[i]]$exclude <- ifelse(test = out_list[[i]]$outlier_A | out_list[[i]]$outlier_B,
                                          yes = "yes",
                                          no = "no")
        }
        else if(what == "rd"){
          RD <- 100 - 100 * out_list[[i]]$MP_A / out_list[[i]]$MP_B
          SD <- sd(RD, na.rm = TRUE)
          AV <- mean(RD, na.rm = TRUE)
          L <- length(RD)
          out_list[[i]] <- out_list[[i]][, list(outlier_RD = 100 - 100 * MP_A / MP_B < AV - (intercept + slope * log(L)) * SD | 100 - 100 * MP_A / MP_B > AV + (intercept + slope * log(L)) * SD), by = list(SampleID)]
          out_list[[i]]$exclude <- ifelse(test = out_list[[i]]$outlier_RD,
                                          yes = "yes",
                                          no = "no")

        }
        else if(what == "ad"){
          AD <- out_list[[i]]$MP_A - out_list[[i]]$MP_B
          SD <- sd(AD, na.rm = TRUE)
          AV <- mean(AD, na.rm = TRUE)
          L <- length(AD)
          out_list[[i]] <- out_list[[i]][, list(outlier_AD = MP_A - MP_B < AV - (intercept + slope * log(L)) * SD | MP_A - MP_B > AV + (intercept + slope * log(L)) * SD), by = list(SampleID)]
          out_list[[i]]$exclude <- ifelse(test = out_list[[i]]$outlier_AD,
                                          yes = "yes",
                                          no = "no")
        }

        else if(what == "ld"){
          LD <- log(out_list[[i]]$MP_A / out_list[[i]]$MP_B)
          SD <- sd(LD, na.rm = TRUE)
          AV <- mean(LD, na.rm = TRUE)
          L <- length(LD)
          out_list[[i]] <- out_list[[i]][, list(outlier_LD = log(MP_A / MP_B) < AV - (intercept + slope * log(L)) * SD | log(MP_A / MP_B) > AV + (intercept + slope * log(L)) * SD), by = list(SampleID)]
          out_list[[i]]$exclude <- ifelse(test = out_list[[i]]$outlier_LD,
                                          yes = "yes",
                                          no = "no")
        }
      }
    }
  }

  else if(type == "3sd"){
    if(where == "replicates"){
      if(isTRUE(testing)){
        return(list("type" = "3sd", "where" = "replicates"))
      }
      impr_list <- lapply(data_list, function(x) global_precision_estimates(data = x))
      for(i in 1:length(data_list)){
        if(what == "raw"){
          out_list[[i]] <- data_list[[i]][, list(outlier_A = any(MP_A < mean(MP_A, na.rm = TRUE) - 3 * sqrt(impr_list[[i]]$Var_A) | MP_A > mean(MP_A, na.rm = TRUE) + 3 * sqrt(impr_list[[i]]$Var_A)),
                                                 outlier_B = any(MP_B < mean(MP_B, na.rm = TRUE) - 3 * sqrt(impr_list[[i]]$Var_B) | MP_B > mean(MP_B, na.rm = TRUE) + 3 * sqrt(impr_list[[i]]$Var_B))), by = list(SampleID)]
          out_list[[i]]$exclude <- ifelse(test = out_list[[i]]$outlier_A | out_list[[i]]$outlier_B,
                                          yes = "yes",
                                          no = "no")
        }
        else if(what == "rd"){
          SD <- out_list[[i]][, list(VAR = var(100 - 100 * MP_A / MP_B)), by = list(SampleID)][, list(SD = mean(VAR) |> sqrt())]$SD
          out_list[[i]] <- data_list[[i]][, list(outlier_RD = any(100 - 100 * MP_A / MP_B < mean(100 - 100 * MP_A / MP_B, na.rm = TRUE) - 3 * SD | 100 - 100 * MP_A / MP_B > mean(100 - 100 * MP_A / MP_B, na.rm = TRUE) + 3 * SD)),
                                          by = list(SampleID)]
          out_list[[i]]$exclude <- ifelse(test = out_list[[i]]$outlier_RD,
                                          yes = "yes",
                                          no = "no")
        }
        else if(what == "ad"){
          SD <- out_list[[i]][, list(VAR = MP_A - MP_B), by = list(SampleID)][, list(SD = mean(VAR) |> sqrt())]$SD
          out_list[[i]] <- data_list[[i]][, list(outlier_AD = any(MP_A - MP_B < mean(MP_A - MP_B, na.rm = TRUE) - 3 * SD | MP_A - MP_B > mean(MP_A - MP_B, na.rm = TRUE) + 3 * SD)),
                                          by = list(SampleID)]
          out_list[[i]]$exclude <- ifelse(test = out_list[[i]]$outlier_AD,
                                          yes = "yes",
                                          no = "no")
        }
        else if(what == "ld"){
          SD <- out_list[[i]][, list(VAR = log(MP_A / MP_B)), by = list(SampleID)][, list(SD = mean(VAR) |> sqrt())]$SD
          out_list[[i]] <- data_list[[i]][, list(outlier_AD = any(log(MP_A / MP_B) < mean(log(MP_A / MP_B), na.rm = TRUE) - 3 * SD | log(MP_A / MP_B) > mean(log(MP_A / MP_B), na.rm = TRUE) + 3 * SD)),
                                          by = list(SampleID)]
          out_list[[i]]$exclude <- ifelse(test = out_list[[i]]$outlier_AD,
                                          yes = "yes",
                                          no = "no")
        }
      }
    }

    else if(where == "samples"){
      if(isTRUE(testing)){
        return(list("type" = "3sd", "where" = "samples"))
      }
      for(i in 1:length(data_list)){
        out_list[[i]] <- fun_of_replicates(data = out_list[[i]], silence = 1L) |> setDT()
        if(what == "raw"){
          SD_A <- sd(out_list[[i]]$MP_A, na.rm = TRUE)
          AV_A <- mean(out_list[[i]]$MP_A, na.rm = TRUE)
          SD_B <- sd(out_list[[i]]$MP_B, na.rm = TRUE)
          AV_B <- mean(out_list[[i]]$MP_B, na.rm = TRUE)
          out_list[[i]] <- out_list[[i]][, list(outlier_A = MP_A < AV_A - 3 * SD_A | MP_A > AV_A + 3 * SD_A,
                                                outlier_B = MP_B < AV_B - 3 * SD_B | MP_B > AV_B + 3 * SD_B), by = list(SampleID)]
          out_list[[i]]$exclude <- ifelse(test = out_list[[i]]$outlier_A | out_list[[i]]$outlier_B,
                                          yes = "yes",
                                          no = "no")
        }
        else if(what == "rd"){
          RD <- 100 - 100 * out_list[[i]]$MP_A / out_list[[i]]$MP_B
          SD <- sd(RD, na.rm = TRUE)
          AV <- mean(RD, na.rm = TRUE)
          out_list[[i]] <- out_list[[i]][, list(outlier_RD = 100 - 100 * MP_A / MP_B < AV - 3 * SD | 100 - 100 * MP_A / MP_B > AV + 3 * SD), by = list(SampleID)]
          out_list[[i]]$exclude <- ifelse(test = out_list[[i]]$outlier_RD,
                                          yes = "yes",
                                          no = "no")

        }
        else if(what == "ad"){
          AD <- out_list[[i]]$MP_A - out_list[[i]]$MP_B
          SD <- sd(AD, na.rm = TRUE)
          AV <- mean(AD, na.rm = TRUE)
          out_list[[i]] <- out_list[[i]][, list(outlier_AD = MP_A - MP_B < AV - 3 * SD | MP_A - MP_B > AV + 3 * SD), by = list(SampleID)]
          out_list[[i]]$exclude <- ifelse(test = out_list[[i]]$outlier_AD,
                                          yes = "yes",
                                          no = "no")
        }

        else if(what == "ld"){
          LD <- log(out_list[[i]]$MP_A / out_list[[i]]$MP_B)
          SD <- sd(AD, na.rm = TRUE)
          AV <- mean(AD, na.rm = TRUE)
          out_list[[i]] <- out_list[[i]][, list(outlier_LD = log(MP_A / MP_B) < AV - 3 * SD | log(MP_A / MP_B) > AV + 3 * SD), by = list(SampleID)]
          out_list[[i]]$exclude <- ifelse(test = out_list[[i]]$outlier_LD,
                                          yes = "yes",
                                          no = "no")
        }
      }
    }
  }

  else if(type == "5cv"){
    if(where == "replicates"){
      if(isTRUE(testing)){
        return(list("type" = "5cv", "where" = "replicates"))
      }
      for(i in 1:length(data_list)){
        if(what == "raw"){
          impr_list <- lapply(data_list, function(x) global_precision_estimates(data = x))
          out_list[[i]] <- data_list[[i]][, list(outlier_A = if(is.na(sd(MP_A, na.rm = TRUE) / mean(MP_A, na.rm = TRUE))){FALSE}else{sd(MP_A, na.rm = TRUE) / mean(MP_A, na.rm = TRUE) > 5 * impr_list[[i]]$CV_A},
                                                 outlier_B = if(is.na(sd(MP_B, na.rm = TRUE) / mean(MP_B, na.rm = TRUE))){FALSE}else{sd(MP_A, na.rm = TRUE) / mean(MP_A, na.rm = TRUE) > 5 * impr_list[[i]]$CV_B}), by = list(SampleID)]
          out_list[[i]]$exclude <- ifelse(test = out_list[[i]]$outlier_A | out_list[[i]]$outlier_B,
                                          yes = "yes",
                                          no = "no")
        }
        else if(what == "rd"){
          CV <- out_list[[i]][, list(VAR = var(abs(100 - 100 * MP_A / MP_B), na.rm = TRUE), AV = mean(abs(100 - 100 * MP_A / MP_B), na.rm = TRUE)), by = list(SampleID)][, list(CV = sqrt(mean(VAR, na.rm = TRUE)) / mean(AV, na.rm = TRUE))]$CV
          out_list[[i]] <- data_list[[i]][, list(outlier_RD = if(is.na(sd(abs(100 - 100 * MP_A / MP_B), na.rm = TRUE) / mean(abs(100 - 100 * MP_A / MP_B), na.rm = TRUE))){NA}else{sd(abs(100 - 100 * MP_A / MP_B), na.rm = TRUE) / mean(abs(100 - 100 * MP_A / MP_B), na.rm = TRUE) > 5 * CV}),
                                          by = list(SampleID)]
          out_list[[i]]$exclude <- ifelse(test = out_list[[i]]$outlier_RD,
                                          yes = "yes",
                                          no = "no")
        }
        else if(what == "ad"){
          CV <- out_list[[i]][, list(VAR = var(abs(MP_A - MP_B), na.rm = TRUE), AV = mean(abs(MP_A - MP_B), na.rm = TRUE)), by = list(SampleID)][, list(CV = sqrt(mean(VAR, na.rm = TRUE)) / mean(AV, na.rm = TRUE))]$CV
          out_list[[i]] <- data_list[[i]][, list(outlier_AD = if(is.na(sd(abs(MP_A - MP_B), na.rm = TRUE) / mean(abs(MP_A - MP_B), na.rm = TRUE))){NA}else{sd(abs(MP_A - MP_B), na.rm = TRUE) / mean(abs(MP_A - MP_B), na.rm = TRUE) > 5 * CV}),
                                          by = list(SampleID)]
          out_list[[i]]$exclude <- ifelse(test = out_list[[i]]$outlier_AD,
                                          yes = "yes",
                                          no = "no")
        }
        else if(what == "ld"){
          CV <- out_list[[i]][, list(VAR = var(log(MP_A / MP_B), na.rm = TRUE), AV = mean(log(MP_A / MP_B), na.rm = TRUE)), by = list(SampleID)][, list(CV = sqrt(mean(VAR, na.rm = TRUE)) / mean(AV, na.rm = TRUE))]$CV
          out_list[[i]] <- data_list[[i]][, list(outlier_LD = if(is.na(sd(log(MP_A / MP_B), na.rm = TRUE) / mean(log(MP_A / MP_B), na.rm = TRUE))){NA}else{sd(log(MP_A / MP_B), na.rm = TRUE) / mean(log(MP_A / MP_B), na.rm = TRUE) > 5 * CV}),
                                          by = list(SampleID)]
          out_list[[i]]$exclude <- ifelse(test = out_list[[i]]$outlier_LD,
                                          yes = "yes",
                                          no = "no")
        }
      }
    }
    else if(where == "samples"){
      if(isTRUE(testing)){
        return(list("type" = "5cv", "where" = "samples"))
      }
      for(i in 1:length(data_list)){
        out_list[[i]] <- fun_of_replicates(data = out_list[[i]], silence = 1L) |> setDT()
        if(what == "raw"){
          CV_A <- sd(out_list[[i]]$MP_A, na.rm = TRUE) / mean(out_list[[i]]$MP_A, na.rm = TRUE)
          CV_B <- sd(out_list[[i]]$MP_B, na.rm = TRUE) / mean(out_list[[i]]$MP_B, na.rm = TRUE)
          out_list[[i]] <- out_list[[i]][, list(outlier_A = sd(MP_A, na.rm = TRUE) / mean(MP_A, na.rm = TRUE) > 5 * CV_A,
                                                outlier_B = sd(MP_B, na.rm = TRUE) / mean(MP_B, na.rm = TRUE) > 5 * CV_B), by = list(SampleID)]
          out_list[[i]]$exclude <- ifelse(test = out_list[[i]]$outlier_A | out_list[[i]]$outlier_B,
                                          yes = "yes",
                                          no = "no")
        }
        else if(what == "rd"){
          RD <- 100 - 100 * out_list[[i]]$MP_A / out_list[[i]]$MP_B
          CV <- sd(RD, na.rm = TRUE) / mean(RD, na.rm = TRUE)
          out_list[[i]] <- out_list[[i]][, list(outlier_RD = sd(abs(100 - 100 * MP_A / MP_B), na.rm = TRUE) / mean(abs(100 - 100 * MP_A / MP_B), na.rm = TRUE) > 5 * CV), by = list(SampleID)]
          out_list[[i]]$exclude <- ifelse(test = out_list[[i]]$outlier_RD,
                                          yes = "yes",
                                          no = "no")

        }
        else if(what == "ad"){
          AD <- out_list[[i]]$MP_A - out_list[[i]]$MP_B
          CV <- sd(AD, na.rm = TRUE) / mean(AD, na.rm = TRUE)
          out_list[[i]] <- out_list[[i]][, list(outlier_AD = sd(abs(MP_A - MP_B), na.rm = TRUE) / mean(abs(MP_A - MP_B), na.rm = TRUE) > 5 * CV), by = list(SampleID)]
          out_list[[i]]$exclude <- ifelse(test = out_list[[i]]$outlier_AD,
                                          yes = "yes",
                                          no = "no")
        }

        else if(what == "ld"){
          LD <- log(out_list[[i]]$MP_A / out_list[[i]]$MP_B)
          CV <- sd(LD, na.rm = TRUE) / mean(LD, na.rm = TRUE)
          out_list[[i]] <- out_list[[i]][, list(outlier_LD = sd(log(MP_A / MP_B), na.rm = TRUE) / mean(log(MP_A / MP_B), na.rm = TRUE) > 5 * CV), by = list(SampleID)]
          out_list[[i]]$exclude <- ifelse(test = out_list[[i]]$outlier_LD,
                                          yes = "yes",
                                          no = "no")
        }
      }
    }
  }

  if(isTRUE(simplify)){
    return(rbindlist(l = out_list, idcol = "comparison"))
  }
  return(out_list)
}
