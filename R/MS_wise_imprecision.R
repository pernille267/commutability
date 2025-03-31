#' Validates \code{imprecision_data}
#'
#' @description
#' This function is used to validate and convert the \code{imprecision_data}
#' argument in \code{MS_wise_imprecision}.
#'
#' @param imprecision_data A object to be validated.
#'
#' @return Processed \code{imprecision_data}
#' @keywords internal
validate_imprecision_data_mwi <- function(imprecision_data){

  # Information about imprecision_data
  imprecision_stats <- NULL
  confidence_intervals_exist <- FALSE

  # Avoid modifying original input
  imprecision_data <- copy(imprecision_data)

  # Convert data iff possible. Error otherwise
  if (!is.data.table(imprecision_data)) {
    if (is.data.frame(imprecision_data) || is.list(imprecision_data)) {
      setDT(data)
    }
    else {
      stop(
        sprintf(
          "Invalid class '%s'. Expected data.table, data.frame, or list.",
           class(data)[1]
          )
        )

    }
  }

  # Allowed columns in imprecision data

  # All relevant column names
  all_stats <- c("CV_A", "CV_A_lwr", "CV_A_upr",
                 "CV_B", "CV_B_lwr", "CV_B_upr",
                 "Var_A", "Var_A_lwr", "Var_A_upr",
                 "Var_B", "Var_B_lwr", "Var_B_upr")

  # All columns corresponding with CV estimates
  all_cv_stats <- c("CV_A", "CV_A_lwr", "CV_A_upr",
                    "CV_B", "CV_B_lwr", "CV_B_upr")

  # All columns corresponding with Variance estimates
  all_var_stats <- c("Var_A", "Var_A_lwr", "Var_A_upr",
                     "Var_B", "Var_B_lwr", "Var_B_upr")

  # All estimates, but confidence intervals are missing
  all_stats_wo_ci <- c("CV_A", "CV_B", "Var_A", "Var_B")

  # All CV estimates, but confidence intervals are missing
  all_cv_stats_w0_ci <- c("CV_A", "CV_B")

  # All Variance estimates, but confidence intervals are missing
  all_var_stats_w0_ci <- c("Var_A", "Var_B")

  if (all(all_stats %in% names(imprecision_data))) {
    imprecision_stats <- all_stats
    confidence_intervals_exist <- TRUE
  }
  else if (all(all_cv_stats %in% names(imprecision_data))) {
    imprecision_stats <- all_cv_stats
    confidence_intervals_exist <- TRUE
  }
  else if (all(all_var_stats %in% names(imprecision_data))) {
    imprecision_stats <- all_var_stats
    confidence_intervals_exist <- TRUE
  }
  else if (all(all_stats_wo_ci %in% names(imprecision_data))) {
    imprecision_stats <- all_stats_wo_ci
  }
  else if (all(all_cv_stats_w0_ci %in% names(imprecision_data))) {
    imprecision_stats <- all_cv_stats_w0_ci
  }
  else if (all(all_var_stats_w0_ci %in% names(imprecision_data))) {
    imprecision_stats <- all_var_stats_w0_ci
  }
  else{
    stop(
      "Expected imprecision statistics estimates not found in imprecison_data.",
      "Make sure imprecision_data at least contains CV_A, CV_B, Var_A and Var_B."
    )
  }

  # Check if 'comparison' exists in imprecision_data
  if (!any("comparison" == names(imprecision_data))) {
    stop(
      "The mandatory column 'comparison' was not found in imprecision data."
    )
  }

  # Check if 'comparison' is character and of correct format
  if (is.character(imprecision_data$comparison)) {
    if(!all(stri_detect_fixed(str = imprecision_data$comparison,
                              pattern = " - "))) {
      stop(
        "The 'comparison' column does not seem to have the ' - ' seperator.",
        " Ensure that 'comparison' is formatted in this way: 'MP_A - MP_B'."
      )
    }
  }

  return(list(imprecision_data = imprecision_data,
              imprecision_stats = imprecision_stats,
              confidence_intervals_exist = confidence_intervals_exist))
}

#' Processes the Raw Output From \code{MS_wise_imprecision()}
#'
#' @description
#' This function is used to convert the raw output from
#' \code{MS_wise_imprecision()} to the desired output.
#'
#' @param raw_output A object to be processed.
#' @param imprecision_stats_names The names found in \code{raw_output}.
#' @param mode A \code{character} string. The desired output structure.
#' @param percent A \code{logical} value. If \code{TRUE}, coefficient of
#'                variations are presented as percentages.
#' @param variance A \code{logical} value. If \code{FALSE}, standard deviations
#'                 are used instead.
#' @param rounding An \code{integer}. The number of decimals in the output.
#'
#' @return Processed \code{raw_output}
#' @keywords internal
process_output_mwi <- function(raw_output, imprecision_stats_names, mode, percent, variance, rounding = 4L){

  # Binding global variables
  output <- NULL
  output_CV <- NULL
  output_Var <- NULL
  CV <- NULL
  CV_lwr <- NULL
  CV_upr <- NULL
  SD <- NULL
  SD_lwr <- NULL
  SD_upr <- NULL
  Var <- NULL
  Var_lwr <- NULL
  Var_upr <- NULL

  # Avoid modifying raw_output
  raw_output <- copy(raw_output)

  # Helper function for making format pe (lwr, upr)
  cc <- function(pe, lwr, upr){
    cc_out <- paste0(
      format(
        x = pe,
        nsmall = rounding,
        digits = rounding
      ),
      " (",
      format(
        x = lwr,
        nsmall = rounding,
        digits = rounding
      ),
      ", ",
      format(
        x = upr,
        nsmall = rounding,
        digits = rounding
      ),
      ")"
    )
    return(cc_out)
  }

  # Extract CV names
  cv_stats_names <- imprecision_stats_names[grep("CV",
                                                 imprecision_stats_names,
                                                 fixed = TRUE)]

  # Extract Variance names
  var_stats_names <- imprecision_stats_names[grep("Var",
                                                  imprecision_stats_names,
                                                  fixed = TRUE)]

  # Get Standard Deviation names
  sd_stats_names <- stri_replace_all_fixed(str = var_stats_names,
                                           pattern = "Var",
                                           replacement = "SD")

  # Modify CV results if desired
  if(length(cv_stats_names) > 0) {
    multiplier <- if (percent) {100} else {1}
    output_CV <- raw_output[, lapply(X = .SD,
                                     FUN = function(cv) {
                                       round(cv * multiplier, rounding)
                                     }),
                            .SDcols = cv_stats_names,
                            by = "MP"]

    if(mode == "visual" && length(cv_stats_names) == 3){
      output_CV <- output_CV[, list("CV (lwr, upr)" = cc(CV, CV_lwr, CV_upr)),
                             by = "MP"]
    }
  }

  # Modify Variance results if desired
  if(length(var_stats_names) > 0) {
    exponent <- if (variance) {1} else {0.5}
    output_Var <- raw_output[, lapply(X = .SD,
                                      FUN = function(variance) {
                                        round(variance ** exponent, rounding)
                                      }),
                             .SDcols = var_stats_names,
                             by = "MP"]
    if(!variance){
      setnames(x = output_Var,
               old = var_stats_names,
               new = sd_stats_names,
               skip_absent = TRUE)
    }

    if(mode == "visual" && length(var_stats_names) == 3){
      if(variance){
        output_Var <- output_Var[, list("Var (lwr, upr)" = cc(Var, Var_lwr, Var_upr)),
                                 by = "MP"]
      }
      else {
        output_Var <- output_Var[, list("SD (lwr, upr)" = cc(SD, SD_lwr, SD_upr)),
                                 by = "MP"]
      }

    }


  }

  if ((!is.null(output_CV)) & (!is.null(output_Var))) {
    output <- merge.data.table(x = output_CV,
                                      y = output_Var,
                                      by = "MP",
                                      sort = FALSE)
  }

  else if ((!is.null(output_CV)) & is.null(output_Var)) {
    output <- output_CV
  }

  else if (is.null(output_CV) & (!is.null(output_Var))) {
    output <- output_Var
  }

  return(output)

}



#' @title
#' Get IVD-MD-wise Imprecison Estimates
#'
#' @param imprecision_data A \code{list}, \code{data.table} or
#'                         \code{data.frame}. Should contain imprecision data
#'                         generated from \code{estimate_imprecision_data()}.
#' @param mode A \code{character} string. The type of output structure:
#'             \itemize{
#'                \item \code{visual: } Creates presentable IVD-MD-wise
#'                       repeatability imprecision estimates, that neatly
#'                       combines IVD-wise imprecision estimates into a maximum
#'                       of three columns. The first column is of course
#'                       \code{IVD-MD}, and the other two are CVs and SDs with
#'                       corresponding bootstrap confidence intervals.
#'                \item \code{exact: } Creates IVD-wise imprecision estimates
#'                       that could be used for further calculations if needed.
#'                       Includes a minimum of four columns (and a maxium of 7),
#'                       where the latter six rows are the imprecision estimates
#'                       for each IVD-MD.
#'             }
#' @param percent A \code{logical} value. If \code{TRUE}, coefficient of
#'                variation estimates are presented as percentages instead of
#'                decimal values.
#' @param variance A \code{logical} value. If \code{FALSE} (default), standard
#'                 deviation estimates are returned instead of variance
#'                 estimates.
#' @param rounding An \code{integer}. The number of decimals to include in the
#'                 \code{numeric} results. Defaults to \code{4L}.
#'
#' @description
#' Extract IVD-MD-wise repeatability imprecision estimates from the output of
#' \code{estimate_imprecision_data()}.
#'
#' @details
#' The output from \code{estimate_imprecision_data()} is seldom informative
#' if one wants to examine the IVD-MD repeatability attributes directly. This
#' function attempts to extract the unique IVD-MD specific estimates from this
#' output and pack it neatly into a \code{data.table} object.
#'
#' The output structure depends on \code{mode}. If \code{'visual'}, the output
#' will be a \code{data.table} that present the information using as few
#' columns as possible. That is, point estimates and corresponding estimated
#' confidence intervals are packed into one row instead of three seperate rows.
#' The packed columns with be \code{character} vectors with format
#' [\code{estimate}] (\code{lwr}, \code{upr}). This \code{mode} is recommended
#' if one desires to present the output in an informative way. Otherwise, if
#' \code{'exact'}, each estimate has its own column. This \code{mode} is
#' recommended if one desires to do further calculations.
#'
#' @return
#' A \code{data.table} with \code{length(unique(comparison))} rows. Contains
#' repeatability imprecision estimates for each IVD-MD. Bootstrap confidence
#' intervals are included.
#'
#' @export
#'
#' @examples
#' # Required packages
#' library(smooth.commutability)
#' library(data.table)
#'
#' # Reproducibility
#' set.seed(99)
#'
#' # Get example data
#' test_cs_data <- crp_cs_data
#'
#' # Estimate imprecision data with BCa confidence intervals
#' test_impr_data <- estimate_imprecision_data(data = test_cs_data,
#'                                             B = 200,
#'                                             type = "BCa",
#'                                             level = 0.95)
#'
#' # Get illustrative IVD-MD-wise imprecision estimates table
#' print(MS_wise_imprecision(test_impr_data, "visual", TRUE, FALSE, 2L))
#'
#' # Get IVD-MD-wise imprecision estimates table that can be used further
#' print(MS_wise_imprecision(test_impr_data, "exact", FALSE, TRUE, 3L))
#'
MS_wise_imprecision <- function(imprecision_data,
                                mode = c("visual", "exact"),
                                percent = FALSE,
                                variance = FALSE,
                                rounding = 4L){

  # Bind global variables
  comparison <- MP_A <- MP_B <- NULL

  # Extract mode
  if(length(mode) > 1){
    mode <- mode[1]
  }

  # Validate imprecision_data
  imprecision_data_validation <- validate_imprecision_data_mwi(
    imprecision_data = imprecision_data
  )

  # Extract fixed imprecision_data
  imprecision_data <- imprecision_data_validation$imprecision_data

  # Relevant stats
  imprecision_stats <- imprecision_data_validation$imprecision_stats

  # Split comparison vector
  imprecision_data[, `:=`(MP_A = stri_split(comparison, fixed = " - ")[[1]][1],
                          MP_B = stri_split(comparison, fixed = " - ")[[1]][2]),
                   by = .I]

  # Seperate '_A' stats and '_B' stats
  A_imprecision_stats <- imprecision_stats[grep("_A", imprecision_stats, fixed = TRUE)]
  B_imprecision_stats <- imprecision_stats[grep("_B", imprecision_stats, fixed = TRUE)]

  # get '_A' IVD-MD-wise stats
  out_A <- imprecision_data[, lapply(X = .SD,
                                     FUN = function(`IVD-MD 2`) {
                                       mean(`IVD-MD 2`, na.rm = TRUE)
                                     }),
                            .SDcols = A_imprecision_stats,
                            by = list("MP" = MP_A)]

  # get '_B' IVD-MD-wise stats
  out_B <- imprecision_data[, lapply(X = .SD,
                                     FUN = function(`IVD-MD 1`) {
                                       mean(`IVD-MD 1`, na.rm = TRUE)
                                     }),
                            .SDcols = B_imprecision_stats,
                            by = list("MP" = MP_B)]

  # Get general names so that we can combine out_A and out_B
  imprecision_stats_new_names <- stri_replace_all_fixed(
    str = A_imprecision_stats,
    pattern = "_A",
    replacement = ""
  )

  # Assign general names
  setnames(
    x = out_A,
    old = names(out_A),
    new = c("MP", imprecision_stats_new_names),
    skip_absent = TRUE
  )

  setnames(
    x = out_B,
    old = names(out_B),
    new = c("MP", imprecision_stats_new_names),
    skip_absent = TRUE
  )

  # Combine out_A and out_B
  out <- rbind(out_A, out_B)

  # Aggregate stats if duplicate
  out <- out[, lapply(X = .SD,
                      FUN = function(`IVD-MD`) {
                        mean(`IVD-MD`, na.rm = TRUE)
                      }),
             .SDcols = imprecision_stats_new_names,
             by = "MP"]


  # Modify output to the user's liking
  return(process_output_mwi(out,
                            imprecision_stats_new_names,
                            mode,
                            percent,
                            variance,
                            rounding))


}
