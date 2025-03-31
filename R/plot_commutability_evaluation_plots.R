#' @title
#' Modifies Commutability Evaluation Data Variables
#'
#' @description
#' Prepare Commutability Evaluaton (CE) data before plotting.
#'
#' @param ce_data A \code{data.table}.
#'
#' @return
#' A \code{data.table}. The \code{ce_data} processed and ready for used in
#' \code{plot_commutability_evaluation_plots()}.
#'
#' @keywords internal
modify_ce_variables_pcep <- function(ce_data) {

  if (any("dins_conclusion" == names(ce_data))) {
    # Checks if dins_conclusion is numeric
    if(is.numeric(ce_data$dins_conclusion)) {
      unique_dins_conclusion_levels <- unique(ce_data$dins_conclusion)
      # Checks if dins_conclusion have at most two unique values
      if (!length(unique_dins_conclusion_levels) <= 2) {
        stop(
          "dins_conclusion have more than two unique levels: ",
          paste(unique_dins_conclusion_levels[1:min(length(unique_dins_conclusion_levels), 5)],
                collapse = ", "),
          if (length(unique_dins_conclusion_levels) > 5) {"..."} else {"."},
          " Ensure that it have one or two unique levels and try again."
        )
      }
      ce_data$dins_conclusion <- factor(x = ce_data$dins_conclusion,
                                        levels = c("0", "1"),
                                        labels = c("acceptable",
                                                   "not acceptable"),
                                        ordered = TRUE)
    }
    else {
      stop(
        "dins_conclusion is not an integer vector, but a ",
        class(ce_data$dins_conclusion),
        "."
      )
    }
  }
  else {
    stop(
      "dins_conclusion is missing in ce_data. It is mandatory to include it."
    )
  }

  if (any("pi_inside" == names(ce_data))) {
    # Checks if pi_inside is numeric
    if (is.numeric(ce_data$pi_inside)) {
      unique_pi_inside_levels <- unique(ce_data$pi_inside)
      # Checks if pi_inside have at most two unique values
      if(!length(unique_pi_inside_levels) <= 2) {
        stop(
          "pi_inside have more than two unique levels: ",
          paste(unique_pi_inside_levels[1:min(length(unique_pi_inside_levels), 5)],
                collapse = ", "),
          if (length(unique_pi_inside_levels) > 5) {"..."} else {"."},
          " Ensure that it have one or two unique levels and try again."
        )
      }
      ce_data$pi_inside <- factor(x = ce_data$pi_inside,
                                  levels = c("1", "0"),
                                  labels = c("yes", "no"),
                                  ordered = TRUE)
    }
    else {
      stop(
        "pi_inside is not an integer vector, but a ",
        class(ce_data$pi_inside),
        "."
      )
    }
  }
  else {
    stop(
      "pi_inside is missing in ce_data. It is mandatory to include it."
    )
  }

  if (any("inside_rate" == names(ce_data))) {

    # Checks if inside_rate is numeric
    if (is.numeric(ce_data$inside_rate)) {

      # Checks if inside_rate is between 0 and 1
      if (!all(ce_data$inside_rate <= 1 & ce_data$inside_rate >= 0, na.rm = TRUE)) {
        stop(
          "Some inside_rate values are not within the interval [0, 1]."
        )
      }

      ce_data$pi_conclusion_correctness <- sapply(
        X = seq_len(nrow(ce_data)),
        FUN = function(row_id) {
          if (ce_data$pi_inside[row_id] == "yes") {
            return(ce_data$inside_rate[row_id])
          }
          else {
            return(1 - ce_data$inside_rate[row_id])
          }
        },
        simplify = TRUE
      )

    }

    else {
      stop(
        "inside_rate is not numeric, but a ",
        class(ce_data$inside_rate),
        "."
      )
    }
  }

  else{
    stop(
      "inside_rate is missing in ce_data. It is mandatory to include it."
    )
  }

  return(ce_data)

}

#' @title
#' Modifies Prediction Band Data Variables
#'
#' @description
#' Prepare Prediction Band (PB) data before plotting.
#'
#' @param pb_data A \code{data.table}.
#'
#' @return
#' A \code{data.table}. The \code{pb_data} processed and ready for used in
#' \code{plot_commutability_evaluation_plots()}.
#'
#' @keywords internal
modify_pb_variables_pcep <- function(pb_data) {

  if (any("dins_conclusion" == names(pb_data))) {
    # Checks if dins_conclusion is numeric
    if(is.numeric(pb_data$dins_conclusion)) {
      unique_dins_conclusion_levels <- unique(pb_data$dins_conclusion)
      # Checks if dins_conclusion have at most two unique values
      if (!length(unique_dins_conclusion_levels) <= 2) {
        stop(
          "dins_conclusion have more than two unique levels: ",
          paste(unique_dins_conclusion_levels[1:min(length(unique_dins_conclusion_levels), 5)],
                collapse = ", "),
          if (length(unique_dins_conclusion_levels) > 5) {"..."} else {"."},
          " Ensure that it have one or two unique levels and try again."
        )
      }
      pb_data$dins_conclusion <- factor(x = pb_data$dins_conclusion,
                                        levels = c("0", "1"),
                                        labels = c("acceptable",
                                                   "not acceptable"),
                                        ordered = TRUE)
    }
    else {
      stop(
        "dins_conclusion is not an integer vector, but a ",
        class(pb_data$dins_conclusion),
        "."
      )
    }
  }
  else {
    stop(
      "dins_conclusion is missing in pb_data. It is mandatory to include it."
    )
  }

  return(pb_data)

}

#' @title
#' Apply Plotting Theme Attributes: PCEP
#'
#' @param plot_theme A \code{character} string.
#' @param additional_arguments A \code{list}.
#'
#' @returns
#' A \code{list}. The modified \code{additional_arguments}.
#'
#' @keywords internal
apply_theme_attributes <- function(plot_theme, additional_arguments) {

  # Get clean plot_theme
  plot_theme <- switch(plot_theme,
                       "default" = "default",
                       "noklus" = "noklus",
                       "soft" = "soft",
                       "depression" = "depression",
                       "happy" = "happy",
                       "default")



  if (plot_theme == "default") {
    theme_arguments <- list(title_color = "#000000",
                            sub_title_color = "#000000",
                            comparison_fill = "#00b9e0",
                            comparison_text_color = "#000000",
                            pb_fill = "green",
                            pb_border = "#000000",
                            curve_color = "#EB5353",
                            point_fill = "#0912BC",
                            point_border = "#000000",
                            legend_fill = "#F9F9F9",
                            legend_text_color = "#000000")
  }
  else if (plot_theme == "noklus") {
    theme_arguments <- list(title_color = "#000000",
                            sub_title_color = "#000000",
                            comparison_fill = "#00b9e0",
                            comparison_text_color = "#FFFFFF",
                            pb_fill = "#28A745",
                            pb_border = "#00b9e0",
                            curve_color = "#00b9e0",
                            point_fill = "#00b9e0",
                            point_border = "#000000",
                            legend_fill = "#FFFFFF",
                            legend_text_color = "#000000")
  }
  else if (plot_theme == "soft") {
    theme_arguments <- list(title_color = "#000000",
                            sub_title_color = "#000000",
                            comparison_fill = "#FFF89A",
                            comparison_text_color = "#000000",
                            pb_fill = "#FFB2A6",
                            pb_border = "#000000",
                            curve_color = "#D18CE0",
                            point_fill = "#9ADCFF",
                            point_border = "#000000",
                            legend_fill = "#FFFFFF",
                            legend_text_color = "#000000")
  }
  else if (plot_theme == "depression") {
    theme_arguments <- list(title_color = "#000000",
                            sub_title_color = "#4C0070",
                            comparison_fill = "#000000",
                            comparison_text_color = "#FFFFFF",
                            pb_fill = "#D18CE0",
                            pb_border = "#370040",
                            curve_color = "#151515",
                            point_fill = "#4C0070",
                            point_border = "#000000",
                            legend_fill = "#F9F9F9",
                            legend_text_color = "#000000")
  }
  else if (plot_theme == "happy") {
    theme_arguments <- list(title_color = "#36AE7C",
                            sub_title_color = "#36AE7C",
                            comparison_fill = "#EB5353",
                            comparison_text_color = "#000000",
                            pb_fill = "#FFFF00",
                            pb_border = "#000000",
                            curve_color = "#151515",
                            point_fill = "#EB9F53",
                            point_border = "#EB5353",
                            legend_fill = "#F9F9F9",
                            legend_text_color = "#000000")
  }

  if(any(plot_theme == c("default", "noklus", "soft", "depression", "happy"))){
    additional_arguments$title_color <- theme_arguments$title_color
    additional_arguments$sub_title_color <- theme_arguments$sub_title_color
    additional_arguments$comparison_fill <- theme_arguments$comparison_fill
    additional_arguments$comparison_text_color <- theme_arguments$comparison_text_color
    additional_arguments$pb_fill <- theme_arguments$pb_fill
    additional_arguments$pb_border <- theme_arguments$pb_text_color
    additional_arguments$curve_color <- theme_arguments$curve_color
    additional_arguments$point_fill <- theme_arguments$point_fill
    additional_arguments$point_border <- theme_arguments$point_border
    additional_arguments$legend_fill <- theme_arguments$legend_fill
    additional_arguments$legend_text_color <- theme_arguments$legend_text_color
  }

  return(additional_arguments)

}


#' @title
#' Update Plotting Attributes: PCEP
#'
#' @param additional_arguments A \code{list}.
#'
#' @returns
#' A \code{list}. The modified \code{additional_arguments}.
#'
#' @keywords internal
update_all_attributes <- function(additional_arguments) {

  is_valid_color <- function(color) {
    valid_color <- FALSE
    if (!is.character(color)) {
      return(valid_color)
    }

    # Check if valid hex color
    valid_hex <- (stri_length(str = color) == 7) && stri_startswith(str = color,
                                                                    fixed = "#")
    # Check if color is among the ones defined in colors()
    valid_predefined <- any(color == colors())

    if (valid_hex || valid_predefined) {
      valid_color <- TRUE
    }

    return(valid_color)

  }

  # Default 'additional_arguments'
  used_main_title <- "Commutability evaluation plots for all unique IVD-MD comparisons"
  used_sub_title <- NULL
  used_x_name <- "Measurements along x-axis * - "
  used_y_name <- "Measurements along y-axis  - *"
  used_n_breaks <- 6L
  used_title_color <- "black"
  used_sub_title_color <- "black"
  used_pb_fill <- "green"
  used_pb_border <- "black"
  used_curve <- FALSE
  used_type <- "equivalence_curve"
  used_curve_color <- "gray"
  used_point_shape <- "circle"
  used_point_size <- 0.75
  used_point_fill <- "gray"
  used_point_border <- "black"
  used_comparison_fill <- "#5BCEFA"
  used_comparison_text_color <- "#000000"
  used_legend_fill <- "#F9F9F9"
  used_legend_text_color <- "#000000"
  used_hide_prediction_intervals <- FALSE

  default_additional_arguments <- list(
    "main_title" = used_main_title,
    "sub_title" = used_sub_title,
    "x_name" = used_x_name,
    "y_name" = used_y_name,
    "n_breaks" = used_n_breaks,
    "title_color" = used_title_color,
    "sub_title_color" = used_sub_title_color,
    "pb_fill" = used_pb_fill,
    "pb_border" = used_pb_border,
    "curve" = used_curve,
    "type" = used_type,
    "curve_color" = used_curve_color,
    "point_shape" = used_point_shape,
    "point_size" = used_point_size,
    "point_fill" = used_point_fill,
    "point_border" = used_point_border,
    "comparison_fill" = used_comparison_fill,
    "comparison_text_color" = used_comparison_text_color,
    "legend_fill" = used_legend_fill,
    "legend_text_color" = used_legend_text_color,
    "hide_prediction_intervals" = used_hide_prediction_intervals
  )

  # Get given arguments
  given_arguments <- names(additional_arguments)

  # main_title (main title)
  if (any("main_title" == given_arguments)) {
    main_title <- additional_arguments$main_title[1]
    if (is.null(main_title)) {
      additional_arguments$main_title <- NULL
      given_arguments <- given_arguments[-match("main_title", given_arguments)]
      used_main_title <- NULL
    }
    else if (is.character(main_title)) {
      main_title <- main_title
      if (main_title == "") {
        additional_arguments$main_title <- NULL
        given_arguments <- given_arguments[-match("main_title", given_arguments)]
        used_main_title <- NULL
      }
    }
    else{
      warning(
        "main_title is not a character string, but a ",
        class(main_title),
        ". Default main_title is used instead."
      )
      additional_arguments$main_title <- used_main_title
    }
  }

  # sub_title (sub title)
  if (any("sub_title" == given_arguments)) {
    sub_title <- additional_arguments$sub_title[1]
    if (is.null(sub_title)) {
      additional_arguments$sub_title <- NULL
      given_arguments <- given_arguments[-match("sub_title", given_arguments)]
      used_sub_title <- NULL
    }
    else if (is.character(sub_title)) {
      sub_title <- sub_title
      if (sub_title == "") {
        additional_arguments$sub_title <- NULL
        given_arguments <- given_arguments[-match("sub_title", given_arguments)]
        used_sub_title <- NULL
      }
    }
    else{
      warning(
        "sub_title is not a character string, but a ",
        class(sub_title),
        ". Default sub_title is used instead."
      )
      additional_arguments$sub_title <- used_sub_title
    }
  }

  # x_name (x-axis label)
  if (any("x_name" == given_arguments)) {
    x_name <- additional_arguments$x_name[1]
    if (!is.character(x_name)) {
      warning(
        "x_name is not a character string, but a ",
        class(x_name),
        ". Default x_name is used instead."
      )
      additional_arguments$x_name <- used_x_name
    }
  }

  # y_name (y-axis label)
  if (any("y_name" == given_arguments)) {
    y_name <- additional_arguments$y_name[1]
    if (!is.character(y_name)) {
      warning(
        "y_name is not a character string, but a ",
        class(y_name),
        ". Default y_name is used instead."
      )
      additional_arguments$y_name <- used_y_name
    }
  }

  # n_breaks (number of breaks on both axis)
  if (any("n_breaks" == given_arguments)) {
    n_breaks <- additional_arguments$n_breaks[1]
    if (!is.numeric(n_breaks)) {
      if (is.character(n_breaks)) {
        n_breaks <- tryCatch(
          expr = {
            as.numeric(n_breaks)
          },
          error = function(e) NA_real_,
          warning = function(w) NA_real_
        )
        if (is.na(n_breaks)) {
          warning(
            "n_breaks is not an integer.",
            " Default n_breaks is used instead."
          )
          n_breaks <- 6L
          additional_arguments$n_breaks <- n_breaks
        }
        n_breaks <- as.integer(round(n_breaks))

        if (n_breaks <= 2L || n_breaks >= 50L) {
          stop(
            "n_breaks is either too small or too large: ",
            n_breaks,
            ". Please choose a value between 3 and 49."
          )
        }
      }
      else {
        warning(
          "n_breaks is not an integer.",
          " Default n_breaks is used instead."
        )
        n_breaks <- 6L
        additional_arguments$n_breaks <- n_breaks
      }
    }

    n_breaks <- as.integer(round(n_breaks))

    if (n_breaks <= 2L || n_breaks >= 50L) {
      stop(
        "n_breaks is either too small or too large: ",
        n_breaks,
        ". Please choose a value between 3 and 49."
      )
    }
  }

  # Arguments that require colors as input

  # title_color (color of main title font)
  if (any("title_color" == given_arguments)) {
    if (!is_valid_color(additional_arguments$title_color)) {
      warning(
        "title_color is not a valid color: ",
        given_arguments$title_color,
        " . Default title_color is used instead."
      )
      additional_arguments$title_color <- used_title_color
    }
  }

  # sub_title_color (color of sub title font)
  if (any("sub_title_color" == given_arguments)) {
    if (!is_valid_color(additional_arguments$sub_title_color)) {
      warning(
        "sub_title_color is not a valid color: ",
        given_arguments$sub_title_color,
        " . Default sub_title_color is used instead."
      )
      additional_arguments$sub_title_color <- used_sub_title_color
    }
  }

  # pb_fill (fill color of prediction band ribbon)
  if (any("pb_fill" == given_arguments)) {
    if (!is_valid_color(additional_arguments$pb_fill)) {
      warning(
        "pb_fill is not a valid color: ",
        given_arguments$pb_fill,
        " . Default pb_fill is used instead."
      )
      additional_arguments$pb_fill <- used_pb_fill
    }
  }

  # pb_border (border color of prediction band ribbon)
  if (any("pb_border" == given_arguments)) {
    if (!is_valid_color(additional_arguments$pb_border)) {
      warning(
        "pb_border is not a valid color: ",
        given_arguments$pb_border,
        " . Default pb_border is used instead."
      )
      additional_arguments$pb_border <- used_pb_border
    }
  }

  # curve_color (color of curve)
  if (any("curve_color" == given_arguments)) {
    if (!is_valid_color(additional_arguments$curve_color)) {
      warning(
        "curve_color is not a valid color: ",
        given_arguments$curve_color,
        " . Default curve_color is used instead."
      )
      additional_arguments$curve_color <- used_curve_color
    }
  }

  # point_fill (fill color of clinical sample points)
  if (any("point_fill" == given_arguments)) {
    if (!is_valid_color(additional_arguments$point_fill)) {
      warning(
        "point_fill is not a valid color: ",
        given_arguments$point_fill,
        " . Default point_fill is used instead."
      )
      additional_arguments$point_fill <- used_point_fill
    }
  }

  # point_border (border color of clinical sample points)
  if (any("point_border" == given_arguments)) {
    if (!is_valid_color(additional_arguments$point_border)) {
      warning(
        "point_border is not a valid color: ",
        given_arguments$point_border,
        " . Default point_border is used instead."
      )
      additional_arguments$point_border <- used_point_border
    }
  }

  # comparison_fill (fill color of strip IVD-MD comparison labels)
  if (any("comparison_fill" == given_arguments)) {
    if (!is_valid_color(additional_arguments$comparison_fill)) {
      warning(
        "comparison_fill is not a valid color: ",
        given_arguments$comparison_fill,
        " . Default comparison_fill is used instead."
      )
      additional_arguments$comparison_fill <- used_comparison_fill
    }
  }

  # comparison_text_color (text color of strip IVD-MD comparison labels)
  if (any("comparison_text_color" == given_arguments)) {
    if (!is_valid_color(additional_arguments$comparison_text_color)) {
      warning(
        "comparison_text_color is not a valid color: ",
        given_arguments$comparison_text_color,
        " . Default comparison_text_color is used instead."
      )
      additional_arguments$comparison_text_color <- used_comparison_text_color
    }
  }

  # legend_fill (fill color of legend background elements)
  if (any("legend_fill" == given_arguments)) {
    if (!is_valid_color(additional_arguments$legend_fill)) {
      warning(
        "legend_fill is not a valid color: ",
        given_arguments$legend_fill,
        " . Default legend_fill is used instead."
      )
      additional_arguments$legend_fill <- used_legend_fill
    }
  }

  # legend_text_color (fill color of legend background elements)
  if (any("legend_text_color" == given_arguments)) {
    if (!is_valid_color(additional_arguments$legend_text_color)) {
      warning(
        "legend_text_color is not a valid color: ",
        given_arguments$legend_text_color,
        " . Default legend_text_color is used instead."
      )
      additional_arguments$legend_text_color <- used_legend_text_color
    }
  }

  updated_additional_arguments <- modifyList(x = default_additional_arguments,
                                             val = additional_arguments,
                                             keep.null = TRUE)

  return(updated_additional_arguments)


}


#' @title
#' Plot Commutability Evaluation Plots For Each Comparison
#'
#' @param cs_data A \code{data.table}, \code{list}, or \code{data.frame}. The
#'                mean-of-replicates (MOR) clinical sample data. Must contain:
#'                \itemize{
#'                  \item \code{comparison: } A \code{character} vector.
#'                        The IVD-MD comparison identifiers. Typically on the
#'                        form \code{'MP_A - MP_B'}.
#'                  \item \code{SampleID: } A \code{character} vector. The
#'                        clinical sample identifiers.
#'                  \item \code{MP_A: } A \code{numeric} vector. The
#'                        mean-of-replicates results from IVD-MD \code{MP_A}
#'                        (response).
#'                  \item \code{MP_B: } A \code{numeric} vector. The
#'                        mean-of-replicates results from IVD-MD \code{MP_B}
#'                        (predictor).
#'                }
#' @param pb_data A \code{data.table} or \code{data.frame}. The Prediction Band
#'                (PB) data. See \code{?do_commutability_evaluation()}.
#' @param ce_data A \code{data.table} or \code{data.frame}. The Commutability
#'                Evaluation (CE) data. See
#'                \code{?do_commutability_evaluation()}.
#' @param exclude_rings A \code{logical}. If \code{TRUE}, \code{inside_rate}
#'                      circles are not drawn in the plots.
#' @param exclude_cs A \code{logical}. If \code{TRUE}, clinical sample
#'                   mean-of-replicates results are not drawn in the plots.
#' @param plot_theme A \code{character} string. The desired plotting theme.
#'                   Options include:
#'                   \itemize{
#'                      \item \code{custom: } No predefined theme. Customized
#'                            theme may be defined by given arguments in
#'                            \code{additional_arguments}.
#'                      \item \code{default: } Uses default theme.
#'                      \item \code{noklus: } Uses Noklus colors.
#'                      \item \code{soft: } Uses pastel colors.
#'                      \item \code{depression: } Uses dark hues such as black,
#'                            gray, and purple.
#'                      \item \code{happy: } Uses vibrant colors such as orange,
#'                            red and yellow.
#'                   }
#' @param additional_arguments A \code{list}. Additional arguments to modify
#'                             the appearance of the output plot. The default
#'                             value is \code{NULL}, indicating that the plot
#'                             will be generated using the default settings.
#'                             Possible additional_argument entries include:
#'                             \itemize{
#'                                \item \code{main_title: } Main title of the plot
#'                                \item \code{sub_title: } Sub title of the plot
#'                                \item \code{x_name: } Title of the x-axis
#'                                \item \code{y_name: } Title of the y-axis
#'                                \item \code{n_breaks: } Number of axis-breaks
#'                                \item \code{title_color: } Text color of 'main_title'. Defaults to
#'                                      \code{'black'}.
#'                                \item \code{sub_title_color: } Text color of 'sub_title'. Defaults to
#'                                      \code{'black'}
#'                                \item \code{pb_fill:  } Fill color of prediction bands if differences in
#'                                      non-selectivity is acceptable. Defaults to \code{'green'}.
#'                                \item \code{pb_border: } Color of the border of the prediction bands.
#'                                      Defaults to \code{'black'}.
#'                                \item \code{curve: } Should a additional curve be drawn based on either
#'                                      the clinical sample measurements or something else. defaults to
#'                                      \code{FALSE}.
#'                                \item \code{curve_type: } The curve type to draw. Relvant if
#'                                      \code{curve = TRUE}. Possible choices are limited to
#'                                      \code{'equivalence_curve'}, \code{'fitted_curve'} and
#'                                      \code{'flexible_curve'}.
#'                                \item \code{curve_color: } The curve color. Relevant if
#'                                      \code{curve = TRUE}. Defaults to \code{'black'}.
#'                                \item \code{point_shape: } The desired shape of clinical sample points.
#'                                      Possible choices include: \code{'circle'} (default), \code{'square'},
#'                                      \code{'diamond'} and \code{'triangle'}.
#'                                \item \code{point_size: } A \code{double} larger than \code{0}. The size
#'                                      (in pts) of drawn clinical sample points. Default is selected by
#'                                      \code{ggplot()}.
#'                                \item \code{point_fill: } The fill color of the clinical sample points.
#'                                \item \code{point_border: } The border color of clinical sample points.
#'                                \item \code{comparison_fill: } The fill color for the IVD-MD comparison
#'                                      strip labels.
#'                                \item \code{comparison_text_color: } The text color for the IVD-MD
#'                                      comparison strip labels.
#'                                \item \code{hide_prediction_intervals: } If \code{TRUE}, pointwise
#'                                      prediction intervals for evaluated material are not drawn.
#'                                \item \code{legend_fill: } The fill color for the legend background.
#'                                      Defaults to \code{'white'}.
#'                                \item \code{legend_text_color: } The text color for the legend font.
#'                                      Defaults to \code{'black'}.
#'                              }
#'
#' @param testing A \code{logical} value. If \code{TRUE} (not recommended), the
#'                output will be in a form that allows testing of the output.
#'                Primarly used for debugging and should not be used by
#'                end-users.
#'
#' @return A \code{ggplot2} object.
#' @export
#'
#' @examples print(1)

plot_commutability_evaluation_plots <- function(cs_data,
                                                pb_data,
                                                ce_data,
                                                exclude_rings = FALSE,
                                                exclude_cs = FALSE,
                                                plot_theme = c("custom", "default", "noklus", "soft", "depression", "happy"),
                                                additional_arguments = NULL,
                                                testing = FALSE){


  # Binding global variables
  MP_A <- MP_B <- SampleID <- SampleIDID <-dins_conclusion <- NULL
  pi_conclusion_correctness <- pi_inside <- pi_lwr <- NULL
  pi_upr <- predictor <- prediction <- NULL

  # Initialize
  use_shapes <- TRUE
  shape_size_mult <- 1.5

  if (length(unique(ce_data$SampleID)) > 6) {
    shape_size_mult <- 1
    if(length(unique(ce_data$SampleID)) > 9) {
      shape_size_mult <- 1.25
    }
    use_shapes <- FALSE
    unique_SampleIDs <- unique(ce_data$SampleID)
    unique_SampleIDIDs <- seq_len(length.out = length(unique_SampleIDs))
    ce_data[, SampleIDID := unique_SampleIDIDs[match(SampleID, unique_SampleIDs)]]
  }

  # Get updated ce_data ready for plotting
  ce_data <- modify_ce_variables_pcep(ce_data = ce_data)

  # Get updated pb_data ready for plotting
  pb_data <- modify_pb_variables_pcep(pb_data = pb_data)

  # Apply theme
  additional_arguments <- apply_theme_attributes(plot_theme = plot_theme,
                                                 additional_arguments = additional_arguments)

  # Update attributes
  additional_arguments <- update_all_attributes(additional_arguments = additional_arguments)

  # Check exclude_cs argument
  if (!is.logical(exclude_cs)) {
    warning(
      "exclude_cs is expected to be either TRUE or FALSE, not ",
      exclude_cs,
      ". Uses default value (FALSE) instead."
    )
    exclude_cs <- FALSE
  }

  if (is.na(exclude_cs)) {
    warning(
      "exclude_cs is expected to be either TRUE or FALSE, not ",
      exclude_cs,
      ". Uses default value (FALSE) instead."
    )
    exclude_cs <- FALSE
  }

  # Check exclude_rings argument
  if (!is.logical(exclude_rings)) {
    warning(
      "exclude_rings is expected to be either TRUE or FALSE, not ",
      exclude_rings,
      ". Uses default value (FALSE) instead."
    )
    exclude_rings <- FALSE
  }

  if (is.na(exclude_rings)) {
    warning(
      "exclude_rings is expected to be either TRUE or FALSE, not ",
      exclude_rings,
      ". Uses default value (FALSE) instead."
    )
    exclude_rings <- FALSE
  }

  cs_point_shape <- switch(additional_arguments$point_shape,
                           "circle" = 21,
                           "square" = 22,
                           "diamond" = 23,
                           "triangle" = 24,
                           24)

  out_plot <- ggplot() +
    geom_ribbon(data = pb_data,
                mapping = aes(x = predictor,
                              ymin = pi_lwr,
                              ymax = pi_upr,
                              fill = dins_conclusion),
                alpha = 0.5,
                color = additional_arguments$pb_border,
                na.rm = TRUE,
                outline.type = "full") +
    facet_wrap(facets = . ~ comparison,
               scales = "free")


  if (additional_arguments$curve) {
    if (additional_arguments$curve_type == "equivalence_curve") {
      out_plot <- out_plot +
        geom_abline(slope = 1,
                    intercept = 0,
                    color = additional_arguments$curve_color,
                    linetype = "twodash")
    }
    else if (additional_arguments$curve_type == "fitted_curve") {
      out_plot <- out_plot +
        geom_line(mapping = aes(x = predictor,
                                y = prediction),
                  color = additional_arguments$curve_color,
                  linetype = "twodash")
    }
    else if (additional_arguments$curve_type == "smooth_curve") {
      out_plot <- out_plot +
        geom_smooth(data = cs_data,
                    mapping = aes(x = MP_B,
                                  y = MP_A),
                    color = additional_arguments$curve_color,
                    method = "loess",
                    se = FALSE,
                    na.rm = TRUE,
                    span = 0.95,
                    linetype = "twodash",
                    formula = y ~ x)
    }
  }

  if (!exclude_cs) {
    out_plot <- out_plot +
      geom_point(data = cs_data,
                 mapping = aes(x = MP_B,
                               y = MP_A),
                 shape = cs_point_shape,
                 fill = additional_arguments$point_fill,
                 size = additional_arguments$point_size,
                 color = additional_arguments$point_border,
                 na.rm = TRUE)
  }

  if (!additional_arguments$hide_prediction_intervals) {
    out_plot <- out_plot +
      geom_segment(data = ce_data,
                   mapping = aes(x = MP_B,
                                 xend = MP_B,
                                 y = pi_lwr,
                                 yend = pi_upr),
                   arrow = arrow(angle = 90, ends = "both", length = unit(x = 0.025, units = "npc")))
  }

  if (use_shapes) {
    out_plot <- out_plot +
      geom_point(data = ce_data,
                 mapping = aes(x = MP_B,
                               y = MP_A,
                               color = pi_inside,
                               shape = SampleID),
                 size = shape_size_mult * (1 + sqrt(shape_size_mult) * 0.6),
                 alpha = 0.4) +
      geom_point(data = ce_data,
                 mapping = aes(x = MP_B,
                               y = MP_A,
                               color = pi_inside,
                               shape = SampleID),
                 size = shape_size_mult * (1 + sqrt(shape_size_mult) * 0.5),
                 alpha = 0.5) +
      geom_point(data = ce_data,
                 mapping = aes(x = MP_B,
                               y = MP_A,
                               color = pi_inside,
                               shape = SampleID),
                 size = shape_size_mult * (1 + sqrt(shape_size_mult) * 0.25),
                 alpha = 0.75) +
      geom_point(data = ce_data,
                 mapping = aes(x = MP_B,
                               y = MP_A,
                               color = pi_inside,
                               shape = SampleID),
                 size = shape_size_mult * (1 + sqrt(shape_size_mult) * 0.10),
                 alpha = 0.90) +
      geom_point(data = ce_data,
                 mapping = aes(x = MP_B,
                               y = MP_A,
                               color = pi_inside,
                               shape = SampleID),
                 size = shape_size_mult) +
      geom_point(data = ce_data,
                 mapping = aes(x = MP_B,
                               y = MP_A,
                               shape = SampleID),
                 size = 0.50,
                 color = "black")
  }

  else {
    out_plot <- out_plot +
      geom_point(data = ce_data,
                 mapping = aes(x = MP_B,
                               y = MP_A,
                               color = pi_inside),
                 size = shape_size_mult * 2) +
      geom_text(data = ce_data,
                mapping = aes(x = MP_B,
                              y = MP_A,
                              label = SampleIDID),
                color = "black",
                size = 5,
                size.unit = "pt")
  }

  if (!exclude_rings) {
    out_plot <- out_plot +
      geom_point(data = ce_data,
                 mapping = aes(x = MP_B,
                               y = MP_A,
                               alpha = pi_conclusion_correctness,
                               color = pi_inside),
                 shape = 1,
                 size = shape_size_mult * (1 + sqrt(shape_size_mult) * 2),
                 show.legend = FALSE)
  }

  if (use_shapes) {
    out_plot <- out_plot +
      labs(title = additional_arguments$main_title,
           subtitle = additional_arguments$sub_title,
           color = "Inside prediction interval",
           shape = "Evaluated materials' ID",
           fill = "Differences in non-selectivity")
  }
  else {
    out_plot <- out_plot +
      labs(title = additional_arguments$main_title,
           subtitle = additional_arguments$sub_title,
           color = "Inside prediction interval",
           fill = "Differences in non-selectivity")
  }

  out_plot <- out_plot +
    scale_x_continuous(name = additional_arguments$x_name,
                       n.breaks = additional_arguments$n_breaks) +
    scale_y_continuous(name = additional_arguments$y_name,
                       n.breaks = additional_arguments$n_breaks) +
    scale_fill_manual(values = c("acceptable" = additional_arguments$pb_fill,
                                 "not acceptable" = "gray")) +
    scale_color_manual(values = c("yes" = "#1994DC",
                                  "no" = "#DC1932"))

  if (!exclude_rings) {
    out_plot <- out_plot +
      scale_alpha_binned(name = "Conclusion correctness",
                         limits = c(0, 1),
                         breaks = c(0.25, 0.50, 0.75, 0.90, 1),
                         labels = c("extremely low",
                                    "low",
                                    "medium",
                                    "high",
                                    "extremely high"),
                         range = c(0, 1))
  }

  out_plot <- out_plot +
    theme_bw() +
    theme(plot.title = element_text(face = "bold",
                                    hjust = 0.5,
                                    color = additional_arguments$title_color),
          plot.subtitle = element_text(face = "bold",
                                       hjust = 0.5,
                                       color = additional_arguments$sub_title_color),
          strip.background = element_rect(fill = additional_arguments$comparison_fill,
                                          color = "black",
                                          linewidth = 1),
          strip.text = element_text(face = "bold",
                                    color = additional_arguments$comparison_text_color),
          legend.background = element_rect(fill = additional_arguments$legend_fill,
                                           color = "black"),
          legend.key = element_rect(fill = additional_arguments$legend_fill,
                                    color = "black"),
          legend.text = element_text(color = additional_arguments$legend_text_color))


  return(out_plot)


}


