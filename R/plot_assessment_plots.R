#' Validates and Converts The \code{method} Argument
#'
#' @description
#' This function is used to validate and convert the \code{method} argument
#' in \code{plot_assessment_plots()}.
#'
#' @param method A \code{character} string to be validated and converted.
#'
#' @return The converted \code{data}
#' @keywords internal
validate_method <- function(method) {

  if (!is.character(method)) {
    stop(
      "Expected a character for method, but got a: ",
      class(method),
      ". Ensure that method is a character and try again."
    )
  }

  if (length(method) != 1) {
    if (length(method) == 0) {
      stop(
        "Expected a character for method, but got an ",
        "empty vector. Ensure that method is a valid character string ",
        "and try agian."
      )
    }
    else if (length(method) > 1) {
      warning(
        "Expected a character string for method, but got a ",
        "character vector of length ",
        length(method),
        "L.",
        "Only the first element of method is used."
      )
      method <- method[1]
    }
  }

  if (is.na(method)) {
    stop(
      "Expected a character for method, but got an ",
      "NA value. Ensure that method is a valid character string ",
      "and try again"
    )
  }

  if (!any(method == c("ols", "clsi", "fg", "ss", "ssw"))) {
    warning(
      "Expected a method among 'ols', 'clsi', 'fg', 'ss' and 'ssw', but got: '",
      method,
      "'. Using the default ('fg') instead."
    )
    method <- "fg"
  }

  return(method)

}

#' Validates and Converts The \code{type} Argument
#'
#' @description
#' This function is used to validate and convert the \code{type} argument
#' in \code{plot_assessment_plots()}.
#'
#' @param type A \code{character} string to be validated and converted.
#'
#' @return The converted \code{data}
#' @keywords internal
validate_type <- function(type) {

  if (!is.character(type)) {
    stop(
      "Expected a character for type, but got a: ",
      class(type),
      ". Ensure that type is a character and try again."
    )
  }

  if (length(type) != 1) {
    if (length(type) == 0) {
      stop(
        "Expected a character for type, but got an ",
        "empty vector. Ensure that type is a valid character string ",
        "and try agian."
      )
    }
    else if (length(type) > 1) {
      warning(
        "Expected a character string for type, but got a ",
        "character vector of length ",
        length(type),
        "L.",
        "Only the first element of type is used."
      )
      type <- type[1]
    }
  }

  if (is.na(type)) {
    stop(
      "Expected a character for type, but got an ",
      "NA value. Ensure that type is a valid character string ",
      "and try again"
    )
  }

  if (!any(type == c("residual_plot",
                     "residual_histogram",
                     "qq_plot",
                     "sd_vs_concentration",
                     "cv_vs_concentration"))) {
    warning(
      "Unexpected plot type. Got: '",
      type,
      "'. Using the default ('residual_plot') instead. ",
      "See documentation for valid plot types."
    )
    type <- "residual_plot"
  }

  return(type)

}

#' Validates and Converts The \code{plot_theme} Argument
#'
#' @description
#' This function is used to validate and convert the \code{plot_theme} argument
#' in \code{plot_assessment_plots()}.
#'
#' @param plot_theme A \code{character} string to be validated and converted.
#'
#' @return The converted \code{data}
#' @keywords internal
validate_plot_theme <- function(plot_theme) {

  if (!is.character(plot_theme)) {
    stop(
      "Expected a character for plot_theme, but got a: ",
      class(plot_theme),
      ". Ensure that plot_theme is a character and try again."
    )
  }

  if (length(plot_theme) != 1) {
    if (length(plot_theme) == 0) {
      stop(
        "Expected a character for plot_theme, but got an ",
        "empty vector. Ensure that plot_theme is a valid character string ",
        "and try agian."
      )
    }
    else if (length(plot_theme) > 1) {
      warning(
        "Expected a character string for plot_theme, but got a ",
        "character vector of length ",
        length(plot_theme),
        "L.",
        "Only the first element of plot_theme is used."
      )
      plot_theme <- plot_theme[1]
    }
  }

  if (is.na(plot_theme)) {
    stop(
      "Expected a character for plot_theme, but got an ",
      "NA value. Ensure that plot_theme is a valid character string ",
      "and try again."
    )
  }

  if (!any(plot_theme == c("custom",
                           "default",
                           "noklus",
                           "soft",
                           "depression",
                           "happy"))) {
    warning(
      "Unexpected input for plot_theme. Got: '",
      plot_theme,
      "'. Using the default plot_theme instead. ",
      "See documentation for valid plot themes."
    )
    plot_theme <- "default"
  }

  return(plot_theme)
}

#' Applies \code{plot_theme} to \code{additional_arguments}
#'
#' @description
#' This function is used to apply the selected \code{plot_theme} to the
#' \code{additional_arguments} argument in \code{plot_assessment_plots()}.
#'
#' @param plot_theme A \code{character} string. The desired plotting theme.
#' @param additional_arguments See \code{?plot_assessment_plots()}.
#'
#' @return The original \code{additional_arguments} with the theme attributes
#'         applied to it.
#' @keywords internal
apply_theme_attributes_pap <- function(plot_theme, additional_arguments) {

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
                            strip_fill = "#00b9e0",
                            strip_text_color = "#000000",
                            histogram_fill = "#F9F9F9",
                            histogram_border = "#000000",
                            curve_color = "#EB5353",
                            point_fill = "#0912BC",
                            point_border = "#000000",
                            legend_fill = "#F9F9F9",
                            legend_text_color = "#000000")
  }
  else if (plot_theme == "noklus") {
    theme_arguments <- list(title_color = "#000000",
                            sub_title_color = "#000000",
                            strip_fill = "#00b9e0",
                            strip_text_color = "#FFFFFF",
                            histogram_fill = "#28A745",
                            histogram_border = "#000000",
                            curve_color = "#28A745",
                            point_fill = "#00b9e0",
                            point_border = "#000000",
                            legend_fill = "#FFFFFF",
                            legend_text_color = "#000000")
  }
  else if (plot_theme == "soft") {
    theme_arguments <- list(title_color = "#000000",
                            sub_title_color = "#000000",
                            strip_fill = "#FFF89A",
                            strip_text_color = "#000000",
                            histogram_fill = "#FFB2A6",
                            histogram_border = "#000000",
                            curve_color = "#D18CE0",
                            point_fill = "#9ADCFF",
                            point_border = "#000000",
                            legend_fill = "#FFFFFF",
                            legend_text_color = "#000000")
  }
  else if (plot_theme == "depression") {
    theme_arguments <- list(title_color = "#000000",
                            sub_title_color = "#4C0070",
                            strip_fill = "#000000",
                            strip_text_color = "#FFFFFF",
                            histogram_fill = "#D18CE0",
                            histogram_border = "#370040",
                            curve_color = "#151515",
                            point_fill = "#4C0070",
                            point_border = "#000000",
                            legend_fill = "#F9F9F9",
                            legend_text_color = "#000000")
  }
  else if (plot_theme == "happy") {
    theme_arguments <- list(title_color = "#36AE7C",
                            sub_title_color = "#36AE7C",
                            strip_fill = "#EB5353",
                            strip_text_color = "#000000",
                            histogram_fill = "#FFFF00",
                            histogram_border = "#000000",
                            curve_color = "#151515",
                            point_fill = "#EB9F53",
                            point_border = "#EB5353",
                            legend_fill = "#F9F9F9",
                            legend_text_color = "#000000")
  }

  if(any(plot_theme == c("default", "noklus", "soft", "depression", "happy"))){
    additional_arguments$title_color <- theme_arguments$title_color
    additional_arguments$sub_title_color <- theme_arguments$sub_title_color
    additional_arguments$strip_fill <- theme_arguments$strip_fill
    additional_arguments$strip_text_color <- theme_arguments$strip_text_color
    additional_arguments$histogram_fill <- theme_arguments$histogram_fill
    additional_arguments$histogram_border <- theme_arguments$histogram_border
    additional_arguments$curve_color <- theme_arguments$curve_color
    additional_arguments$point_fill <- theme_arguments$point_fill
    additional_arguments$point_border <- theme_arguments$point_border
    additional_arguments$legend_fill <- theme_arguments$legend_fill
    additional_arguments$legend_text_color <- theme_arguments$legend_text_color
  }

  return(additional_arguments)

}

#' Applies Custom Settings and Merge Them with Defaults of \code{additional_arguments}
#'
#' @description
#' This function is used to update the \code{additional_arguments} argument
#' based on custom choices and default choices in
#' \code{plot_assessment_plots()}.
#'
#' @param additional_arguments See \code{?plot_assessment_plots()}.
#' @param method See \code{?plot_assessment_plots()}.
#' @param type See \code{?plot_assessment_plots()}.
#'
#' @return The updated \code{additional_arguments} ready to be used.
#' @keywords internal
update_all_attributes_pap <- function(additional_arguments, method, type) {

  # Get default main labels based on method and type
  get_main_labels <- function(method, type) {

    # Get dynamic labels
    residuals_label <- paste0("Standardized (", method, ") ", "Residuals")
    fitted_label <- paste0("Fitted (", method, ") ", "Values")
    quantiles_label <- paste0("Empirical Quantiles of Standardized (",
                              method,
                              ") ",
                              "Residuals")

    default_x_name <- switch(type,
                             "residual_plot" = fitted_label,
                             "residual_histogram" = residuals_label,
                             "qq_plot" = "Theoretical N(0, 1) Quantile Values",
                             "sd_vs_concentration" = "Estimated Concentration Values",
                             "cv_vs_concentration" = "Estimated Concentration Values")

    default_y_name <- switch(type,
                             "residual_plot" = residuals_label,
                             "residual_histogram" = "Density",
                             "qq_plot" = quantiles_label,
                             "sd_vs_concentration" = "[*]",
                             "cv_vs_concentration" = "Coefficient of Variation[*]")

    default_main_title <- switch(type,
                                 "residual_plot" = "Standardized Residuals Versus Fitted Values",
                                 "residual_histogram" = "Histogram of Standardized Residuals",
                                 "qq_plot" = "QQ-Normal Plot",
                                 "sd_vs_concentration" = "[*] Versus Concentration Plot",
                                 "cv_vs_concentration" = "Coefficient of Variation Versus Concentration")

    default_sub_title <- switch(method,
                                "ols" = "Ordinary Least Squares Regression",
                                "clsi" = "Deming Regression (CLSI EP14 Variant)",
                                "fg" = "Deming Regression (Fuller & Gillard Variant)",
                                "ss" = "Smoothing Spline Regression",
                                "ssw" = "Weighted Smoothing Spline Regression")

    return(list(default_main_title = default_main_title,
                default_sub_title = default_sub_title,
                default_x_name = default_x_name,
                default_y_name = default_y_name))


  }

  # Function to check if a color is valid
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

  main_labels <- get_main_labels(method = method,
                                 type = type)

  # Default 'additional_arguments'
  used_main_title <- main_labels$default_main_title
  used_sub_title <- main_labels$default_sub_title
  used_x_name <- main_labels$default_x_name
  used_y_name <- main_labels$default_y_name
  used_n_breaks <- 6L
  used_title_color <- "black"
  used_sub_title_color <- "black"
  used_strip_fill <- "#5BCEFA"
  used_strip_text_color <- "#000000"
  used_point_shape <- "circle"
  used_point_size <- 1.25
  used_point_fill <- "gray"
  used_point_border <- "black"
  used_curve_color <- "red"
  used_curve_se <- FALSE
  used_curve_se_fill <- "#F9F9F9"
  used_curve_formula <- "y ~ x"
  used_loess_span <- 0.95
  used_histogram_fill <- "#F9F9F9"
  used_histogram_border <- "black"
  used_cv_percent <- TRUE
  used_var_instead <- FALSE

  default_additional_arguments <- list(
    "main_title" = used_main_title,
    "sub_title" = used_sub_title,
    "x_name" = used_x_name,
    "y_name" = used_y_name,
    "n_breaks" = used_n_breaks,
    "strip_fill" = used_strip_fill,
    "strip_text_color" = used_strip_text_color,
    "point_shape" = used_point_shape,
    "point_size" = used_point_size,
    "point_fill" = used_point_fill,
    "point_border" = used_point_border,
    "curve_color" = used_curve_color,
    "curve_se" = used_curve_se,
    "curve_se_fill" = used_curve_se_fill,
    "curve_formula" = used_curve_formula,
    "loess_span" = used_loess_span,
    "histogram_fill" = used_histogram_fill,
    "histogram_border" = used_histogram_border,
    "cv_percent" = used_cv_percent,
    "var_instead" = used_var_instead
  )

  # Get given arguments
  given_arguments <- names(additional_arguments)

  # Text arguments

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

  # Misc. arguments ...

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

  # loess_span (the smoothing parameter for loess modelling)
  if (any("loess_span" == given_arguments)) {
    loess_span <- additional_arguments$loess_span[1]
    if (!is.numeric(loess_span)) {
      if (is.character(loess_span)) {
        loess_span <- tryCatch(
          expr = {
            as.numeric(loess_span)
          },
          error = function(e) NA_real_,
          warning = function(w) NA_real_
        )
        if (is.na(loess_span)) {
          warning(
            "loess_span is not a double between 0 and 1.",
            " Default loess_span is used instead."
          )
          additional_arguments$loess_span <- used_loess_span
        }

        if (loess_span < 0.5 || loess_span > 2) {
          stop(
            "loess_span is either too small or too large: ",
            loess_span,
            ". Please choose a value between 0.5 and 2."
          )
        }
      }
      else {
        warning(
          "loess_span is not a double.",
          " Default loess_span is used instead."
        )
        additional_arguments$loess_span <- used_loess_span
      }
    }

    if (loess_span < 0.5 || loess_span > 2) {
      stop(
        "loess_span is either too small or too large: ",
        loess_span,
        ". Please choose a value between 0.5 and 2."
      )
    }


  }

  # cv_percent (whether cv is presented as a percentage)
  if (any("cv_percent" == given_arguments)) {
    cv_percent <- additional_arguments$cv_percent[1]
    if (!is.logical(cv_percent)) {
      if (!is.integer(cv_percent)) {
        warning(
          "cv_percent is expected to be a logical value, but got a: ",
          class(cv_percent),
          ". Default cv_percent (",
          used_cv_percent,
          ") is used instead."
        )
        cv_percent <- used_cv_percent
        additional_arguments$cv_percent <- cv_percent
      }
      else {
        cv_percent <- tryCatch(
          expr = {
            as.logical(cv_percent)
          },
          warning = function(w) used_cv_percent,
          error = function(e) used_cv_percent
        )
        additional_arguments$cv_percent <- cv_percent
      }
    }

    if(is.na(cv_percent)) {
      warning(
        "cv_percent is expected to be a non-missing logical value, but got: ",
        cv_percent,
        ". Default cv_percent (",
        used_cv_percent,
        ") is used instead."
      )
      cv_percent <- used_cv_percent
      additional_arguments$cv_percent <- cv_percent
    }
  }

  # var_instead (whether variance is used instead of standard deviation)
  if (any("var_instead" == given_arguments)) {
    var_instead <- additional_arguments$var_instead[1]
    if (!is.logical(var_instead)) {
      if (!is.integer(var_instead)) {
        warning(
          "var_instead is expected to be a logical value, but got a: ",
          class(var_instead),
          ". Default var_instead (",
          used_var_instead,
          ") is used instead."
        )
        var_instead <- used_var_instead
        additional_arguments$var_instead <- var_instead
      }
      else {
        var_instead <- tryCatch(
          expr = {
            as.logical(var_instead)
          },
          warning = function(w) used_var_instead,
          error = function(e) used_var_instead
        )
        additional_arguments$var_instead <- var_instead
      }
    }

    if(is.na(var_instead)) {
      warning(
        "var_instead is expected to be a non-missing logical value, but got: ",
        var_instead,
        ". Default var_instead (",
        used_var_instead,
        ") is used instead."
      )
      var_instead <- used_var_instead
      additional_arguments$var_instead <- var_instead
    }
  }

  # Arguments that require colors as input ...

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

  # strip_fill (fill color of strip labels)
  if (any("strip_fill" == given_arguments)) {
    if (!is_valid_color(additional_arguments$strip_fill)) {
      warning(
        "strip_fill is not a valid color: ",
        given_arguments$strip_fill,
        " . Default strip_fill is used instead."
      )
      additional_arguments$strip_fill <- used_strip_fill
    }
  }

  # strip_text_color (text color of strip labels)
  if (any("strip_text_color" == given_arguments)) {
    if (!is_valid_color(additional_arguments$strip_text_color)) {
      warning(
        "strip_text_color is not a valid color: ",
        given_arguments$strip_text_color,
        " . Default strip_text_color is used instead."
      )
      additional_arguments$strip_text_color <- used_strip_text_color
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

  # curve_se_fill (fill color of curve error band)
  if (any("curve_se_fill" == given_arguments)) {
    if (!is_valid_color(additional_arguments$curve_se_fill)) {
      warning(
        "curve_se_fill is not a valid color: ",
        given_arguments$curve_se_fill,
        " . Default curve_se_fill is used instead."
      )
      additional_arguments$curve_se_fill <- used_curve_se_fill
    }
  }

  # histogram_fill (fill color of prediction band ribbon)
  if (any("histogram_fill" == given_arguments)) {
    if (!is_valid_color(additional_arguments$histogram_fill)) {
      warning(
        "histogram_fill is not a valid color: ",
        given_arguments$histogram_fill,
        " . Default histogram_fill is used instead."
      )
      additional_arguments$histogram_fill <- used_histogram_fill
    }
  }

  # histogram_border (border color of prediction band ribbon)
  if (any("histogram_border" == given_arguments)) {
    if (!is_valid_color(additional_arguments$histogram_border)) {
      warning(
        "histogram_border is not a valid color: ",
        given_arguments$histogram_border,
        " . Default histogram_border is used instead."
      )
      additional_arguments$histogram_border <- used_histogram_border
    }
  }


  updated_additional_arguments <- modifyList(x = default_additional_arguments,
                                             val = additional_arguments,
                                             keep.null = TRUE)

  return(updated_additional_arguments)


}

#' Get Plotting Data Estimated from \code{data}
#'
#' @description
#' This function is used to calculate the assessment data used in the plots.
#'
#' @param data See \code{?plot_assessment_plots()}.
#' @param method See \code{?plot_assessment_plots()}.
#' @param type See \code{?plot_assessment_plots()}.
#' @param cv_percent A \code{logical} value. If \code{TRUE},
#'                   coefficients of variation is presented as percentages.
#' @param var_instead A \code{logical} value. If \code{TRUE},
#'                    variances are used instead of standard deviations.
#'
#' @return
#' A \code{list} of length \code{2L}:
#' \itemize{
#'    \item \code{assessment_data: } A \code{data.table}. Contains the plotting
#'          data for the desired \code{type}.
#'    \item \code{supplement_assessment_information: } A \code{data.table} or
#'          \code{list} that contains additional information that could be
#'          utilized in the plotting.
#' }
#' @keywords internal
get_assessment_data <- function(data,
                                method,
                                type,
                                cv_percent = TRUE,
                                var_instead = FALSE) {

  # Bind global variables
  comparison <- comparisonID <- NULL

  # Avoid modifying original data
  data <- copy(data)

  # For error- and warning-message purposes
  data[, comparisonID := comparison]

  get_comparison_residual_data <- function(data, method) {

    # Avoid modifying the original data
    comp <- copy(data)

    # Linear models
    if (any(method == c("ols", "clsi", "fg"))) {

      # Required stats
      impr_data <- global_precision_estimates(comp)
      mor_data <- fun_of_replicates(comp)

      # Attempt to calculate residual data for IVD-MD comparison
      comp_residual_data <- tryCatch(
        expr = {
          residuals_eqa(
            data = mor_data,
            imprecision_estimates = impr_data,
            method = method,
            studentize = TRUE,
            unit_sd = FALSE,
            invalid_NA = FALSE
          )
        },
        warning = function(w) NULL,
        error = function(e) NULL
      )

      if (is.null(comp_residual_data)) {
        stop(
          "Could not calculate the (",
          method,
          ") ",
          "residual data for the ",
          comp$comparisonID[1],
          " IVD-MD comparison."
        )
      }
    }

    # Smoothing Splines
    else if (any(method == c("ss", "ssw"))) {

      # Get weighting
      weighting <- switch(method,
                          "ss" = 1,
                          "ssw" = "estimate")

      # Required stats
      mor_data <- fun_of_replicates(comp)
      ss_fit <- tryCatch(
        expr = {
          smoothing_spline(data = mor_data,
                           weights = weighting,
                           df = NULL,
                           lambda = NULL,
                           df_max = 7.5,
                           attempt_fast = FALSE,
                           na_rm = TRUE)
        },
        error = NULL
      )

      if (is.null(ss_fit)) {
        stop(
          "Could not calculate the (",
          method,
          ") ",
          "residual data for the ",
          comp$comparisonID[1],
          " IVD-MD comparison."
        )
      }

      comp_residual_data <- list(
        "fitted" = ss_fit$fitted,
        "residuals" = ss_fit$residuals * sqrt(ss_fit$weights) /
          sqrt(ss_fit$var_eps * (1 - ss_fit$df / length(ss_fit$u)))
      )

    }

    return(comp_residual_data)


  }

  get_model_fit_information <- function(data, method) {

    # Avoid modifying the original data
    comp <- copy(data)

    # Linear models
    if (any(method == c("ols", "clsi", "fg"))) {

      # Required stats
      mor_data <- fun_of_replicates(comp)

      return(list("zeta" = estimate_zeta_ols(data = comp)$zeta,
                  "df" = 2))
    }

    # Smoothing Splines

    # Get weighting
    weighting <- switch(method,
                        "ss" = 1,
                        "ssw" = "estimate")

    ss_zeta <- tryCatch(
      expr = estimate_zeta_ss(data = comp,
                              df = NULL,
                              weighted = method == "ssw",
                              mor = FALSE,
                              na_rm = TRUE)$zeta,
      error = function(e) NA_real_,
      warning = function(e) NA_real_
    )

    if (is.na(ss_zeta)) {
      warning(
        "Could not estimate zeta for method = '",
        method,
        "', for the ",
        comp$comparisonID[1],
        " IVD-MD comparison. ",
        "A NA-value is returned."
      )
    }

    # Required stats
    mor_data <- fun_of_replicates(comp)
    ss_df <- tryCatch(
      expr = {
        smoothing_spline(data = mor_data,
                         weights = weighting,
                         df = NULL,
                         lambda = NULL,
                         df_max = 7.5,
                         attempt_fast = FALSE,
                         na_rm = TRUE)$df
      },
      error = NA_real_,
      warning = NA_real_
    )

    if (is.na(ss_df)) {
      warning(
        "Could not obtain a suitable df for method = '",
        method,
        "', for the ",
        comp$comparisonID[1],
        " IVD-MD comparison. ",
        "A NA-value is returned."
      )
    }

    return(list("zeta" = ss_zeta,
                "df" = ss_df))

  }

  get_comparison_qq_data <- function(comparison_residual_data) {

    # Avoid modifying the original data
    comparison_residual_data <- copy(comparison_residual_data)

    # Get qqplot object
    qqplot_object <- qqplot(x = rnorm(1e6),
                            y = comparison_residual_data$residuals,
                            plot.it = FALSE)

    output <- list("theoretical_quantiles" = qqplot_object$x,
                   "empirical_quantiles" = qqplot_object$y)

    return(output)

  }

  get_vs_concentration_data <- function(data,
                                        type,
                                        cv_percent = TRUE,
                                        var_instead = FALSE) {

    get_cv_versus_concentration_data <- function(data, cv_percent = TRUE) {

      # Bind global variables
      value <- NULL

      get_cv <- function(x) {
        if (length(x[!is.na(x)]) >= 2) {
          return(sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE))
        }
        else {
          return(NA_real_)
        }
      }

      get_concentration <- function(x) {
        if (length(x[!is.na(x)]) >= 1) {
          return(mean(x, na.rm = TRUE))
        }
        else {
          return(NA_real_)
        }
      }

      # Avoid modifying the original data
      data <- copy(data)

      # CV percent multiplier
      cv_multiplier <- if (cv_percent) {100} else {1}

      # Get CV versus concetration data
      data <- data[, list("cv" = get_cv(value) * cv_multiplier,
                          "concentration" = get_concentration(value)),
                   by = c("IVD-MD", "SampleID")]

      return(data)

    }

    get_sd_versus_concentration_data <- function(data, var_instead = FALSE) {

      # Bind global variables
      value <- NULL

      get_sd <- function(x) {
        if (length(x[!is.na(x)]) >= 2) {
          return(sd(x, na.rm = TRUE))
        }
        else {
          return(NA_real_)
        }
      }

      get_concentration <- function(x) {
        if (length(x[!is.na(x)]) >= 1) {
          return(mean(x, na.rm = TRUE))
        }
        else {
          return(NA_real_)
        }
      }

      # Avoid modifying the original data
      data <- copy(data)

      # CV percent multiplier
      sd_exponent <- if (var_instead) {2} else {1}

      # Get SD / Var versus concetration data
      data <- data[, list("sd" = get_sd(value) ** sd_exponent,
                          "concentration" = get_concentration(value)),
                   by = c("IVD-MD", "SampleID")]

      return(data)

    }

    # Initialize
    versus_concentration_data <- NULL

    # Avoid modifying the original data
    data <- copy(data)
    data[, comparisonID := NULL]

    # Get reversed data
    reversed_data <- reverse_comparison_data(data = data)

    # Get numeric columns
    numeric_columns <- setdiff(x = names(reversed_data),
                               y = c("SampleID", "ReplicateID"))

    # Convert to long-format
    reversed_data_long <- melt.data.table(
      data = reversed_data,
      id.vars = c("SampleID", "ReplicateID"),
      measure.vars = numeric_columns,
      variable.name = "IVD-MD",
      value.name = "value")

    # Calculate stats
    if (type == "sd_vs_concentration") {
      versus_concentration_data <- get_sd_versus_concentration_data(
        data = reversed_data_long,
        var_instead = var_instead
      )
    }
    else if (type == "cv_vs_concentration") {
      versus_concentration_data <- get_cv_versus_concentration_data(
        data = reversed_data_long,
        cv_percent = cv_percent
      )
    }

    if (is.null(versus_concentration_data)) {
      stop(
        "Could not calculate '",
        type,
        "' assessment plots."
      )
    }

    return(versus_concentration_data)

  }



  assessment_data <- NULL
  supplement_assessment_information <- NULL


  if (any(type == c("residual_plot", "residual_histogram", "qq_plot"))) {
    assessment_data <- data[, get_comparison_residual_data(data = .SD,
                                                           method = method),
                            by = "comparison"]
    supplement_assessment_information <- data[, get_model_fit_information(data = .SD,
                                                                          method = method),
                                              by = "comparison"]

    if (type == "qq_plot") {
      assessment_data <- assessment_data[, get_comparison_qq_data(.SD),
                                         by = "comparison"]

    }
  }

  else if (any(type == c("sd_vs_concentration", "cv_vs_concentration"))) {
    assessment_data <- get_vs_concentration_data(data = data,
                                                 type = type,
                                                 cv_percent = cv_percent,
                                                 var_instead = var_instead)



    supplement_assessment_information <- list(
      "sd_main_title_placeholder" = ifelse(var_instead,
                                           "Variance",
                                           "Standard Deviation"),
      "sd_y_name_placeholder" = ifelse(var_instead,
                                       "Variance",
                                       "Standard Deviation"),
      "cv_y_name_placeholder" = ifelse(cv_percent,
                                       " (%)",
                                       "")
    )

  }

  if (is.null(assessment_data)) {
    stop(
      "Could not get assessment data from data."
    )
  }

  output <- list("assessment_data" = assessment_data,
                 "supplement_assessment_information" = supplement_assessment_information)

  return(output)

}

#' @title
#' Draw Residual Versus Fitted Plots
#'
#' @description
#' This function is used to plot a residual plot for each IVD-MD comparison.
#'
#' @param assessment_data A \code{list}. Output from
#'                        \code{get_assessment_data()}.
#' @param additional_arguments See \code{?plot_assessment_plots()}.
#' @param draw_curves See \code{?plot_assessment_plots()}.
#'
#' @return
#' A \code{ggplot2} object. The generated residual plots.
#' @keywords internal
plot_residual_plot <- function(assessment_data, additional_arguments, draw_curves) {

  # Bind global variables
  residuals <- fitted <- NULL

  # Avoid modifying original data
  plotting_data <- copy(assessment_data$assessment_data)

  # Get point shape as integer
  point_shape_integer <- switch(additional_arguments$point_shape,
                                "circle" = 21,
                                "square" = 22,
                                "diamond" = 23,
                                "triangle" = 24)

  # Base plot
  output_plot <- ggplot() +
    geom_hline(yintercept = qnorm(p = c(0.025, 0.25, 0.50, 0.75, 0.975)),
               linetype = "twodash",
               color = "gray") +
    geom_point(data = plotting_data,
               mapping = aes(x = fitted,
                             y = residuals),
               shape = point_shape_integer,
               size = additional_arguments$point_size,
               fill = additional_arguments$point_fill,
               color = additional_arguments$point_border) +
    facet_wrap(facets = . ~ comparison,
               scales = "free")

  # Draw curves if desired
  if (draw_curves) {
    output_plot <- output_plot +
      geom_smooth(data = plotting_data,
                  mapping = aes(x = fitted,
                                y = residuals),
                  formula = additional_arguments$curve_formula,
                  method = "loess",
                  color = additional_arguments$curve_color,
                  span = additional_arguments$loess_span,
                  se = additional_arguments$curve_se,
                  fill = additional_arguments$curve_se_fill)
  }

  # Appearance
  output_plot <- output_plot +
    labs(title = additional_arguments$main_title,
         subtitle = additional_arguments$sub_title) +
    scale_x_continuous(name = additional_arguments$x_name,
                       n.breaks = additional_arguments$n_breaks) +
    scale_y_continuous(name = additional_arguments$y_name,
                       n.breaks = additional_arguments$n_breaks) +
    theme_bw() +
    theme(plot.title = element_text(face = "bold",
                                    color = additional_arguments$main_title_color,
                                    hjust = 0.5),
          plot.subtitle = element_text(face = "bold",
                                       color = additional_arguments$main_title_color,
                                       hjust = 0.5),
          strip.background = element_rect(color = "black",
                                          fill = additional_arguments$strip_fill),
          strip.text = element_text(face = "bold",
                                    color = additional_arguments$strip_text_color))

  return(output_plot)


}

#' @title
#' Draw Residual Histograms
#'
#' @description
#' This function is used to plot a residual histogram for each IVD-MD
#' comparison.
#'
#' @param assessment_data A \code{list}. Output from
#'                        \code{get_assessment_data()}.
#' @param additional_arguments See \code{?plot_assessment_plots()}.
#' @param draw_curves See \code{?plot_assessment_plots()}.
#'
#' @return
#' A \code{ggplot2} object. The generated residual histograms.
#' @keywords internal
plot_residual_hist <- function(assessment_data, additional_arguments, draw_curves) {

  # Bind global variables
  residuals <- density <- fitted <- NULL

  # Avoid modifying original data
  plotting_data <- copy(assessment_data$assessment_data)

  # Calculate number of bins (crude estimate)
  n_bins <- ceiling(mean(tapply(X = plotting_data$residuals,
                                INDEX = plotting_data$comparison,
                                FUN = nclass.FD,
                                simplify = TRUE)))

  # Base plot
  output_plot <- ggplot() +
    geom_histogram(data = plotting_data,
                   mapping = aes(x = residuals,
                                 y = after_stat(density)),
                   bins = n_bins,
                   fill = additional_arguments$histogram_fill,
                   color = additional_arguments$histogram_border) +
    facet_wrap(facets = . ~ comparison,
               scales = "free")

  # Draw curves if desired
  if (draw_curves) {

    # Get N(0, 1) density curves
    normal_density_curves <- copy(plotting_data)[, list(
      residuals = seq(
        from = min(residuals, na.rm = TRUE),
        to = max(residuals, na.rm = TRUE),
        length.out = 200L
      )
    ),
    by = "comparison"]
    normal_density_curves[, density := dnorm(residuals)]

    # Draw theoretical density curves on top of histograms
    output_plot <- output_plot +
      geom_line(data = normal_density_curves,
                mapping = aes(x = residuals,
                              y = density),
                color = additional_arguments$curve_color,
                linewidth = additional_arguments$point_size - 0.5)
  }

  # Appearance
  output_plot <- output_plot +
    labs(title = additional_arguments$main_title,
         subtitle = additional_arguments$sub_title) +
    scale_x_continuous(name = additional_arguments$x_name,
                       n.breaks = additional_arguments$n_breaks,
                       expand = c(0, 0)) +
    scale_y_continuous(name = additional_arguments$y_name,
                       n.breaks = additional_arguments$n_breaks,
                       expand = expansion(mult = c(0, 0.05)),
                       limits = c(0, NA)) +
    theme_classic() +
    theme(plot.title = element_text(face = "bold",
                                    color = additional_arguments$main_title_color,
                                    hjust = 0.5),
          plot.subtitle = element_text(face = "bold",
                                       color = additional_arguments$main_title_color,
                                       hjust = 0.5),
          strip.background = element_rect(color = "black",
                                          fill = additional_arguments$strip_fill),
          strip.text = element_text(face = "bold",
                                    color = additional_arguments$strip_text_color))

  return(output_plot)
}

#' @title
#' Draw QQ Normal Plots
#'
#' @description
#' This function is used to plot a QQ-normal plots for each IVD-MD
#' comparison.
#'
#' @param assessment_data A \code{list}. Output from
#'                        \code{get_assessment_data()}.
#' @param additional_arguments See \code{?plot_assessment_plots()}.
#' @param draw_curves See \code{?plot_assessment_plots()}.
#'
#' @return
#' A \code{ggplot2} object. The generated QQ-normal plots.
#' @keywords internal
plot_qq_plot <- function(assessment_data, additional_arguments, draw_curves) {

  # Bind global variables
  theoretical_quantiles <- empirical_quantiles <- NULL

  # Avoid modifying original data
  plotting_data <- copy(assessment_data$assessment_data)

  # Get point shape as integer
  point_shape_integer <- switch(additional_arguments$point_shape,
                                "circle" = 21,
                                "square" = 22,
                                "diamond" = 23,
                                "triangle" = 24)

  # Base plot
  output_plot <- ggplot()

  # Draw curves if desired
  if (draw_curves) {
    output_plot <- output_plot +
      geom_smooth(data = plotting_data,
                  mapping = aes(x = theoretical_quantiles,
                                y = empirical_quantiles),
                  formula = additional_arguments$curve_formula,
                  method = "loess",
                  color = additional_arguments$curve_color,
                  span = additional_arguments$loess_span,
                  se = additional_arguments$curve_se,
                  fill = additional_arguments$curve_se_fill)
  }

  # Add points above curve
  output_plot <- output_plot +
    geom_abline(slope = 1,
                intercept = 0,
                color = "black") +
    geom_point(data = plotting_data,
               mapping = aes(x = theoretical_quantiles,
                             y = empirical_quantiles),
               shape = point_shape_integer,
               size = additional_arguments$point_size,
               fill = additional_arguments$point_fill,
               color = additional_arguments$point_border) +
    facet_wrap(facets = . ~ comparison,
               scales = "free")

  # Appearance
  output_plot <- output_plot +
    labs(title = additional_arguments$main_title,
         subtitle = additional_arguments$sub_title) +
    scale_x_continuous(name = additional_arguments$x_name,
                       n.breaks = additional_arguments$n_breaks) +
    scale_y_continuous(name = additional_arguments$y_name,
                       n.breaks = additional_arguments$n_breaks) +
    theme_bw() +
    theme(plot.title = element_text(face = "bold",
                                    color = additional_arguments$main_title_color,
                                    hjust = 0.5),
          plot.subtitle = element_text(face = "bold",
                                       color = additional_arguments$main_title_color,
                                       hjust = 0.5),
          strip.background = element_rect(color = "black",
                                          fill = additional_arguments$strip_fill),
          strip.text = element_text(face = "bold",
                                    color = additional_arguments$strip_text_color))

  return(output_plot)



}

#' @title
#' Draw Standard Deviation (or Variance) Versus Concentration Plots
#'
#' @description
#' This function is used to plot a standard deviation (or variance) versus
#' concentration plot for each IVD-MD in \code{data}.
#'
#' @param assessment_data A \code{list}. Output from
#'                        \code{get_assessment_data()}.
#' @param additional_arguments See \code{?plot_assessment_plots()}.
#' @param draw_curves See \code{?plot_assessment_plots()}.
#'
#' @return
#' A \code{ggplot2} object. The generated standard deviation (or variance)
#' versus concentration plots.
#' @keywords internal
plot_sd_vs_concentration <- function(assessment_data,
                                     additional_arguments,
                                     draw_curves) {

  # Bind global variables
  concentration <- NULL

  # Avoid modifying original data
  plotting_data <- copy(assessment_data$assessment_data)
  supplement_information <- copy(assessment_data$supplement_assessment_information)

  # Fill in placeholders if default names are used
  if (stri_detect(str = additional_arguments$y_name, fixed = "[*]")) {
    additional_arguments$y_name <- stri_replace_all_fixed(
      str = additional_arguments$y_name,
      pattern = "[*]",
      replacement = supplement_information$sd_y_name_placeholder
    )
  }

  if (stri_detect(str = additional_arguments$main_title, fixed = "[*]")) {
    additional_arguments$main_title <- stri_replace_all_fixed(
      str = additional_arguments$main_title,
      pattern = "[*]",
      replacement = supplement_information$sd_main_title_placeholder
    )
  }


  # Get point shape as integer
  point_shape_integer <- switch(additional_arguments$point_shape,
                                "circle" = 21,
                                "square" = 22,
                                "diamond" = 23,
                                "triangle" = 24)

  # Base plot
  output_plot <- ggplot() +
    geom_point(data = plotting_data,
               mapping = aes(x = concentration,
                             y = sd),
               shape = point_shape_integer,
               size = additional_arguments$point_size,
               fill = additional_arguments$point_fill,
               color = additional_arguments$point_border) +
    facet_wrap(facets = . ~ `IVD-MD`,
               scales = "free")

  # Add curves if desired
  if (draw_curves) {
    output_plot <- output_plot +
      geom_smooth(data = plotting_data,
                  mapping = aes(x = concentration,
                                y = sd),
                  formula = additional_arguments$curve_formula,
                  method = "loess",
                  color = additional_arguments$curve_color,
                  span = additional_arguments$loess_span,
                  se = additional_arguments$curve_se,
                  fill = additional_arguments$curve_se_fill)
  }

  # Appearance
  output_plot <- output_plot +
    labs(title = additional_arguments$main_title,
         subtitle = additional_arguments$sub_title) +
    scale_x_continuous(name = additional_arguments$x_name,
                       n.breaks = additional_arguments$n_breaks) +
    scale_y_continuous(name = additional_arguments$y_name,
                       n.breaks = additional_arguments$n_breaks) +
    theme_classic() +
    theme(plot.title = element_text(face = "bold",
                                    color = additional_arguments$main_title_color,
                                    hjust = 0.5),
          plot.subtitle = element_text(face = "bold",
                                       color = additional_arguments$main_title_color,
                                       hjust = 0.5),
          strip.background = element_rect(color = "black",
                                          fill = additional_arguments$strip_fill),
          strip.text = element_text(face = "bold",
                                    color = additional_arguments$strip_text_color))

  return(output_plot)


}

#' @title
#' Draw Coefficient of Variation Versus Concentration Plots
#'
#' @description
#' This function is used to plot a coefficient of variation versus
#' concentration plot for each IVD-MD in \code{data}.
#'
#' @param assessment_data A \code{list}. Output from
#'                        \code{get_assessment_data()}.
#' @param additional_arguments See \code{?plot_assessment_plots()}.
#' @param draw_curves See \code{?plot_assessment_plots()}.
#'
#' @return
#' A \code{ggplot2} object. The generated coefficient of variation versus
#' concentration plots.
#' @keywords internal
plot_cv_vs_concentration <- function(assessment_data,
                                     additional_arguments,
                                     draw_curves) {

  # Bind global variables
  concentration <- cv <- NULL

  # Avoid modifying original data
  plotting_data <- copy(assessment_data$assessment_data)
  supplement_information <- copy(assessment_data$supplement_assessment_information)

  # Fill in placeholders if default names are used
  if (stri_detect(str = additional_arguments$y_name, fixed = "[*]")) {
    additional_arguments$y_name <- stri_replace_all_fixed(
      str = additional_arguments$y_name,
      pattern = "[*]",
      replacement = supplement_information$cv_y_name_placeholder
    )
  }

  # Get point shape as integer
  point_shape_integer <- switch(additional_arguments$point_shape,
                                "circle" = 21,
                                "square" = 22,
                                "diamond" = 23,
                                "triangle" = 24)

  # Base plot
  output_plot <- ggplot() +
    geom_point(data = plotting_data,
               mapping = aes(x = concentration,
                             y = cv),
               shape = point_shape_integer,
               size = additional_arguments$point_size,
               fill = additional_arguments$point_fill,
               color = additional_arguments$point_border) +
    facet_wrap(facets = . ~ `IVD-MD`,
               scales = "free")

  # Add curves if desired
  if (draw_curves) {
    output_plot <- output_plot +
      geom_smooth(data = plotting_data,
                  mapping = aes(x = concentration,
                                y = cv),
                  formula = additional_arguments$curve_formula,
                  method = "loess",
                  color = additional_arguments$curve_color,
                  span = additional_arguments$loess_span,
                  se = additional_arguments$curve_se,
                  fill = additional_arguments$curve_se_fill)
  }

  # Appearance
  output_plot <- output_plot +
    labs(title = additional_arguments$main_title,
         subtitle = additional_arguments$sub_title) +
    scale_x_continuous(name = additional_arguments$x_name,
                       n.breaks = additional_arguments$n_breaks) +
    scale_y_continuous(name = additional_arguments$y_name,
                       n.breaks = additional_arguments$n_breaks) +
    theme_classic() +
    theme(plot.title = element_text(face = "bold",
                                    color = additional_arguments$main_title_color,
                                    hjust = 0.5),
          plot.subtitle = element_text(face = "bold",
                                       color = additional_arguments$main_title_color,
                                       hjust = 0.5),
          strip.background = element_rect(color = "black",
                                          fill = additional_arguments$strip_fill),
          strip.text = element_text(face = "bold",
                                    color = additional_arguments$strip_text_color))

  return(output_plot)


}


#' @title
#' Plot Model Diagnostics Plots
#'
#' @param data A \code{data.table}, \code{list} or \code{data.frame}.
#'             Must contain the clinical sample data with variables:
#'             \itemize{
#'                \item \code{comparison: } A \code{character} vector. The
#'                      comparison identifiers. Typically on the form
#'                      \code{'MP_A - MP_B'}.
#'                \item \code{SampleID: } A \code{character} vector. The sample
#'                      identifiers for the clinical samples.
#'                \item \code{ReplicateID: } A \code{character} vector. The
#'                      replicated measurement identifiers.
#'                \item \code{MP_A: } A \code{numeric} vector. The observed
#'                      measurement results from IVD-MD \code{MP_A} (response).
#'                \item \code{MP_B: } A \code{numeric} vector. The observed
#'                      measurement results from IVD-MD \code{MP_A} (predictor).
#'             }
#' @param method A \code{character} string. The desired model for calculating
#'               residuals and fitted values. The possible options are:
#'               \code{'fg'} (default), \code{'clsi'}, \code{'ols'},
#'               \code{'ss'} and \code{'ssw'}.
#' @param type A \code{character} string. The desired model assement plot to
#'             draw. Can be one out of:
#'             \itemize{
#'              \item \code{residual_plot: } Residual plot (default).
#'                    Standardized model residuals at the y-axis and fitted
#'                    values at the x-axis.
#'              \item \code{residual_histogram: } Residual histogram plot.
#'                    Standardized residuals are the x-axis and density values
#'                    at the y-axis.
#'              \item \code{qq_plot: } Normal QQ plot. Theoretical quantiles
#'                    of the N(0, 1)-distribution at the x-axis and empirical
#'                    quantiles of the standardized residuals at the y-axis.
#'              \item \code{sd_vs_concentration: } Clinical sample standard
#'                    deviations at the y-axis and the corresponding
#'                    concentration values at the x-axis.
#'              \item \code{cv_vs_concentration: } Clinical sample coefficient
#'                    of variation at the y-axis and the corresponding
#'                    concentration values at the y-axis.
#'             }
#' @param draw_curves A \code{logical} value. If \code{TRUE} (default),
#'                    informative curves are added to the assessment plots when
#'                    relevant.
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
#'                              \item \code{main_title: }
#'                              \item \code{sub_title: }
#'                              \item \code{x_name: }
#'                              \item \code{y_name: }
#'                              \item \code{n_breaks: }
#'                              \item \code{title_color: }
#'                              \item \code{sub_title_color: }
#'                              \item \code{strip_fill: }
#'                              \item \code{strip_text_color: }
#'                              \item \code{point_shape: }
#'                              \item \code{point_size: }
#'                              \item \code{point_fill: }
#'                              \item \code{point_border: }
#'                              \item \code{curve_color: }
#'                              \item \code{curve_se: }
#'                              \item \code{curve_se_fill: }
#'                              \item \code{curve_formula: }
#'                              \item \code{loess_span: }
#'                              \item \code{histogram_fill: }
#'                              \item \code{histogram_border: }
#'                              \item \code{cv_percent: }
#'                              \item \code{var_instead: }
#'                             }
#'
#' @description
#' Generate model assessment plots for each IVD-MD comparison (or IVD-MD).
#'
#' @details
#' Modeling make assumptions. One should strive to test these assumptions to
#' validate whether a particular estimated model can be trusted. This function
#' can be used to draw different diagnostic plots that be used to assess
#' whether the model assumptions are satisfied for \code{data}.
#'
#' Currently, this function supports five different plot types:
#' \itemize{
#'    \item \code{residual_plot: } Residual plot (default). Standardized model
#'          residuals at the y-axis and fitted values at the x-axis. Can be
#'          used to check for model error heteroscedasticity. Can also be used
#'          to check for non-linearity if a linear model is used to estimate
#'          the residuals.
#'    \item \code{residual_histogram: } Residual histogram plot. Standardized
#'          residuals are the x-axis and density values at the y-axis. Can be
#'          used to check for model error normality.
#'    \item \code{qq_plot: } Normal QQ plot. Theoretical quantiles of the
#'          N(0, 1)-distribution at the x-axis and empirical quantiles of the
#'          standardized residuals at the y-axis. Can be used to check for
#'          model error normality.
#'    \item \code{sd_vs_concentration: } Clinical sample standard deviations at
#'          the y-axis and the corresponding concentration values at the
#'          x-axis. Can be used to check for measurement error variance
#'          heteroscedasticity. Often a supplement to \code{residual_plot}.
#'    \item \code{cv_vs_concentration: } Clinical sample coefficient of
#'          variation at the y-axis and the corresponding concentration values
#'          at the y-axis. Can be used to check whether a log-transformation
#'          would solve heteroscedasticity issues for the measurement error
#'          variance.
#' }
#'
#' For formal testing, the end-user may use \code{perform_assessment_tests()}.
#'
#' Note: In general, it is not recommended to specify
#' \code{additional_arguments}. The default settings are usually adequate in
#' most scenarios.
#'
#'
#' @return A \code{ggplot2} object. The drawn desired assessment plots.
#' @export
#'
#' @examples
#' # Required packages
#' library(smooth.commutability)
#' library(data.table)
#'
#' # Read example data from smooth.commutability package
#' test_cs_data <- copy(crp_cs_data)
#'
#' # Draw Residual Plots Using Weighted Smoothing Splines
#' plot_assessment_plots(data = test_cs_data,
#'                       method = "ssw",
#'                       type = "residual_plot",
#'                       draw_curves = TRUE,
#'                       plot_theme = "noklus")
#'
#' # Draw QQ Normal Plots Using Deming Regression
#' plot_assessment_plots(data = test_cs_data,
#'                       method = "fg",
#'                       type = "qq_plot",
#'                       draw_curves = FALSE,
#'                       plot_theme = "depression")
#'
#' # Draw Coefficient of Variation Versus Concentration Plots
#' plot_assessment_plots(data = test_cs_data,
#'                       type = "cv_vs_concentration",
#'                       draw_curves = TRUE,
#'                       plot_theme = "soft",
#'                       additional_arguments = list(cv_percent = TRUE))
#'
#'

plot_assessment_plots <- function(data,
                                  method = "fg",
                                  type = "residual_plot",
                                  draw_curves = TRUE,
                                  plot_theme = "default",
                                  additional_arguments = NULL){

  # Validate data
  data <- validate_data_pat(data)

  # Validate method
  method <- validate_method(method)

  # Validate type
  type <- validate_type(type)

  # Validate plot_theme
  plot_theme <- validate_plot_theme(plot_theme)

  # Apply plotting theme
  additional_arguments <- apply_theme_attributes_pap(
    plot_theme = plot_theme,
    additional_arguments = additional_arguments
  )

  # Update additional_arguments
  additional_arguments <- update_all_attributes_pap(
    additional_arguments = additional_arguments,
    method = method,
    type = type
  )

  # Get assessment data
  assessment_data <- get_assessment_data(
    data = data,
    method = method,
    type = type,
    cv_percent = additional_arguments$cv_percent,
    var_instead = additional_arguments$var_instead
  )

  # Draw plots
  if (type == "residual_plot") {
    plot_residual_plot(assessment_data = assessment_data,
                       additional_arguments = additional_arguments,
                       draw_curves = draw_curves)
  }

  else if (type == "residual_histogram") {
    plot_residual_hist(assessment_data = assessment_data,
                       additional_arguments = additional_arguments,
                       draw_curves = draw_curves)
  }

  else if (type == "qq_plot") {
    plot_qq_plot(assessment_data = assessment_data,
                 additional_arguments = additional_arguments,
                 draw_curves = draw_curves)
  }

  else if (type == "sd_vs_concentration") {
    plot_sd_vs_concentration(assessment_data = assessment_data,
                             additional_arguments = additional_arguments,
                             draw_curves = draw_curves)
  }

  else if (type == "cv_vs_concentration") {
    plot_cv_vs_concentration(assessment_data = assessment_data,
                             additional_arguments = additional_arguments,
                             draw_curves = draw_curves)
  }

  else {
    stop(
      "Plot type is not recongnized. Got: '",
      type,
      "'.",
      "Valid plot types include: 'residual_plot', 'residual_histogram', ",
      "'qq_plot', 'sd_vs_concentration', 'cv_vs_concentration'. "
    )
  }




}
