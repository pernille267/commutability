#' Plots commutability evaluation plots based on ce_data, pb_data and ce_data
#'
#' @param cs_data A \code{list}, \code{data table} or \code{data frame} - Data containing clinical samples' measurements.
#' @param pb_data A \code{list}, \code{data table} or \code{data frame} - Data containing prediction band data.
#' @param ce_data A \code{list}, \code{data table} or \code{data frame} - Data containing prediction evaluation data for evaluated control materials.
#' @param exclude_rings \code{Logical} - Remove circles that signify strengths of commutability evaluation conclusions. Default is \code{FALSE}.
#' @param exclude_cs \code{Logical} - Remove clinical sample data from plots. Default is \code{FALSE}.
#' @param plot_theme \code{Character} - Which plotting theme should be used. Possible values are 'default', 'noklus', 'soft', 'depression' and 'happy'. Default is 'custom', which means that no particular theme is applied to the plots.
#' @param additional_arguments \code{List} - Additional arguments for the output plot. Default is \code{NULL}, which implies that default plot settings are used. Optional additional arguments include
#' \itemize{
#'   \item{\code{main_title}: }{Main title of the plot}
#'   \item{\code{sub_title}: }{Sub title of the plot}
#'   \item{\code{x_name}: }{Title of the x-axis}
#'   \item{\code{y_name}: }{Title of the y-axis}
#'   \item{\code{n_breaks}: }{Number of axis-breaks}
#'   \item{\code{title_color}: }{Text color of 'main_title'. Default color is \code{'black'}}
#'   \item{\code{sub_title_color}: }{Text color of 'sub_title'. Default color is \code{'black'}}
#'   \item{\code{pb_fill:  }}{Fill color of prediction bands. Default fill color is \code{'green'}}
#'   \item{\code{pb_border}:  }{Color of the border of the prediction bands. Default color is \code{'black'}}
#'   \item{\code{curve}:  }{Should a curve be included to the clinical samples measurements. Default is \code{FALSE}}
#'   \item{\code{curve_type}: }{Which type of curve should be included. Only relevant if \code{curve = TRUE}. Possible values are 'equivalence_curve', 'fitted_curve' and 'flexible_curve'}
#'   \item{\code{curve_color}: }{Color of curve. Only relevant if \code{curve = TRUE}. Default is \code{'black'}.}
#'   \item{\code{point_shape}: }{Shape of clinical sample points. Possible inputs are 'circle' (default), 'square', 'diamond' and 'triangle'}
#'   \item{\code{point_size}: }{Size of clinical sample points. Possible inputs are of type \code{double} and must larger than 0. Default is automatically determined}
#'   \item{\code{point_fill}: }{fill color of clinical sample points. May be a color defined in \code{colors()} or a HEX code}
#'   \item{\code{point_border}: }{border color of clinical sample points. May be a color defined in \code{colors()} or a HEX code. Default color is \code{'black'}}
#'   \item{\code{comparison_fill}: }{Fill color for the individual plot labels listing the IVD-MD comparisons. Must be a valid color name or HEX code}
#'   \item{\code{comparison_text_color}: }{Text color for the individual plot labels listing the IVD-MD comparisons. Must be a valid color name or HEX code}
#'   \item{\code{legend_fill}: }{Fill color for the legend regions. Must be a valid color name or HEX code}
#'   \item{\code{legend_text_color}: }{Text color for the legends. Must be a valid color name or HEX code}
#' }
#' @param testing \code{Logical} - Should the function output be test-friendly? Only useful for maintainer, so the end-user should not change this parameter. Default is \code{FALSE}.
#'
#' @return \code{ggplot2} object that is a grid of plots having dimensions corresponding to the number of unique IVD-MD comparisons
#' @export
#'
#' @examples print(1)

plot_commutability_evaluation_plots <- function(cs_data, pb_data, ce_data, exclude_rings = FALSE, exclude_cs = FALSE, plot_theme = c("custom", "default", "noklus", "soft", "depression", "happy"), additional_arguments = NULL, testing = FALSE){

  if(any("dins_conclusion" == names(pb_data))){
    pb_data$dins_conclusion <- ifelse(pb_data$dins_conclusion == 0, "acceptable", "not acceptable")
  }
  else{
    stop("'dins_conclusion' is missing in pb_data")
  }

  if(any("dins_conclusion" == names(ce_data))){
    ce_data$dins_conclusion <- ifelse(ce_data$dins_conclusion == 0, "acceptable", "not acceptable")
  }
  else{
    stop("'dins_conclusion' is missing in ce_data")
  }

  if(any("pi_inside" == names(ce_data))){
    ce_data$pi_inside <- ifelse(ce_data$pi_inside == 0, "no", "yes")
  }
  else{
    stop("'pi_inside' is missing in ce_data")
  }

  if(any("inside_rate" == names(ce_data))){
    ce_data$pi_conclusion_correctness <- mapply(FUN = function(x, y) if(x == "yes"){y}else{1 - y}, ce_data$pi_inside, ce_data$inside_rate)
  }

  else{
    stop("'inside_rate' is missing in ce_data")
  }

  if(is.null(plot_theme)){
    plot_theme <- "custom"
  }

  if(length(plot_theme) > 1){
    plot_theme <- plot_theme[1]
  }

  if(is.na(plot_theme)){
    plot_theme <- "custom"
  }

  if(!any(plot_theme == c("custom", "default", "noklus", "soft", "depression", "happy"))){
    plot_theme <- "custom"
  }

  if(!any(isTRUE(exclude_cs), isFALSE(exclude_cs))){
    warning(paste0("exclude_cs = '", exclude_cs, "' is not an accepted output. 'exclude_cs' is therefor set to FALSE"))
    exclude_cs <- FALSE
  }

  if(!any(isTRUE(exclude_rings), isFALSE(exclude_rings))){
    warning(paste0("exclude_rings = '", exclude_rings, "' is not an accepted output. 'exclude_rings' is therefor set to FALSE"))
    exclude_rings <- FALSE
  }

  if(plot_theme != "custom"){
    if(any(plot_theme == c("default", "noklus", "soft", "depression", "happy"))){
      if(plot_theme == "default"){
        plot_theme <- "default"
        theme_arguments <- list(title_color = "#097894",
                                sub_title_color = "#097894",
                                comparison_fill = "#55CDEC",
                                comparison_text_color = "#000000",
                                pb_fill = "#BFE3B4",
                                pb_border = "#000000",
                                curve_color = "#552A51",
                                point_fill = "#0912BC",
                                point_border = "#000000",
                                legend_fill = "#F9F9F9",
                                legend_text_color = "#000000")
      }
      else if(plot_theme == "noklus"){
        plot_theme <- "noklus"
        theme_arguments <- list(title_color = "#28A745",
                                sub_title_color = "#28A745",
                                comparison_fill = "#FFFFFF",
                                comparison_text_color = "#3DD2FF",
                                pb_fill = "#8AE4FF",
                                pb_border = "#000000",
                                curve_color = "#00300B",
                                point_fill = "#00E636",
                                point_border = "#000000",
                                legend_fill = "#FFFFFF",
                                legend_text_color = "#28A745")
      }
      else if(plot_theme == "soft"){
        plot_theme <- "soft"
        theme_arguments <- list(title_color = "#9ADCFF",
                                sub_title_color = "#9ADCFF",
                                comparison_fill = "#FFF89A",
                                comparison_text_color = "#B631D2",
                                pb_fill = "#FFB2A6",
                                pb_border = "#FF8673",
                                curve_color = "#D18CE0",
                                point_fill = "#9ADCFF",
                                point_border = "#000000",
                                legend_fill = "#FFFFFF",
                                legend_text_color = "#9ADCFF")
      }
      else if(plot_theme == "depression"){
        plot_theme <- "depression"
        theme_arguments <- list(title_color = "#160040",
                                sub_title_color = "#4C0070",
                                comparison_fill = "#160040",
                                comparison_text_color = "#FFFFFF",
                                pb_fill = "#79018C",
                                pb_border = "#370040",
                                curve_color = "#151515",
                                point_fill = "#9A0680",
                                point_border = "#000000",
                                legend_fill = "#F9F9F9",
                                legend_text_color = "#6E85B2")
      }
      else if(plot_theme == "happy"){
        plot_theme <- "happy"
        theme_arguments <- list(title_color = "#36AE7C",
                                sub_title_color = "#36AE7C",
                                comparison_fill = "#F9D923",
                                comparison_text_color = "#EB5353",
                                pb_fill = "#187498",
                                pb_border = "#000000",
                                curve_color = "#151515",
                                point_fill = "#EB9F53",
                                point_border = "#EB5353",
                                legend_fill = "#F9F9F9",
                                legend_text_color = "#36AE7C")
      }
    }
    else{
      plot_theme <- "custom"
    }
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


  # Default 'additional_arguments'
  default_main_title <- "Commutability evaluation plots for all unique IVD-MD comparisons"
  default_sub_title <- NULL
  default_x_name <- "Measurements along x-axis * - "
  default_y_name <- "Measurements along y-axis  - *"
  default_n_breaks <- 6L
  default_title_color <- "black"
  default_sub_title_color <- "black"
  default_pb_fill <- "green"
  default_pb_border <- "black"
  default_curve <- FALSE
  default_type <- "equivalence_curve"
  default_curve_color <- "gray"
  default_point_shape <- "circle"
  default_point_size <- 0.75
  default_point_fill <- "gray"
  default_point_border <- "black"
  default_comparison_fill <- "#5BCEFA"
  default_comparison_text_color <- "#000000"
  default_legend_fill <- "#F9F9F9"
  default_legend_text_color <- "#000000"

  given_arguments <- names(additional_arguments)

  if(any("main_title" == given_arguments)){
    main_title <- additional_arguments$main_title[1]

    if(is.null(main_title)){
      additional_arguments$main_title <- NULL
      given_arguments <- additional_arguments |> names()
      default_main_title <- NULL
    }
    else if(isTRUE(main_title == "")){
      additional_arguments$main_title <- NULL
      given_arguments <- additional_arguments |> names()
      default_main_title <- NULL
    }
    else if(is.character(main_title)){
      main_title <- main_title
    }
    else{
      warning(paste0("main_title is not of character type, but '", typeof(main_title) ,"'. Default main_title is used instead"))
      additional_arguments$main_title <- default_main_title
    }
  }
  if(any("sub_title" == given_arguments)){
    sub_title <- additional_arguments$sub_title[1]
    if(is.null(sub_title) || is.na(sub_title)){
      additional_arguments$sub_title <- default_sub_title
      given_arguments <- additional_arguments |> names()
    }
    else if(isTRUE(sub_title == "")){
      additional_arguments$sub_title <- default_sub_title
      given_arguments <- additional_arguments |> names()
    }
    else if(is.character(sub_title)){
      additional_arguments$sub_title <- sub_title
    }
    else{
      warning(paste0("sub_title is not of character type, but '", typeof(sub_title) ,"'. Default sub_title is used instead"))
      additional_arguments$sub_title <- default_sub_title
      given_arguments <- additional_arguments |> names()
    }
  }

  if(any("x_name" == given_arguments)){
    x_name <- additional_arguments$x_name[1]
    if(is.character(x_name)){
      x_name <- x_name
    }
    else{
      warning(paste0("x_name is not of character type, but '", typeof(x_name) ,"'. Default x_name is used instead"))
      x_name <- "Measurements along x-axis  - *"
    }
  }
  if(any("y_name" == given_arguments)){
    y_name <- additional_arguments$y_name[1]
    if(is.character(y_name)){
      y_name <- y_name
    }
    else{
      warning(paste0("y_name is not of character type, but '", typeof(y_name) ,"'. Default y_name is used instead"))
      y_name <- "Measurements along y-axis  - *"
    }
  }
  if(any("n_breaks" == given_arguments)){
    n_breaks <- additional_arguments$n_breaks[1]
    if(is.na(n_breaks) | is.null(n_breaks)){
      n_breaks <- 6L
    }
    if(is.character(n_breaks)){
      if(stri_detect(str = n_breaks, regex = "[[:digit:]]")){
        candidate_n_breaks <- stri_replace_all(str = n_breaks, replacement = "", regex = "[^[:digit:]]")
        if(stri_detect(str = candidate_n_breaks, regex = "[[:digit:]]")){
          n_breaks <- as.numeric(candidate_n_breaks)
        }
        else{
          warning(paste0("A digit was first found, but it got away..."," :( "," A severe tragedy..."))
          n_breaks <- 6L
        }
      }
      else{
        warning(paste0("The picked value for n_breaks, ", n_breaks, " , does not even contain any numbers and is therefore not valid. Default value for n_breaks is used instead"))
        n_breaks <- 6L
      }
    }
    else if(is.integer(n_breaks) | is.numeric(n_breaks)){
      n_breaks <- n_breaks
    }
    else{
      warning(paste0("The picked value for n_breaks, ", n_breaks, " , is not valid. Default value for n_breaks is used instead"))
      n_breaks <- 6L
    }

    additional_arguments$n_breaks <- n_breaks

  }

  color_arguments <- c("title_color", "sub_title_color", "pb_fill", "pb_border", "curve_color", "point_fill", "point_border", "comparison_fill", "comparison_text_color", "legend_fill", "legend_text_color")

  for(i in 1:length(color_arguments)){

    if(any(color_arguments[i] == given_arguments)){

      registered_color <- additional_arguments[[which(color_arguments[i] == given_arguments)]][1]

      if(is.na(registered_color) | is.null(registered_color)){
        if(color_arguments[i] == "title_color"){
          registered_color <- default_title_color
        }
        else if(color_arguments[i] == "sub_title_color"){
          registered_color <- default_sub_title_color
        }
        else if(color_arguments[i] == "pb_fill"){
          registered_color <- default_pb_fill
        }
        else if(color_arguments[i] == "pb_border"){
          registered_color <- default_pb_border
        }
        else if(color_arguments[i] == "curve_color"){
          registered_color <- default_curve_color
        }
        else if(color_arguments[i] == "point_fill"){
          registered_color <- default_point_fill
        }
        else if(color_arguments[i] == "point_border"){
          registered_color <- default_point_border
        }
        else if(color_arguments[i] == "comparison_fill"){
          registered_color <- default_comparison_fill
        }
        else if(color_arguments[i] == "comparison_text_color"){
          registered_color <- default_comparison_text_color
        }
        else if(color_arguments[i] == "legend_fill"){
          registered_color <- default_legend_fill
        }
        else if(color_arguments[i] == "legend_text_color"){
          registered_color <- default_legend_text_color
        }
      }

      if(!(any(registered_color == colors()) | any(registered_color == 1:length(colors())))){
        if(stri_length(registered_color) == 7 & stri_locate_first_fixed(registered_color, pattern = "#", )[1,1] == 1){
          test_hex <- stri_sub(registered_color, from = 2, to = 7) |> stri_split_boundaries(type = "character") |> unlist()
          valid_hex <- all(sapply(test_hex, FUN = function(x) if(stri_detect(x, regex = "[[:digit:]|[abcdefABCDEF]]")){TRUE}else{FALSE}))
          if(!valid_hex){
            warning(paste0("The picked color for '", color_arguments, "' , was suspected to be a HEX code, but was not valid. Default fill color is used instead"))
            if(color_arguments[i] == "title_color"){
              registered_color <- default_title_color
            }
            else if(color_arguments[i] == "sub_title_color"){
              registered_color <- default_sub_title_color
            }
            else if(color_arguments[i] == "pb_fill"){
              registered_color <- default_pb_fill
            }
            else if(color_arguments[i] == "pb_border"){
              registered_color <- default_pb_border
            }
            else if(color_arguments[i] == "curve_color"){
              registered_color <- default_curve_color
            }
            else if(color_arguments[i] == "point_fill"){
              registered_color <- default_point_fill
            }
            else if(color_arguments[i] == "point_border"){
              registered_color <- default_point_border
            }
            else if(color_arguments[i] == "comparison_fill"){
              registered_color <- default_comparison_fill
            }
            else if(color_arguments[i] == "comparison_text_color"){
              registered_color <- default_comparison_text_color
            }
            else if(color_arguments[i] == "legend_fill"){
              registered_color <- default_legend_fill
            }
            else if(color_arguments[i] == "legend_text_color"){
              registered_color <- default_legend_text_color
            }
            else{
              stop("Something went wrong... How?")
            }
          }
        }
        else{
          warning(paste0("The picked color for '", color_arguments[i], "' , is not valid. Default fill color is used instead"))
          if(color_arguments[i] == "title_color"){
            registered_color <- default_title_color
          }
          else if(color_arguments[i] == "sub_title_color"){
            registered_color <- default_sub_title_color
          }
          else if(color_arguments[i] == "pb_fill"){
            registered_color <- default_pb_fill
          }
          else if(color_arguments[i] == "pb_border"){
            registered_color <- default_pb_border
          }
          else if(color_arguments[i] == "curve_color"){
            registered_color <- default_curve_color
          }
          else if(color_arguments[i] == "point_fill"){
            registered_color <- default_point_fill
          }
          else if(color_arguments[i] == "point_border"){
            registered_color <- default_point_border
          }
          else if(color_arguments[i] == "comparison_fill"){
            registered_color <- default_comparison_fill
          }
          else if(color_arguments[i] == "comparison_text_color"){
            registered_color <- default_comparison_text_color
          }
          else if(color_arguments[i] == "legend_fill"){
            registered_color <- default_legend_fill
          }
          else if(color_arguments[i] == "legend_text_color"){
            registered_color <- default_legend_text_color
          }
          else{
            stop("Something went wrong... How?")
          }
        }
      }
      additional_arguments[[which(color_arguments[i] == given_arguments)]] <- registered_color
    }

  }

  include_line <- 0

  if(any("curve" == given_arguments)){
    if(any("curve_type" == given_arguments)){
      if(additional_arguments$curve_type == "equivalence_curve"){
        include_line <- 1
      }
    }
    else{
      include_line <- 0
    }
  }

  include_linear_smooth <- FALSE

  if(any("curve" == given_arguments)){
    if(any("curve_type" == given_arguments)){
      if(additional_arguments$curve_type == "fitted_curve"){
        include_linear_smooth <- TRUE
      }
    }
    else{
      include_linear_smooth <- FALSE
    }
  }

  include_flexible_smooth <- FALSE

  if(any("curve" == given_arguments)){
    if(any("curve_type" == given_arguments)){
      if(additional_arguments$curve_type == "flexible_curve"){
        include_flexible_smooth <- TRUE
      }
    }
    else{
      include_flexible_smooth <- FALSE
    }
  }


  if(!exclude_cs & !exclude_rings){

    if(include_flexible_smooth){
      ggplot() +
        geom_ribbon(data = pb_data,
                    mapping = aes(x = predictor, ymin = pi_lwr, ymax = pi_upr, fill = dins_conclusion),
                    alpha = 0.5,
                    color = if(any("pb_border" == given_arguments)){additional_arguments$pb_border}else{default_pb_border},
                    na.rm = TRUE,
                    outline.type = "full") +
        facet_wrap(facets = . ~ comparison, scales = "free") +
        geom_smooth(data = cs_data,
                    mapping = aes(x = MP_B, y = MP_A),
                    color = if(any("curve_color" == given_arguments)){additional_arguments$curve_color}else{default_curve_color},
                    method = "loess",
                    se = FALSE,
                    na.rm = TRUE,
                    span = 0.95,
                    linetype = "dotted",
                    formula = y ~ x) +
        geom_point(data = cs_data,
                   mapping = aes(x = MP_B, y = MP_A),
                   shape = if(any("point_shape" == given_arguments)){if(additional_arguments$point_shape == "circle"){21}else if(additional_arguments$point_shape == "square"){22}else if(additional_arguments$point_shape == "diamond"){23}else if(additional_arguments$point_shape == "triangle"){24}else{21}}else{21},
                   fill = if(any("point_fill" == given_arguments)){additional_arguments$point_fill}else{default_point_fill},
                   size = if(any("point_size" == given_arguments)){additional_arguments$point_size}else{default_point_size},
                   color = if(any("point_border" == given_arguments)){additional_arguments$point_border}else{default_point_border}) +
        geom_segment(data = ce_data, mapping = aes(x = MS_B, xend = MS_B, y = pi_lwr, yend = pi_upr), arrow = arrow(angle = 90, ends = "both", length = unit(x = 0.025, units = "npc"))) +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 3, alpha = 0.3) + theme_bw() +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 2.75, alpha = 0.4) +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 2.50, alpha = 0.5) +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 2.25, alpha = 0.75) +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 2.00, alpha = 0.90) +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 1.50, alpha = 1) +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, shape = SampleID), size = 0.50, alpha = 1, color = "black") +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, alpha = pi_conclusion_correctness, color = pi_inside), shape = 1, size = 5, show.legend = FALSE) +
        labs(title = if(any("main_title" == given_arguments)){additional_arguments$main_title}else{default_main_title},
             subtitle = if(any("sub_title" == given_arguments)){additional_arguments$sub_title}else{default_sub_title},
             color = "Inside prediction interval",
             shape = "Evaluated materials' ID",
             fill = "Difference in non-selectivity") +
        scale_x_continuous(name = if(any("x_name" == given_arguments)){additional_arguments$x_name}else{default_x_name},
                           n.breaks = if(any("n_breaks" == given_arguments)){additional_arguments$n_breaks}else{default_n_breaks}) +
        scale_y_continuous(name = if(any("y_name" == given_arguments)){additional_arguments$y_name}else{default_y_name},
                           n.breaks = if(any("n_breaks" == given_arguments)){additional_arguments$n_breaks}else{default_n_breaks}) +
        scale_fill_manual(values = c("acceptable" = if(any("pb_fill" == given_arguments)){additional_arguments$pb_fill}else{default_pb_fill}, "not acceptable" = "gray")) +
        scale_color_manual(values = c("yes" = "#1994DC", "no" = "#DC1932")) +
        scale_alpha_binned(name = "Conclusion correctness", limits = c(0, 1), n.breaks = 30, range = c(0, 1)) +
        theme(strip.background = element_rect(fill = if(any("comparison_fill" == given_arguments)){additional_arguments$comparison_fill}else{default_comparison_fill},
                                              color = "#000000",
                                              size = 1),
              strip.text = element_text(face = "bold",
                                        color = if(any("comparison_text_color" == given_arguments)){additional_arguments$comparison_text_color}else{default_comparison_text_color}),
              legend.background = element_rect(fill = if(any("legend_fill" == given_arguments)){additional_arguments$legend_fill}else{default_legend_fill},
                                               color = "#000000"),
              legend.key = element_rect(fill = "white", color = "black"),
              legend.text = element_text(color = if(any("legend_text_color" == given_arguments)){additional_arguments$legend_text_color}else{default_legend_text_color}),
              title = element_text(face = "bold",
                                   color = if(any("title_color" == given_arguments)){additional_arguments$legend_text_color}else{default_legend_text_color}),
              plot.title = element_text(hjust = 0.5),
              plot.subtitle = element_text(hjust = 0.5))
    }
    else if(include_linear_smooth){
      ggplot() +
        geom_ribbon(data = pb_data,
                    mapping = aes(x = predictor, ymin = pi_lwr, ymax = pi_upr, fill = dins_conclusion),
                    alpha = 0.5,
                    color = if(any("pb_border" == given_arguments)){additional_arguments$pb_border}else{default_pb_border},
                    na.rm = TRUE,
                    outline.type = "full") +
        facet_wrap(facets = . ~ comparison, scales = "free") +
        geom_smooth(data = cs_data,
                    mapping = aes(x = MP_B, y = MP_A),
                    alpha = include_linear_smooth,
                    color = if(any("curve_color" == given_arguments)){additional_arguments$curve_color}else{default_curve_color},
                    se = FALSE,
                    na.rm = TRUE,
                    linetype = "dotted",
                    method = "lm",
                    formula = y ~ x) +
        geom_point(data = cs_data,
                   mapping = aes(x = MP_B, y = MP_A),
                   shape = if(any("point_shape" == given_arguments)){if(additional_arguments$point_shape == "circle"){21}else if(additional_arguments$point_shape == "square"){22}else if(additional_arguments$point_shape == "diamond"){23}else if(additional_arguments$point_shape == "triangle"){24}else{21}}else{21},
                   fill = if(any("point_fill" == given_arguments)){additional_arguments$point_fill}else{default_point_fill},
                   size = if(any("point_size" == given_arguments)){additional_arguments$point_size}else{default_point_size},
                   color = if(any("point_border" == given_arguments)){additional_arguments$point_border}else{default_point_border}) +

        geom_segment(data = ce_data, mapping = aes(x = MS_B, xend = MS_B, y = pi_lwr, yend = pi_upr), arrow = arrow(angle = 90, ends = "both", length = unit(x = 0.025, units = "npc"))) +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 3, alpha = 0.3) + theme_bw() +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 2.75, alpha = 0.4) +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 2.50, alpha = 0.5) +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 2.25, alpha = 0.75) +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 2.00, alpha = 0.90) +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 1.50, alpha = 1) +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, shape = SampleID), size = 0.50, alpha = 1, color = "black") +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, alpha = pi_conclusion_correctness, color = pi_inside), shape = 1, size = 5, show.legend = FALSE) +
        labs(title = if(any("main_title" == given_arguments)){additional_arguments$main_title}else{default_main_title},
             subtitle = if(any("sub_title" == given_arguments)){additional_arguments$sub_title}else{default_sub_title},
             color = "Inside prediction interval",
             shape = "Evaluated materials' ID",
             fill = "Difference in non-selectivity") +
        scale_x_continuous(name = if(any("x_name" == given_arguments)){additional_arguments$x_name}else{default_x_name},
                           n.breaks = if(any("n_breaks" == given_arguments)){additional_arguments$n_breaks}else{default_n_breaks}) +
        scale_y_continuous(name = if(any("y_name" == given_arguments)){additional_arguments$y_name}else{default_y_name},
                           n.breaks = if(any("n_breaks" == given_arguments)){additional_arguments$n_breaks}else{default_n_breaks}) +
        scale_fill_manual(values = c("acceptable" = if(any("pb_fill" == given_arguments)){additional_arguments$pb_fill}else{default_pb_fill}, "not acceptable" = "gray")) +
        scale_color_manual(values = c("yes" = "#1994DC", "no" = "#DC1932")) +
        scale_alpha_binned(name = "Conclusion correctness", limits = c(0, 1), n.breaks = 30, range = c(0, 1)) +
        theme(strip.background = element_rect(fill = if(any("comparison_fill" == given_arguments)){additional_arguments$comparison_fill}else{default_comparison_fill},
                                              color = "#000000",
                                              size = 1),
              strip.text = element_text(face = "bold",
                                        color = if(any("comparison_text_color" == given_arguments)){additional_arguments$comparison_text_color}else{default_comparison_text_color}),
              legend.background = element_rect(fill = if(any("legend_fill" == given_arguments)){additional_arguments$legend_fill}else{default_legend_fill},
                                               color = "#000000"),
              legend.key = element_rect(fill = "white", color = "black"),
              legend.text = element_text(color = if(any("legend_text_color" == given_arguments)){additional_arguments$legend_text_color}else{default_legend_text_color}),
              title = element_text(face = "bold",
                                   color = if(any("title_color" == given_arguments)){additional_arguments$legend_text_color}else{default_legend_text_color}),
              plot.title = element_text(hjust = 0.5),
              plot.subtitle = element_text(hjust = 0.5))
    }
    else{
      ggplot() +
        geom_ribbon(data = pb_data,
                    mapping = aes(x = predictor, ymin = pi_lwr, ymax = pi_upr, fill = dins_conclusion),
                    alpha = 0.5,
                    color = if(any("pb_border" == given_arguments)){additional_arguments$pb_border}else{default_pb_border},
                    na.rm = TRUE,
                    outline.type = "full") +
        facet_wrap(facets = . ~ comparison, scales = "free") +
        geom_abline(slope = 1, intercept = 0,
                    alpha = include_line,
                    color = if(any("curve_color" == given_arguments)){additional_arguments$curve_color}else{default_curve_color},
                    linetype = "dotted") +
        geom_point(data = cs_data,
                   mapping = aes(x = MP_B, y = MP_A),
                   shape = if(any("point_shape" == given_arguments)){if(additional_arguments$point_shape == "circle"){21}else if(additional_arguments$point_shape == "square"){22}else if(additional_arguments$point_shape == "diamond"){23}else if(additional_arguments$point_shape == "triangle"){24}else{21}}else{21},
                   fill = if(any("point_fill" == given_arguments)){additional_arguments$point_fill}else{default_point_fill},
                   size = if(any("point_size" == given_arguments)){additional_arguments$point_size}else{default_point_size},
                   color = if(any("point_border" == given_arguments)){additional_arguments$point_border}else{default_point_border}) +
        geom_segment(data = ce_data, mapping = aes(x = MS_B, xend = MS_B, y = pi_lwr, yend = pi_upr), arrow = arrow(angle = 90, ends = "both", length = unit(x = 0.025, units = "npc"))) +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 3, alpha = 0.3) + theme_bw() +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 2.75, alpha = 0.4) +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 2.50, alpha = 0.5) +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 2.25, alpha = 0.75) +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 2.00, alpha = 0.90) +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 1.50, alpha = 1) +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, shape = SampleID), size = 0.50, alpha = 1, color = "black") +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, alpha = pi_conclusion_correctness, color = pi_inside), shape = 1, size = 5, show.legend = FALSE) +
        labs(title = if(any("main_title" == given_arguments)){additional_arguments$main_title}else{default_main_title},
             subtitle = if(any("sub_title" == given_arguments)){additional_arguments$sub_title}else{default_sub_title},
             color = "Inside prediction interval",
             shape = "Evaluated materials' ID",
             fill = "Difference in non-selectivity") +
        scale_x_continuous(name = if(any("x_name" == given_arguments)){additional_arguments$x_name}else{default_x_name},
                           n.breaks = if(any("n_breaks" == given_arguments)){additional_arguments$n_breaks}else{default_n_breaks}) +
        scale_y_continuous(name = if(any("y_name" == given_arguments)){additional_arguments$y_name}else{default_y_name},
                           n.breaks = if(any("n_breaks" == given_arguments)){additional_arguments$n_breaks}else{default_n_breaks}) +
        scale_fill_manual(values = c("acceptable" = if(any("pb_fill" == given_arguments)){additional_arguments$pb_fill}else{default_pb_fill}, "not acceptable" = "gray")) +
        scale_color_manual(values = c("yes" = "#1994DC", "no" = "#DC1932")) +
        scale_alpha_binned(name = "Conclusion correctness", limits = c(0, 1), n.breaks = 30, range = c(0, 1)) +
        theme(strip.background = element_rect(fill = if(any("comparison_fill" == given_arguments)){additional_arguments$comparison_fill}else{default_comparison_fill},
                                              color = "#000000",
                                              size = 1),
              strip.text = element_text(face = "bold",
                                        color = if(any("comparison_text_color" == given_arguments)){additional_arguments$comparison_text_color}else{default_comparison_text_color}),
              legend.background = element_rect(fill = if(any("legend_fill" == given_arguments)){additional_arguments$legend_fill}else{default_legend_fill},
                                               color = "#000000"),
              legend.key = element_rect(fill = "white", color = "black"),
              legend.text = element_text(color = if(any("legend_text_color" == given_arguments)){additional_arguments$legend_text_color}else{default_legend_text_color}),
              title = element_text(face = "bold",
                                   color = if(any("title_color" == given_arguments)){additional_arguments$legend_text_color}else{default_legend_text_color}),
              plot.title = element_text(hjust = 0.5),
              plot.subtitle = element_text(hjust = 0.5))
    }


  }
  else if(!exclude_cs & exclude_rings){


    if(include_flexible_smooth){
      ggplot() +
        geom_ribbon(data = pb_data,
                    mapping = aes(x = predictor, ymin = pi_lwr, ymax = pi_upr, fill = dins_conclusion),
                    alpha = 0.5,
                    color = if(any("pb_border" == given_arguments)){additional_arguments$pb_border}else{default_pb_border},
                    na.rm = TRUE,
                    outline.type = "full") +
        facet_wrap(facets = . ~ comparison, scales = "free") +
        geom_smooth(data = cs_data,
                    mapping = aes(x = MP_B, y = MP_A),
                    alpha = include_flexible_smooth,
                    color = if(any("curve_color" == given_arguments)){additional_arguments$curve_color}else{default_curve_color},
                    method = "loess",
                    se = FALSE,
                    na.rm = TRUE,
                    linetype = "dotted",
                    span = 0.95,
                    formula = y ~ x) +
        geom_point(data = cs_data,
                   mapping = aes(x = MP_B, y = MP_A), na.rm = TRUE,
                   shape = if(any("point_shape" == given_arguments)){if(additional_arguments$point_shape == "circle"){21}else if(additional_arguments$point_shape == "square"){22}else if(additional_arguments$point_shape == "diamond"){23}else if(additional_arguments$point_shape == "triangle"){24}else{21}}else{21},
                   fill = if(any("point_fill" == given_arguments)){additional_arguments$point_fill}else{default_point_fill},
                   size = if(any("point_size" == given_arguments)){additional_arguments$point_size}else{default_point_size},
                   color = if(any("point_border" == given_arguments)){additional_arguments$point_border}else{default_point_border}) +
        geom_segment(data = ce_data, mapping = aes(x = MS_B, xend = MS_B, y = pi_lwr, yend = pi_upr), arrow = arrow(angle = 90, ends = "both", length = unit(x = 0.025, units = "npc"))) +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 3, alpha = 0.3) + theme_bw() +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 2.75, alpha = 0.4) +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 2.50, alpha = 0.5) +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 2.25, alpha = 0.75) +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 2.00, alpha = 0.90) +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 1.50, alpha = 1) +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, shape = SampleID), size = 0.50, alpha = 1, color = "black") +
        labs(title = if(any("main_title" == given_arguments)){additional_arguments$main_title}else{default_main_title},
             subtitle = if(any("sub_title" == given_arguments)){additional_arguments$sub_title}else{default_sub_title},
             color = "Inside prediction interval",
             shape = "Evaluated materials' ID",
             fill = "Difference in non-selectivity") +
        scale_x_continuous(name = if(any("x_name" == given_arguments)){additional_arguments$x_name}else{default_x_name},
                           n.breaks = if(any("n_breaks" == given_arguments)){additional_arguments$n_breaks}else{default_n_breaks}) +
        scale_y_continuous(name = if(any("y_name" == given_arguments)){additional_arguments$y_name}else{default_y_name},
                           n.breaks = if(any("n_breaks" == given_arguments)){additional_arguments$n_breaks}else{default_n_breaks}) +
        scale_fill_manual(values = c("acceptable" = if(any("pb_fill" == given_arguments)){additional_arguments$pb_fill}else{default_pb_fill}, "not acceptable" = "gray")) +
        scale_color_manual(values = c("yes" = "#1994DC", "no" = "#DC1932")) +
        scale_alpha_binned(name = "Conclusion correctness", limits = c(0, 1), n.breaks = 30, range = c(0, 1)) +
        theme(strip.background = element_rect(fill = if(any("comparison_fill" == given_arguments)){additional_arguments$comparison_fill}else{default_comparison_fill},
                                              color = "#000000",
                                              size = 1),
              strip.text = element_text(face = "bold",
                                        color = if(any("comparison_text_color" == given_arguments)){additional_arguments$comparison_text_color}else{default_comparison_text_color}),
              legend.background = element_rect(fill = if(any("legend_fill" == given_arguments)){additional_arguments$legend_fill}else{default_legend_fill},
                                               color = "#000000"),
              legend.text = element_text(color = if(any("legend_text_color" == given_arguments)){additional_arguments$legend_text_color}else{default_legend_text_color}),
              legend.key = element_rect(fill = "white", color = "black"),
              title = element_text(face = "bold",
                                   color = if(any("title_color" == given_arguments)){additional_arguments$legend_text_color}else{default_legend_text_color}),
              plot.title = element_text(hjust = 0.5),
              plot.subtitle = element_text(hjust = 0.5))
    }
    else if(include_linear_smooth){
      ggplot() +
        geom_ribbon(data = pb_data,
                    mapping = aes(x = predictor, ymin = pi_lwr, ymax = pi_upr, fill = dins_conclusion),
                    alpha = 0.5,
                    color = if(any("pb_border" == given_arguments)){additional_arguments$pb_border}else{default_pb_border},
                    na.rm = TRUE,
                    outline.type = "full") +
        facet_wrap(facets = . ~ comparison, scales = "free") +
        geom_smooth(data = cs_data,
                    mapping = aes(x = MP_B, y = MP_A),
                    alpha = include_linear_smooth,
                    color = if(any("curve_color" == given_arguments)){additional_arguments$curve_color}else{default_curve_color},
                    se = FALSE,
                    na.rm = TRUE,
                    linetype = "dotted",
                    method = "lm",
                    formula = y ~ x) +
        geom_point(data = cs_data,
                   mapping = aes(x = MP_B, y = MP_A), na.rm = TRUE,
                   shape = if(any("point_shape" == given_arguments)){if(additional_arguments$point_shape == "circle"){21}else if(additional_arguments$point_shape == "square"){22}else if(additional_arguments$point_shape == "diamond"){23}else if(additional_arguments$point_shape == "triangle"){24}else{21}}else{21},
                   fill = if(any("point_fill" == given_arguments)){additional_arguments$point_fill}else{default_point_fill},
                   size = if(any("point_size" == given_arguments)){additional_arguments$point_size}else{default_point_size},
                   color = if(any("point_border" == given_arguments)){additional_arguments$point_border}else{default_point_border}) +
        geom_segment(data = ce_data, mapping = aes(x = MS_B, xend = MS_B, y = pi_lwr, yend = pi_upr), arrow = arrow(angle = 90, ends = "both", length = unit(x = 0.025, units = "npc"))) +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 3, alpha = 0.3) + theme_bw() +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 2.75, alpha = 0.4) +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 2.50, alpha = 0.5) +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 2.25, alpha = 0.75) +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 2.00, alpha = 0.90) +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 1.50, alpha = 1) +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, shape = SampleID), size = 0.50, alpha = 1, color = "black") +
        labs(title = if(any("main_title" == given_arguments)){additional_arguments$main_title}else{default_main_title},
             subtitle = if(any("sub_title" == given_arguments)){additional_arguments$sub_title}else{default_sub_title},
             color = "Inside prediction interval",
             shape = "Evaluated materials' ID",
             fill = "Difference in non-selectivity") +
        scale_x_continuous(name = if(any("x_name" == given_arguments)){additional_arguments$x_name}else{default_x_name},
                           n.breaks = if(any("n_breaks" == given_arguments)){additional_arguments$n_breaks}else{default_n_breaks}) +
        scale_y_continuous(name = if(any("y_name" == given_arguments)){additional_arguments$y_name}else{default_y_name},
                           n.breaks = if(any("n_breaks" == given_arguments)){additional_arguments$n_breaks}else{default_n_breaks}) +
        scale_fill_manual(values = c("acceptable" = if(any("pb_fill" == given_arguments)){additional_arguments$pb_fill}else{default_pb_fill}, "not acceptable" = "gray")) +
        scale_color_manual(values = c("yes" = "#1994DC", "no" = "#DC1932")) +
        scale_alpha_binned(name = "Conclusion correctness", limits = c(0, 1), n.breaks = 30, range = c(0, 1)) +
        theme(strip.background = element_rect(fill = if(any("comparison_fill" == given_arguments)){additional_arguments$comparison_fill}else{default_comparison_fill},
                                              color = "#000000",
                                              size = 1),
              strip.text = element_text(face = "bold",
                                        color = if(any("comparison_text_color" == given_arguments)){additional_arguments$comparison_text_color}else{default_comparison_text_color}),
              legend.background = element_rect(fill = if(any("legend_fill" == given_arguments)){additional_arguments$legend_fill}else{default_legend_fill},
                                               color = "#000000"),
              legend.text = element_text(color = if(any("legend_text_color" == given_arguments)){additional_arguments$legend_text_color}else{default_legend_text_color}),
              legend.key = element_rect(fill = "white", color = "black"),
              title = element_text(face = "bold",
                                   color = if(any("title_color" == given_arguments)){additional_arguments$legend_text_color}else{default_legend_text_color}),
              plot.title = element_text(hjust = 0.5),
              plot.subtitle = element_text(hjust = 0.5))
    }
    else{
      ggplot() +
        geom_ribbon(data = pb_data,
                    mapping = aes(x = predictor, ymin = pi_lwr, ymax = pi_upr, fill = dins_conclusion),
                    alpha = 0.5,
                    color = if(any("pb_border" == given_arguments)){additional_arguments$pb_border}else{default_pb_border},
                    na.rm = TRUE,
                    outline.type = "full") +
        facet_wrap(facets = . ~ comparison, scales = "free") +
        geom_abline(slope = 1, intercept = 0,
                    alpha = include_line,
                    color = if(any("curve_color" == given_arguments)){additional_arguments$curve_color}else{default_curve_color},
                    linetype = "dotted") +
        geom_point(data = cs_data,
                   mapping = aes(x = MP_B, y = MP_A), na.rm = TRUE,
                   shape = if(any("point_shape" == given_arguments)){if(additional_arguments$point_shape == "circle"){21}else if(additional_arguments$point_shape == "square"){22}else if(additional_arguments$point_shape == "diamond"){23}else if(additional_arguments$point_shape == "triangle"){24}else{21}}else{21},
                   fill = if(any("point_fill" == given_arguments)){additional_arguments$point_fill}else{default_point_fill},
                   size = if(any("point_size" == given_arguments)){additional_arguments$point_size}else{default_point_size},
                   color = if(any("point_border" == given_arguments)){additional_arguments$point_border}else{default_point_border}) +
        geom_segment(data = ce_data, mapping = aes(x = MS_B, xend = MS_B, y = pi_lwr, yend = pi_upr), arrow = arrow(angle = 90, ends = "both", length = unit(x = 0.025, units = "npc"))) +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 3, alpha = 0.3) + theme_bw() +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 2.75, alpha = 0.4) +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 2.50, alpha = 0.5) +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 2.25, alpha = 0.75) +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 2.00, alpha = 0.90) +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 1.50, alpha = 1) +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, shape = SampleID), size = 0.50, alpha = 1, color = "black") +
        labs(title = if(any("main_title" == given_arguments)){additional_arguments$main_title}else{default_main_title},
             subtitle = if(any("sub_title" == given_arguments)){additional_arguments$sub_title}else{default_sub_title},
             color = "Inside prediction interval",
             shape = "Evaluated materials' ID",
             fill = "Difference in non-selectivity") +
        scale_x_continuous(name = if(any("x_name" == given_arguments)){additional_arguments$x_name}else{default_x_name},
                           n.breaks = if(any("n_breaks" == given_arguments)){additional_arguments$n_breaks}else{default_n_breaks}) +
        scale_y_continuous(name = if(any("y_name" == given_arguments)){additional_arguments$y_name}else{default_y_name},
                           n.breaks = if(any("n_breaks" == given_arguments)){additional_arguments$n_breaks}else{default_n_breaks}) +
        scale_fill_manual(values = c("acceptable" = if(any("pb_fill" == given_arguments)){additional_arguments$pb_fill}else{default_pb_fill}, "not acceptable" = "gray")) +
        scale_color_manual(values = c("yes" = "#1994DC", "no" = "#DC1932")) +
        scale_alpha_binned(name = "Conclusion correctness", limits = c(0, 1), n.breaks = 30, range = c(0, 1)) +
        theme(strip.background = element_rect(fill = if(any("comparison_fill" == given_arguments)){additional_arguments$comparison_fill}else{default_comparison_fill},
                                              color = "#000000",
                                              size = 1),
              strip.text = element_text(face = "bold",
                                        color = if(any("comparison_text_color" == given_arguments)){additional_arguments$comparison_text_color}else{default_comparison_text_color}),
              legend.background = element_rect(fill = if(any("legend_fill" == given_arguments)){additional_arguments$legend_fill}else{default_legend_fill},
                                               color = "#000000"),
              legend.text = element_text(color = if(any("legend_text_color" == given_arguments)){additional_arguments$legend_text_color}else{default_legend_text_color}),
              legend.key = element_rect(fill = "white", color = "black"),
              title = element_text(face = "bold",
                                   color = if(any("title_color" == given_arguments)){additional_arguments$legend_text_color}else{default_legend_text_color}),
              plot.title = element_text(hjust = 0.5),
              plot.subtitle = element_text(hjust = 0.5))
    }


  }
  else if(exclude_cs & !exclude_rings){

    if(include_flexible_smooth){
      ggplot() +
        geom_ribbon(data = pb_data,
                    mapping = aes(x = predictor, ymin = pi_lwr, ymax = pi_upr, fill = dins_conclusion),
                    alpha = 0.5,
                    color = if(any("pb_border" == given_arguments)){additional_arguments$pb_border}else{default_pb_border},
                    na.rm = TRUE,
                    outline.type = "full") +
        facet_wrap(facets = . ~ comparison, scales = "free") +
        geom_smooth(data = cs_data,
                    mapping = aes(x = MP_B, y = MP_A),
                    na.rm = TRUE,
                    alpha = include_flexible_smooth,
                    color = if(any("curve_color" == given_arguments)){additional_arguments$curve_color}else{default_curve_color},
                    method = "loess",
                    se = FALSE,
                    linetype = "dotted",
                    span = 0.95,
                    formula = y ~ x) +
        geom_segment(data = ce_data, mapping = aes(x = MS_B, xend = MS_B, y = pi_lwr, yend = pi_upr), arrow = arrow(angle = 90, ends = "both", length = unit(x = 0.025, units = "npc"))) +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 3, alpha = 0.3) + theme_bw() +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 2.75, alpha = 0.4) +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 2.50, alpha = 0.5) +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 2.25, alpha = 0.75) +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 2.00, alpha = 0.90) +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 1.50, alpha = 1) +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, shape = SampleID), size = 0.50, alpha = 1, color = "black") +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, alpha = pi_conclusion_correctness, color = pi_inside), shape = 1, size = 5, show.legend = FALSE) +
        labs(title = if(any("main_title" == given_arguments)){additional_arguments$main_title}else{default_main_title},
             subtitle = if(any("sub_title" == given_arguments)){additional_arguments$sub_title}else{default_sub_title},
             color = "Inside prediction interval",
             shape = "Evaluated materials' ID",
             fill = "Difference in non-selectivity") +
        scale_x_continuous(name = if(any("x_name" == given_arguments)){additional_arguments$x_name}else{default_x_name},
                           n.breaks = if(any("n_breaks" == given_arguments)){additional_arguments$n_breaks}else{default_n_breaks}) +
        scale_y_continuous(name = if(any("y_name" == given_arguments)){additional_arguments$y_name}else{default_y_name},
                           n.breaks = if(any("n_breaks" == given_arguments)){additional_arguments$n_breaks}else{default_n_breaks}) +
        scale_fill_manual(values = c("acceptable" = if(any("pb_fill" == given_arguments)){additional_arguments$pb_fill}else{default_pb_fill}, "not acceptable" = "gray")) +
        scale_color_manual(values = c("yes" = "#1994DC", "no" = "#DC1932")) +
        scale_alpha_binned(name = "Conclusion correctness", limits = c(0, 1), n.breaks = 30, range = c(0, 1)) +
        theme(strip.background = element_rect(fill = if(any("comparison_fill" == given_arguments)){additional_arguments$comparison_fill}else{default_comparison_fill},
                                              color = "#000000",
                                              size = 1),
              strip.text = element_text(face = "bold",
                                        color = if(any("comparison_text_color" == given_arguments)){additional_arguments$comparison_text_color}else{default_comparison_text_color}),
              legend.background = element_rect(fill = if(any("legend_fill" == given_arguments)){additional_arguments$legend_fill}else{default_legend_fill},
                                               color = "#000000"),
              legend.key = element_rect(fill = "white", color = "black"),
              legend.text = element_text(color = if(any("legend_text_color" == given_arguments)){additional_arguments$legend_text_color}else{default_legend_text_color}),
              title = element_text(face = "bold",
                                   color = if(any("title_color" == given_arguments)){additional_arguments$legend_text_color}else{default_legend_text_color}),
              plot.title = element_text(hjust = 0.5),
              plot.subtitle = element_text(hjust = 0.5))
    }
    else if(include_linear_smooth){
      ggplot() +
        geom_ribbon(data = pb_data,
                    mapping = aes(x = predictor, ymin = pi_lwr, ymax = pi_upr, fill = dins_conclusion),
                    alpha = 0.5,
                    color = if(any("pb_border" == given_arguments)){additional_arguments$pb_border}else{default_pb_border},
                    na.rm = TRUE,
                    outline.type = "full") +
        facet_wrap(facets = . ~ comparison, scales = "free") +
        geom_smooth(data = cs_data,
                    mapping = aes(x = MP_B, y = MP_A),
                    na.rm = TRUE,
                    alpha = include_linear_smooth,
                    color = if(any("curve_color" == given_arguments)){additional_arguments$curve_color}else{default_curve_color},
                    se = FALSE,
                    linetype = "dotted",
                    method = "lm",
                    formula = y ~ x) +
        geom_segment(data = ce_data, mapping = aes(x = MS_B, xend = MS_B, y = pi_lwr, yend = pi_upr), arrow = arrow(angle = 90, ends = "both", length = unit(x = 0.025, units = "npc"))) +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 3, alpha = 0.3) + theme_bw() +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 2.75, alpha = 0.4) +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 2.50, alpha = 0.5) +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 2.25, alpha = 0.75) +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 2.00, alpha = 0.90) +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 1.50, alpha = 1) +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, shape = SampleID), size = 0.50, alpha = 1, color = "black") +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, alpha = pi_conclusion_correctness, color = pi_inside), shape = 1, size = 5, show.legend = FALSE) +
        labs(title = if(any("main_title" == given_arguments)){additional_arguments$main_title}else{default_main_title},
             subtitle = if(any("sub_title" == given_arguments)){additional_arguments$sub_title}else{default_sub_title},
             color = "Inside prediction interval",
             shape = "Evaluated materials' ID",
             fill = "Difference in non-selectivity") +
        scale_x_continuous(name = if(any("x_name" == given_arguments)){additional_arguments$x_name}else{default_x_name},
                           n.breaks = if(any("n_breaks" == given_arguments)){additional_arguments$n_breaks}else{default_n_breaks}) +
        scale_y_continuous(name = if(any("y_name" == given_arguments)){additional_arguments$y_name}else{default_y_name},
                           n.breaks = if(any("n_breaks" == given_arguments)){additional_arguments$n_breaks}else{default_n_breaks}) +
        scale_fill_manual(values = c("acceptable" = if(any("pb_fill" == given_arguments)){additional_arguments$pb_fill}else{default_pb_fill}, "not acceptable" = "gray")) +
        scale_color_manual(values = c("yes" = "#1994DC", "no" = "#DC1932")) +
        scale_alpha_binned(name = "Conclusion correctness", limits = c(0, 1), n.breaks = 30, range = c(0, 1)) +
        theme(strip.background = element_rect(fill = if(any("comparison_fill" == given_arguments)){additional_arguments$comparison_fill}else{default_comparison_fill},
                                              color = "#000000",
                                              size = 1),
              strip.text = element_text(face = "bold",
                                        color = if(any("comparison_text_color" == given_arguments)){additional_arguments$comparison_text_color}else{default_comparison_text_color}),
              legend.background = element_rect(fill = if(any("legend_fill" == given_arguments)){additional_arguments$legend_fill}else{default_legend_fill},
                                               color = "#000000"),
              legend.key = element_rect(fill = "white", color = "black"),
              legend.text = element_text(color = if(any("legend_text_color" == given_arguments)){additional_arguments$legend_text_color}else{default_legend_text_color}),
              title = element_text(face = "bold",
                                   color = if(any("title_color" == given_arguments)){additional_arguments$legend_text_color}else{default_legend_text_color}),
              plot.title = element_text(hjust = 0.5),
              plot.subtitle = element_text(hjust = 0.5))
    }
    else{
      ggplot() +
        geom_ribbon(data = pb_data,
                    mapping = aes(x = predictor, ymin = pi_lwr, ymax = pi_upr, fill = dins_conclusion),
                    alpha = 0.5,
                    color = if(any("pb_border" == given_arguments)){additional_arguments$pb_border}else{default_pb_border},
                    na.rm = TRUE,
                    outline.type = "full") +
        facet_wrap(facets = . ~ comparison, scales = "free") +
        geom_abline(slope = 1, intercept = 0,
                    alpha = include_line,
                    color = if(any("curve_color" == given_arguments)){additional_arguments$curve_color}else{default_curve_color},
                    linetype = "dotted") +
        geom_segment(data = ce_data, mapping = aes(x = MS_B, xend = MS_B, y = pi_lwr, yend = pi_upr), arrow = arrow(angle = 90, ends = "both", length = unit(x = 0.025, units = "npc"))) +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 3, alpha = 0.3) + theme_bw() +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 2.75, alpha = 0.4) +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 2.50, alpha = 0.5) +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 2.25, alpha = 0.75) +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 2.00, alpha = 0.90) +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 1.50, alpha = 1) +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, shape = SampleID), size = 0.50, alpha = 1, color = "black") +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, alpha = pi_conclusion_correctness, color = pi_inside), shape = 1, size = 5, show.legend = FALSE) +
        labs(title = if(any("main_title" == given_arguments)){additional_arguments$main_title}else{default_main_title},
             subtitle = if(any("sub_title" == given_arguments)){additional_arguments$sub_title}else{default_sub_title},
             color = "Inside prediction interval",
             shape = "Evaluated materials' ID",
             fill = "Difference in non-selectivity") +
        scale_x_continuous(name = if(any("x_name" == given_arguments)){additional_arguments$x_name}else{default_x_name},
                           n.breaks = if(any("n_breaks" == given_arguments)){additional_arguments$n_breaks}else{default_n_breaks}) +
        scale_y_continuous(name = if(any("y_name" == given_arguments)){additional_arguments$y_name}else{default_y_name},
                           n.breaks = if(any("n_breaks" == given_arguments)){additional_arguments$n_breaks}else{default_n_breaks}) +
        scale_fill_manual(values = c("acceptable" = if(any("pb_fill" == given_arguments)){additional_arguments$pb_fill}else{default_pb_fill}, "not acceptable" = "gray")) +
        scale_color_manual(values = c("yes" = "#1994DC", "no" = "#DC1932")) +
        scale_alpha_binned(name = "Conclusion correctness", limits = c(0, 1), n.breaks = 30, range = c(0, 1)) +
        theme(strip.background = element_rect(fill = if(any("comparison_fill" == given_arguments)){additional_arguments$comparison_fill}else{default_comparison_fill},
                                              color = "#000000",
                                              size = 1),
              strip.text = element_text(face = "bold",
                                        color = if(any("comparison_text_color" == given_arguments)){additional_arguments$comparison_text_color}else{default_comparison_text_color}),
              legend.background = element_rect(fill = if(any("legend_fill" == given_arguments)){additional_arguments$legend_fill}else{default_legend_fill},
                                               color = "#000000"),
              legend.key = element_rect(fill = "white", color = "black"),
              legend.text = element_text(color = if(any("legend_text_color" == given_arguments)){additional_arguments$legend_text_color}else{default_legend_text_color}),
              title = element_text(face = "bold",
                                   color = if(any("title_color" == given_arguments)){additional_arguments$legend_text_color}else{default_legend_text_color}),
              plot.title = element_text(hjust = 0.5),
              plot.subtitle = element_text(hjust = 0.5))
    }

  }

  else if(exclude_cs & exclude_rings){
    if(include_flexible_smooth){
      ggplot() +
        geom_ribbon(data = pb_data,
                    mapping = aes(x = predictor, ymin = pi_lwr, ymax = pi_upr, fill = dins_conclusion),
                    alpha = 0.5,
                    color = if(any("pb_border" == given_arguments)){additional_arguments$pb_border}else{default_pb_border},
                    na.rm = TRUE,
                    outline.type = "full") +
        facet_wrap(facets = . ~ comparison, scales = "free") +
        geom_smooth(data = cs_data,
                    mapping = aes(x = MP_B, y = MP_A),
                    alpha = include_flexible_smooth,
                    color = if(any("curve_color" == given_arguments)){additional_arguments$curve_color}else{default_curve_color},
                    method = "loess",
                    se = FALSE,
                    linetype = "dotted",
                    span = 0.95,
                    formula = y ~ x) +
        geom_segment(data = ce_data, mapping = aes(x = MS_B, xend = MS_B, y = pi_lwr, yend = pi_upr), arrow = arrow(angle = 90, ends = "both", length = unit(x = 0.025, units = "npc"))) +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 3, alpha = 0.3) + theme_bw() +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 2.75, alpha = 0.4) +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 2.50, alpha = 0.5) +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 2.25, alpha = 0.75) +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 2.00, alpha = 0.90) +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 1.50, alpha = 1) +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, shape = SampleID), size = 0.50, alpha = 1, color = "black") +
        labs(title = if(any("main_title" == given_arguments)){additional_arguments$main_title}else{default_main_title},
             subtitle = if(any("sub_title" == given_arguments)){additional_arguments$sub_title}else{default_sub_title},
             color = "Inside prediction interval",
             shape = "Evaluated materials' ID",
             fill = "Difference in non-selectivity") +
        scale_x_continuous(name = if(any("x_name" == given_arguments)){additional_arguments$x_name}else{default_x_name},
                           n.breaks = if(any("n_breaks" == given_arguments)){additional_arguments$n_breaks}else{default_n_breaks}) +
        scale_y_continuous(name = if(any("y_name" == given_arguments)){additional_arguments$y_name}else{default_y_name},
                           n.breaks = if(any("n_breaks" == given_arguments)){additional_arguments$n_breaks}else{default_n_breaks}) +
        scale_fill_manual(values = c("acceptable" = if(any("pb_fill" == given_arguments)){additional_arguments$pb_fill}else{default_pb_fill}, "not acceptable" = "gray")) +
        scale_color_manual(values = c("yes" = "#1994DC", "no" = "#DC1932")) +
        scale_alpha_binned(name = "Conclusion correctness", limits = c(0, 1), n.breaks = 30, range = c(0, 1)) +
        theme(strip.background = element_rect(fill = if(any("comparison_fill" == given_arguments)){additional_arguments$comparison_fill}else{default_comparison_fill},
                                              color = "#000000",
                                              size = 1),
              strip.text = element_text(face = "bold",
                                        color = if(any("comparison_text_color" == given_arguments)){additional_arguments$comparison_text_color}else{default_comparison_text_color}),
              legend.background = element_rect(fill = if(any("legend_fill" == given_arguments)){additional_arguments$legend_fill}else{default_legend_fill},
                                               color = "#000000"),
              legend.key = element_rect(fill = "white", color = "black"),
              legend.text = element_text(color = if(any("legend_text_color" == given_arguments)){additional_arguments$legend_text_color}else{default_legend_text_color}),
              title = element_text(face = "bold",
                                   color = if(any("title_color" == given_arguments)){additional_arguments$legend_text_color}else{default_legend_text_color}),
              plot.title = element_text(hjust = 0.5),
              plot.subtitle = element_text(hjust = 0.5))
    }
    else if(include_linear_smooth){
      ggplot() +
        geom_ribbon(data = pb_data,
                    mapping = aes(x = predictor, ymin = pi_lwr, ymax = pi_upr, fill = dins_conclusion),
                    alpha = 0.5,
                    color = if(any("pb_border" == given_arguments)){additional_arguments$pb_border}else{default_pb_border},
                    na.rm = TRUE,
                    outline.type = "full") +
        facet_wrap(facets = . ~ comparison, scales = "free") +
        geom_smooth(data = cs_data,
                    mapping = aes(x = MP_B, y = MP_A),
                    alpha = include_flexible_smooth,
                    color = if(any("curve_color" == given_arguments)){additional_arguments$curve_color}else{default_curve_color},
                    se = FALSE,
                    linetype = "dotted",
                    method = "lm",
                    formula = y ~ x) +
        geom_segment(data = ce_data, mapping = aes(x = MS_B, xend = MS_B, y = pi_lwr, yend = pi_upr), arrow = arrow(angle = 90, ends = "both", length = unit(x = 0.025, units = "npc"))) +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 3, alpha = 0.3) + theme_bw() +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 2.75, alpha = 0.4) +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 2.50, alpha = 0.5) +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 2.25, alpha = 0.75) +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 2.00, alpha = 0.90) +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 1.50, alpha = 1) +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, shape = SampleID), size = 0.50, alpha = 1, color = "black") +
        labs(title = if(any("main_title" == given_arguments)){additional_arguments$main_title}else{default_main_title},
             subtitle = if(any("sub_title" == given_arguments)){additional_arguments$sub_title}else{default_sub_title},
             color = "Inside prediction interval",
             shape = "Evaluated materials' ID",
             fill = "Difference in non-selectivity") +
        scale_x_continuous(name = if(any("x_name" == given_arguments)){additional_arguments$x_name}else{default_x_name},
                           n.breaks = if(any("n_breaks" == given_arguments)){additional_arguments$n_breaks}else{default_n_breaks}) +
        scale_y_continuous(name = if(any("y_name" == given_arguments)){additional_arguments$y_name}else{default_y_name},
                           n.breaks = if(any("n_breaks" == given_arguments)){additional_arguments$n_breaks}else{default_n_breaks}) +
        scale_fill_manual(values = c("acceptable" = if(any("pb_fill" == given_arguments)){additional_arguments$pb_fill}else{default_pb_fill}, "not acceptable" = "gray")) +
        scale_color_manual(values = c("yes" = "#1994DC", "no" = "#DC1932")) +
        scale_alpha_binned(name = "Conclusion correctness", limits = c(0, 1), n.breaks = 30, range = c(0, 1)) +
        theme(strip.background = element_rect(fill = if(any("comparison_fill" == given_arguments)){additional_arguments$comparison_fill}else{default_comparison_fill},
                                              color = "#000000",
                                              size = 1),
              strip.text = element_text(face = "bold",
                                        color = if(any("comparison_text_color" == given_arguments)){additional_arguments$comparison_text_color}else{default_comparison_text_color}),
              legend.background = element_rect(fill = if(any("legend_fill" == given_arguments)){additional_arguments$legend_fill}else{default_legend_fill},
                                               color = "#000000"),
              legend.key = element_rect(fill = "white", color = "black"),
              legend.text = element_text(color = if(any("legend_text_color" == given_arguments)){additional_arguments$legend_text_color}else{default_legend_text_color}),
              title = element_text(face = "bold",
                                   color = if(any("title_color" == given_arguments)){additional_arguments$legend_text_color}else{default_legend_text_color}),
              plot.title = element_text(hjust = 0.5),
              plot.subtitle = element_text(hjust = 0.5))
    }
    else{
      ggplot() +
        geom_ribbon(data = pb_data,
                    mapping = aes(x = predictor, ymin = pi_lwr, ymax = pi_upr, fill = dins_conclusion),
                    alpha = 0.5,
                    color = if(any("pb_border" == given_arguments)){additional_arguments$pb_border}else{default_pb_border},
                    na.rm = TRUE,
                    outline.type = "full") +
        facet_wrap(facets = . ~ comparison, scales = "free") +
        geom_abline(slope = 1,
                    intercept = 0,
                    alpha = include_line,
                    color = if(any("curve_color" == given_arguments)){additional_arguments$curve_color}else{default_curve_color},
                    linetype = "dotted") +
        geom_segment(data = ce_data, mapping = aes(x = MS_B, xend = MS_B, y = pi_lwr, yend = pi_upr), arrow = arrow(angle = 90, ends = "both", length = unit(x = 0.025, units = "npc"))) +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 3, alpha = 0.3) + theme_bw() +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 2.75, alpha = 0.4) +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 2.50, alpha = 0.5) +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 2.25, alpha = 0.75) +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 2.00, alpha = 0.90) +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 1.50, alpha = 1) +
        geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, shape = SampleID), size = 0.50, alpha = 1, color = "black") +
        labs(title = if(any("main_title" == given_arguments)){additional_arguments$main_title}else{default_main_title},
             subtitle = if(any("sub_title" == given_arguments)){additional_arguments$sub_title}else{default_sub_title},
             color = "Inside prediction interval",
             shape = "Evaluated materials' ID",
             fill = "Difference in non-selectivity") +
        scale_x_continuous(name = if(any("x_name" == given_arguments)){additional_arguments$x_name}else{default_x_name},
                           n.breaks = if(any("n_breaks" == given_arguments)){additional_arguments$n_breaks}else{default_n_breaks}) +
        scale_y_continuous(name = if(any("y_name" == given_arguments)){additional_arguments$y_name}else{default_y_name},
                           n.breaks = if(any("n_breaks" == given_arguments)){additional_arguments$n_breaks}else{default_n_breaks}) +
        scale_fill_manual(values = c("acceptable" = if(any("pb_fill" == given_arguments)){additional_arguments$pb_fill}else{default_pb_fill}, "not acceptable" = "gray")) +
        scale_color_manual(values = c("yes" = "#1994DC", "no" = "#DC1932")) +
        scale_alpha_binned(name = "Conclusion correctness", limits = c(0, 1), n.breaks = 30, range = c(0, 1)) +
        theme(strip.background = element_rect(fill = if(any("comparison_fill" == given_arguments)){additional_arguments$comparison_fill}else{default_comparison_fill},
                                              color = "#000000",
                                              size = 1),
              strip.text = element_text(face = "bold",
                                        color = if(any("comparison_text_color" == given_arguments)){additional_arguments$comparison_text_color}else{default_comparison_text_color}),
              legend.background = element_rect(fill = if(any("legend_fill" == given_arguments)){additional_arguments$legend_fill}else{default_legend_fill},
                                               color = "#000000"),
              legend.key = element_rect(fill = "white", color = "black"),
              legend.text = element_text(color = if(any("legend_text_color" == given_arguments)){additional_arguments$legend_text_color}else{default_legend_text_color}),
              title = element_text(face = "bold",
                                   color = if(any("title_color" == given_arguments)){additional_arguments$legend_text_color}else{default_legend_text_color}),
              plot.title = element_text(hjust = 0.5),
              plot.subtitle = element_text(hjust = 0.5))
    }

  }
}


