#' Plots commutability evaluation plots based on ce_data, pb_data and ce_data
#'
#' @param cs_data A \code{list}, \code{data table} or \code{data frame} - Data containing clinical samples' measurements.
#' @param pb_data A \code{list}, \code{data table} or \code{data frame} - Data containing prediction band data.
#' @param ce_data A \code{list}, \code{data table} or \code{data frame} - Data containing prediction evaluation data for evaluated control materials.
#' @param exclude_rings \code{Logical} - Remove circles that signify strengths of commutability evaluation conclusions. Default is \code{FALSE}.
#' @param exclude_cs \code{Logical} - Remove clinical sample data from plots. Default is \code{FALSE}.
#' @param additional_arguments \code{List} - Additional arguments for the output plot. Default is \code{NULL}, which implies that default plot settings are used. Optinal additional arguments include
#' \itemize{
#'   \item{\code{main_title}: }{Main title of the plot}
#'   \item{\code{sub_title}: }{Sub title of the plot}
#'   \item{\code{x_name}: }{Title of the x-axis}
#'   \item{\code{y_name}: }{Title of the y-axis}
#'   \item{\code{n_breaks}: }{Number of axis-breaks}
#'   \item{\code{comparison_fill}: }{Fill color for the individual plot labels listing the IVD-MD comparisons. Must be a valid color name or HEX code}
#'   \item{\code{legend_fill}: }{Fill color for the legend regions. Must be a valid color name or HEX code}
#' }
#'
#' @return \code{ggplot2} object that is a grid of plots having dimensions corresponding to the number of unique IVD-MD comparisons
#' @export
#'
#' @examples print(1)

plot_commutability_evaluation_plots <- function(cs_data, pb_data, ce_data, exclude_rings = FALSE, exclude_cs = FALSE, additional_arguments = NULL){

  pb_data$dins_conclusion <- ifelse(pb_data$dins_conclusion == 0, "acceptable", "not acceptable")
  ce_data$dins_conclusion <- ifelse(ce_data$dins_conclusion == 0, "acceptable", "not acceptable")
  ce_data$pi_inside <- ifelse(ce_data$pi_inside == 0, "no", "yes")
  ce_data$pi_conclusion_correctness <- mapply(FUN = function(x, y) if(x == "yes"){y}else{1 - y}, ce_data$pi_inside, ce_data$inside_rate)

  main_title <- "Commutability evaluation plots"
  sub_title <- NULL
  x_name <- "Measurements along x-axis * - "
  y_name <- "Measurements along y-axis  - *"
  n_breaks <- 6L
  comparison_fill <- "#5BCEFA"
  legend_fill <- "#FCFCFC"
  given_arguments <- names(additional_arguments)

  if(any("main_title" == given_arguments)){
    main_title <- additional_arguments$main_title[1]
    if(is.character(main_title)){
      main_title <- main_title
    }
    else{
      warning(paste0("main_title is not of character type, but '", typeof(main_title) ,"'. Default main_title is used instead"))
      main_title <- "Commutability evaluation plots"
    }
  }
  if(any("sub_title" == given_arguments)){
    sub_title <- additional_arguments$sub_title[1]
    if(is.character(sub_title)){
      sub_title <- sub_title
    }
    else if(is.null(sub_title)){
      sub_title <- NULL
    }
    else{
      warning(paste0("sub_title is not of character type, but '", typeof(sub_title) ,"'. Default sub_title is used instead"))
      sub_title <- NULL
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
  }

  if(any("comparison_fill" == given_arguments)){
    comparison_fill <- additional_arguments$comparison_fill[1]
    if(is.na(comparison_fill) | is.null(comparison_fill)){
      comparison_fill <- "#5BCEFA"
    }
    if(!(any(comparison_fill == colors()) | any(comparison_fill == 1:length(colors())))){
      if(stri_length(comparison_fill) == 7 & stri_locate_first_fixed(comparison_fill, pattern = "#", )[1,1] == 1){
        test_hex <- stri_sub(comparison_fill, from = 2, to = 7) |> stri_split_boundaries(type = "character") |> unlist()
        valid_hex <- all(sapply(test_hex, FUN = function(x) if(stri_detect(x, regex = "[[:digit:]|[abcdefABCDEF]]")){TRUE}else{FALSE}))
        if(!valid_hex){
          warning(paste0("The picked color for comparison_fill, ", comparison_fill, " , is not valid. Default fill color is used instead"))
          comparison_fill <- "#5BCEFA"
        }
      }
      else{
        warning(paste0("The picked color for comparison_fill, ", comparison_fill, " , is not valid. Default fill color is used instead"))
        comparison_fill <- "#5BCEFA"
      }
    }
  }

  if(any("legend_fill" == given_arguments)){
    legend_fill <- additional_arguments$legend_fill[1]
    if(is.na(legend_fill) | is.null(legend_fill)){
      legend_fill <- "#FCFCFC"
    }
    if(!(any(legend_fill == colors()) | any(legend_fill == 1:length(colors())))){
      if(stri_length(legend_fill) == 7 & stri_locate_first_fixed(legend_fill, pattern = "#", )[1,1] == 1){
        test_hex <- stri_sub(legend_fill, from = 2, to = 7)  |> stri_split_boundaries(type = "character") |> unlist()
        valid_hex <- all(sapply(test_hex, FUN = function(x) if(stri_detect(x, regex = "[[:digit:]|[abcdefABCDEF]]")){TRUE}else{FALSE}))
        if(!valid_hex){
          warning(paste0("The picked color for legend_fill, ", legend_fill, " , is not valid. Default fill color is used instead"))
          legend_fill <- "#FCFCFC"
        }
      }
      else{
        warning(paste0("The picked color for legend_fill, ", legend_fill, " , is not valid. Default fill color is used instead"))
        legend_fill <- "#FCFCFC"
      }
    }
  }

  if(!exclude_cs & !exclude_rings){
    ggplot() + geom_ribbon(data = pb_data, mapping = aes(x = predictor, ymin = pi_lwr, ymax = pi_upr, fill = dins_conclusion), alpha = 0.3, color = "black", na.rm = TRUE, outline.type = "full") +
      facet_wrap(facets = . ~ comparison, scales = "free") +
      geom_point(data = cs_data, mapping = aes(x = MP_B, y = MP_A), shape = 20, alpha = 0.5, size = 0.5) +
      geom_segment(data = ce_data, mapping = aes(x = MS_B, xend = MS_B, y = pi_lwr, yend = pi_upr), arrow = arrow(angle = 90, ends = "both", length = unit(x = 0.025, units = "npc"))) +
      geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 3, alpha = 0.3) + theme_bw() +
      geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 2.75, alpha = 0.4) +
      geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 2.50, alpha = 0.5) +
      geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 2.25, alpha = 0.75) +
      geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 2.00, alpha = 0.90) +
      geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 1.50, alpha = 1) +
      geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, shape = SampleID), size = 0.50, alpha = 1, color = "black") +
      geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, alpha = pi_conclusion_correctness, color = pi_inside), shape = 1, size = 5, show.legend = FALSE) +
      labs(title = main_title, color = "Inside prediction interval", shape = "Evaluated materials' ID", fill = "Difference in non-selectivity") +
      scale_x_continuous(name = x_name, n.breaks = n_breaks) +
      scale_y_continuous(name = y_name, n.breaks = n_breaks) +
      scale_fill_manual(values = c("acceptable" = "green", "not acceptable" = "gray")) +
      scale_color_manual(values = c("yes" = "#1994dc", "no" = "#dc1932")) +
      scale_alpha_binned(name = "Conclusion correctness", limits = c(0, 1), n.breaks = 30, range = c(0, 1)) +
      theme(strip.background = element_rect(fill = comparison_fill, color = "#000000", size = 1),
            strip.text = element_text(face = "bold", color = "#000000"),
            legend.background = element_rect(fill = legend_fill, color = "#000000"),
            plot.title = element_text(face = "bold", color = "#000000"))
  }
  else if(!exclude_cs & exclude_rings){
    ggplot() + geom_ribbon(data = pb_data, mapping = aes(x = predictor, ymin = pi_lwr, ymax = pi_upr, fill = dins_conclusion), alpha = 0.3, color = "black", na.rm = TRUE, outline.type = "full") +
      facet_wrap(facets = . ~ comparison, scales = "free") +
      geom_point(data = cs_data, mapping = aes(x = MP_B, y = MP_A), shape = 20, alpha = 0.5, size = 0.5) +
      geom_segment(data = ce_data, mapping = aes(x = MS_B, xend = MS_B, y = pi_lwr, yend = pi_upr), arrow = arrow(angle = 90, ends = "both", length = unit(x = 0.025, units = "npc"))) +
      geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 3, alpha = 0.3) + theme_bw() +
      geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 2.75, alpha = 0.4) +
      geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 2.50, alpha = 0.5) +
      geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 2.25, alpha = 0.75) +
      geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 2.00, alpha = 0.90) +
      geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 1.50, alpha = 1) +
      geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, shape = SampleID), size = 0.50, alpha = 1, color = "black") +
      labs(title = main_title, color = "Inside prediction interval", shape = "Evaluated materials' ID", fill = "Difference in non-selectivity") +
      scale_x_continuous(name = x_name, n.breaks = n_breaks) +
      scale_y_continuous(name = y_name, n.breaks = n_breaks) +
      scale_fill_manual(values = c("acceptable" = "green", "not acceptable" = "gray")) +
      scale_color_manual(values = c("yes" = "#1994dc", "no" = "#dc1932")) +
      theme(strip.background = element_rect(fill = comparison_fill, color = "#000000", size = 1),
            strip.text = element_text(face = "bold", color = "#000000"),
            legend.background = element_rect(fill = legend_fill, color = "#000000"),
            plot.title = element_text(face = "bold", color = "#000000"))
  }
  else if(exclude_cs & !exclude_rings){
    ggplot() + geom_ribbon(data = pb_data, mapping = aes(x = predictor, ymin = pi_lwr, ymax = pi_upr, fill = dins_conclusion), alpha = 0.3, color = "black", na.rm = TRUE, outline.type = "full") +
      facet_wrap(facets = . ~ comparison, scales = "free") +
      geom_segment(data = ce_data, mapping = aes(x = MS_B, xend = MS_B, y = pi_lwr, yend = pi_upr), arrow = arrow(angle = 90, ends = "both", length = unit(x = 0.025, units = "npc"))) +
      geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 3, alpha = 0.3) + theme_bw() +
      geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 2.75, alpha = 0.4) +
      geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 2.50, alpha = 0.5) +
      geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 2.25, alpha = 0.75) +
      geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 2.00, alpha = 0.90) +
      geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 1.50, alpha = 1) +
      geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, shape = SampleID), size = 0.50, alpha = 1, color = "black") +
      geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, alpha = pi_conclusion_correctness, color = pi_inside), shape = 1, size = 5, show.legend = FALSE) +
      labs(title = main_title, color = "Inside prediction interval", shape = "Evaluated materials' ID", fill = "Difference in non-selectivity") +
      scale_x_continuous(name = x_name, n.breaks = n_breaks) +
      scale_y_continuous(name = y_name, n.breaks = n_breaks) +
      scale_fill_manual(values = c("acceptable" = "green", "not acceptable" = "gray")) +
      scale_color_manual(values = c("yes" = "#1994dc", "no" = "#dc1932")) +
      scale_alpha_binned(name = "Conclusion correctness", limits = c(0, 1), n.breaks = 30, range = c(0, 1)) +
      theme(strip.background = element_rect(fill = comparison_fill, color = "#000000", size = 1),
            strip.text = element_text(face = "bold", color = "#000000"),
            legend.background = element_rect(fill = legend_fill, color = "#000000"),
            plot.title = element_text(face = "bold", color = "#000000"))
  }

  else if(exclude_cs & exclude_rings){
    ggplot() + geom_ribbon(data = pb_data, mapping = aes(x = predictor, ymin = pi_lwr, ymax = pi_upr, fill = dins_conclusion), alpha = 0.3, color = "black", na.rm = TRUE, outline.type = "full") +
      facet_wrap(facets = . ~ comparison, scales = "free") +
      geom_segment(data = ce_data, mapping = aes(x = MS_B, xend = MS_B, y = pi_lwr, yend = pi_upr), arrow = arrow(angle = 90, ends = "both", length = unit(x = 0.025, units = "npc"))) +
      geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 3, alpha = 0.3) + theme_bw() +
      geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 2.75, alpha = 0.4) +
      geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 2.50, alpha = 0.5) +
      geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 2.25, alpha = 0.75) +
      geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 2.00, alpha = 0.90) +
      geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID), size = 1.50, alpha = 1) +
      geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, shape = SampleID), size = 0.50, alpha = 1, color = "black") +
      labs(title = main_title, color = "Inside prediction interval", shape = "Evaluated materials' ID", fill = "Difference in non-selectivity") +
      scale_x_continuous(name = x_name, n.breaks = n_breaks) +
      scale_y_continuous(name = y_name, n.breaks = n_breaks) +
      scale_fill_manual(values = c("acceptable" = "green", "not acceptable" = "gray")) +
      scale_color_manual(values = c("yes" = "#1994dc", "no" = "#dc1932")) +
      theme(strip.background = element_rect(fill = comparison_fill, color = "#000000", size = 1),
            strip.text = element_text(face = "bold", color = "#000000"),
            legend.background = element_rect(fill = legend_fill, color = "#000000"),
            plot.title = element_text(face = "bold", color = "#000000"))
  }
}


