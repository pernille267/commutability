#' Plot model assessment tests
#'
#' @param data \code{data table}, \code{data frame} or \code{list} - Data on long-format grouped by comparison. Must contain 'comparison', 'SampleID', 'ReplicateID', 'MP_A' and 'MP_B'.
#' @param method \code{character} - Method to use when calculating the linear model residuals. Can be one of the following: 'fg' (default), 'clsi' or 'ols'.
#' @param type \code{character} - The type of model assessment plot to be plotted. Can either be 'residual_plot' (default), 'qq_plot', 'sd_vs_concentration', 'cv_vs_concentration' or 'residual_histogram'
#' @param draw_curves \code{character} - Should curves be drawn to visualize relationships between two quantities (quantities may differ based on which plot is chosen by \code{type})
#' @param plot_theme \code{character} - Which plotting theme should be applied to the plots. Accepted inputs are 'default', 'noklus', 'soft', 'depression', and 'happy'.
#' @param additional_arguments \code{list} or \code{named array} with 1 row - Additional arguments to be used in calculations and plotting. Possible additional arguments are:
#' \itemize{
#'   \item{\code{x_name}:  }{Title on x-axis. Default value depends on \code{type}}
#'   \item{\code{y_name}:  }{Title on y-axis. Default value depends on \code{type}}
#'   \item{\code{n_breaks}:  }{Number of axis-ticks used. Default value is \code{6}}
#'   \item{\code{main_title}:  }{Plot title. Default value depends on \code{type}}
#'   \item{\code{sub_title}:  }{Plot subtitle. Default value is \code{NULL}}
#'   \item{\code{title_color}:  }{Color of plot title. Default value is \code{'black'}}
#'   \item{\code{sub_title_color}:  }{Color of plot subtitle. Default value is \code{'black'}}
#'   \item{\code{strip_fill}:  }{Fill color of the grouping labels. Default value depends on \code{type}}
#'   \item{\code{strip_text_color}:  }{Text color of the grouping labels. Default value is \code{'black'}}
#'   \item{\code{point_fill}:  }{Fill color of points. Default value is \code{'violet'}}
#'   \item{\code{point_border}:  }{Color of points' border. Default value is \code{'black'}}
#'   \item{\code{point_shape}:  }{Shape of points. Possible values are 'circle', 'square', 'diamond' and 'triangle'. Default is \code{'circle'}}
#'   \item{\code{histogram_fill}:  }{Fill color of histogram bins. Default value is \code{#F9F9F9} (very light gray). Only relevant if \code{type = 'residual_histogram'}}
#'   \item{\code{histogram_border}:  }{Border color of histogram bins. Default value is \code{'black'}. Only relevant if \code{type = 'residual_histogram'}}
#'   \item{\code{curve_color}:  }{Color of drawn curve. Default value is \code{'black'}}
#'   \item{\code{curve_se}:  }{Should confidence bands be included to drawn curve. Default value is \code{FALSE}}
#'   \item{\code{curve_se_fill}:  }{Color of the confidence bands included to drawn curve. Default value is \code{#F9F9F9} (very light gray)}
#'   \item{\code{curve_se_level}:  }{Confidence level for the confidence bands included to drawn curve. Default value is \code{0.95}}
#'   \item{\code{curve_formula}:  }{Edit the standard formula 'y ~ x'. Can be any 'y ~ f(x)'. Default value is 'y ~ x'}
#'   \item{\code{loess_span}:  }{Span of the loess curve. Default value is \code{0.90}. Larger values increase smoothing and smaller values decrease smoothing.}
#'   \item{\code{cv_percent}:  }{Should estimated CVS be given in percent? Default is \code{FALSE}. Only relevant if \code{type = 'cv_vs_concentration'}}
#'   \item{\code{var_instead}:  }{Should variance be given instead of standard deviation? Default is \code{FALSE}. Only relevant if \code{type = 'sd_vs_concentration'}}
#'
#' }
#' @param testing \code{logical} - Should the output be test-friendly? This should only be used by the maintainer of the package, as it has no practical use to the end-user.
#'
#' @return A \code{ggplot2} object, which is the drawn assessment plot
#' @export
#'
#' @examples print(1)

plot_assessment_plots <- function(data, method = "fg", type = "residual_plot", draw_curves = "none", plot_theme = c("custom", "default", "noklus", "soft", "depression", "happy") , additional_arguments = NULL, testing = FALSE){

  comparison <- x <- y <- `..density..` <- NULL;

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

  if(plot_theme != "custom"){
    if(any(plot_theme == c("default", "noklus", "soft", "depression", "happy"))){
      if(plot_theme == "default"){
        plot_theme <- "default"
        theme_arguments <- list(title_color = "#097894",
                                 sub_title_color = "#097894",
                                 strip_fill = "#55CDEC",
                                 strip_text_color = "#000000",
                                 curve_color = "#552A51",
                                 curve_se_fill = "#9AD2F4",
                                 point_fill = "#0912BC",
                                 point_border = "#000000",
                                 histogram_fill = "#F9F9F9")
      }
      else if(plot_theme == "noklus"){
        plot_theme <- "noklus"
        theme_arguments <- list(title_color = "#28A745",
                                 sub_title_color = "#28A745",
                                 strip_fill = "#FFFFFF",
                                 strip_text_color = "#28A745",
                                 curve_color = "#00300B",
                                 curve_se_fill = "#00A2D4",
                                 point_fill = "#00E636",
                                 point_border = "#000000",
                                 histogram_fill = "#3DD2FF")
      }
      else if(plot_theme == "soft"){
        plot_theme <- "soft"
        theme_arguments <- list(title_color = "#9ADCFF",
                                 sub_title_color = "#9ADCFF",
                                 strip_fill = "#FFF89A",
                                 strip_text_color = "#CC70E0",
                                 curve_color = "#D18CE0",
                                 curve_se_fill = "#FFB2A6",
                                 point_fill = "#9ADCFF",
                                 point_border = "#000000",
                                 histogram_fill = "#9ADCFF")
      }
      else if(plot_theme == "depression"){
        plot_theme <- "depression"
        theme_arguments <- list(title_color = "#160040",
                                 sub_title_color = "#4C0070",
                                 strip_fill = "#160040",
                                 strip_text_color = "#FFFFFF",
                                 curve_color = "#151515",
                                 curve_se_fill = "#79018C",
                                 point_fill = "#9A0680",
                                 point_border = "#000000",
                                 histogram_fill = "#6E85B2")
      }
      else if(plot_theme == "happy"){
        plot_theme <- "happy"
        theme_arguments <- list(title_color = "#36AE7C",
                                 sub_title_color = "#36AE7C",
                                 strip_fill = "#F9D923",
                                 strip_text_color = "#EB5353",
                                 curve_color = "#151515",
                                 curve_se_fill = "#187498",
                                 point_fill = "#187498",
                                 point_border = "#EB5353",
                                 histogram_fill = "#36AE7C")
      }
    }
    else{
      plot_theme <- "custom"
    }
  }

  if(any(plot_theme == c("default", "noklus", "soft", "depression", "happy"))){
    additional_arguments$title_color <- theme_arguments$title_color
    additional_arguments$sub_title_color <- theme_arguments$sub_title_color
    additional_arguments$strip_fill <- theme_arguments$strip_fill
    additional_arguments$strip_text_color <- theme_arguments$strip_text_color
    additional_arguments$curve_color <- theme_arguments$curve_color
    additional_arguments$curve_se_fill <- theme_arguments$curve_se_fill
    additional_arguments$point_fill <- theme_arguments$point_fill
    additional_arguments$point_border <- theme_arguments$point_border
    additional_arguments$curve_color <- theme_arguments$curve_color
    additional_arguments$histogram_fill <- theme_arguments$histogram_fill
  }


  if(!is.data.table(data)){

    if(is.data.frame(data)){
      data <- as.data.table(data)
    }
    else if(is.list(data)){
      setDT(data)
    }
    else{
      stop("data is not of class data table, data frame or list! Make sure this is the case for data")
    }
  }

  if(!is.character(data$SampleID)){
    data$SampleID <- as.character(data$SampleID)
  }
  if(!is.character(data$ReplicateID)){
    data$ReplicateID <- as.character(data$ReplicateID)
  }
  if(!any("comparison" == names(data))){
    stop("comparison was not found in data. Make sure it is included. Are you sure data is on long format?")
  }

  if(sum(is.na(data)) > 0){
    data <- data |> na.omit()
  }

  data_list <- split(data, by = "comparison", keep.by = FALSE)
  impr_list <- lapply(X = data_list, FUN = global_precision_estimates)
  plotting_data <- as.list(1:length(data_list))
  names(plotting_data) <- names(data_list)

  if(type == "residual_plot"){

    residual_data <- mapply(FUN = function(x, y) residuals_eqa(data = fun_of_replicates(x),
                                                               imprecision_estimates = y,
                                                               method = method,
                                                               studentize = 1L),
                            data_list, impr_list, SIMPLIFY = FALSE)

    for(i in 1:length(data_list)){
      plotting_data[[i]] <- list("comparison" = rep(names(data_list)[i], length(residual_data[[i]]$fitted)),
                                 "x" = residual_data[[i]]$fitted,
                                 "y" = residual_data[[i]]$residuals) |> setDT()

    }

    plotting_data <- rbindlist(l = plotting_data, idcol = FALSE)
  }

  else if(type == "qq_plot"){

    residual_data <- mapply(FUN = function(x, y) residuals_eqa(data = fun_of_replicates(x),
                                                               imprecision_estimates = y,
                                                               method = method,
                                                               studentize = 1L),
                            data_list, impr_list, SIMPLIFY = FALSE)

    for(i in 1:length(data_list)){
      qqstats <- qqplot(x = rnorm(1e4), y = residual_data[[i]]$residuals, plot.it = FALSE)
      plotting_data[[i]] <- list("comparison" = rep(names(data_list)[i], length(residual_data[[i]]$residuals)),
                                 "x" = qqstats$x,
                                 "y" = qqstats$y) |> setDT()
    }
    plotting_data <- rbindlist(l = plotting_data, idcol = FALSE)

  }
  else if(type == "sd_vs_concentration"){

    sds_data <- lapply(X = data_list, FUN = function(x) fun_of_replicates(x, "sd") |> setDT()) |> rbindlist(idcol = "comparison")
    con_data <- lapply(X = data_list, FUN = function(x) fun_of_replicates(x, "mean") |> setDT()) |> rbindlist(idcol = "comparison")

    # FOR SDS
    first_rows <- unique(sds_data$comparison)[1:(length(unique(sds_data$comparison))-1)]
    last_rows <- unique(sds_data$comparison)[length(unique(sds_data$comparison))]
    first_rows <- sds_data[comparison %in% first_rows, ]
    last_rows <- sds_data[comparison == last_rows, ]
    first_rows$comparison <- lapply(X = stri_split(first_rows$comparison, fixed = " - "), FUN = function(x) x[1]) |> unlist()
    first_rows$MP_B <- NULL
    last_rows_1 <- data.table("comparison" = lapply(X = stri_split(last_rows$comparison, fixed = " - "), FUN = function(x) x[1]) |> unlist(),
                              "SampleID" = last_rows$SampleID,
                              "MP_A" = last_rows$MP_A)
    last_rows_2 <- data.table("comparison" = lapply(X = stri_split(last_rows$comparison, fixed = " - "), FUN = function(x) x[2]) |> unlist(),
                              "SampleID" = last_rows$SampleID,
                              "MP_A" = last_rows$MP_B)
    sds_data_transformed <- rbind(first_rows, last_rows_1, last_rows_2) |> unique()
    names(sds_data_transformed) <- c("MS", "SampleID", "y")

    # FOR CON
    first_rows <- unique(con_data$comparison)[1:(length(unique(con_data$comparison))-1)]
    last_rows <- unique(con_data$comparison)[length(unique(con_data$comparison))]
    first_rows <- con_data[comparison %in% first_rows, ]
    last_rows <- con_data[comparison == last_rows, ]
    first_rows$comparison <- lapply(X = stri_split(first_rows$comparison, fixed = " - "), FUN = function(x) x[1]) |> unlist()
    first_rows$MP_B <- NULL
    last_rows_1 <- data.table("comparison" = lapply(X = stri_split(last_rows$comparison, fixed = " - "), FUN = function(x) x[1]) |> unlist(),
                              "SampleID" = last_rows$SampleID,
                              "MP_A" = last_rows$MP_A)
    last_rows_2 <- data.table("comparison" = lapply(X = stri_split(last_rows$comparison, fixed = " - "), FUN = function(x) x[2]) |> unlist(),
                              "SampleID" = last_rows$SampleID,
                              "MP_A" = last_rows$MP_B)
    con_data_transformed <- rbind(first_rows, last_rows_1, last_rows_2) |> unique()
    names(con_data_transformed) <- c("MS", "SampleID", "x")

    plotting_data <- merge(sds_data_transformed, con_data_transformed, by = c("MS","SampleID"))
  }
  else if(type == "cv_vs_concentration"){
    cvs_data <- lapply(X = data_list, FUN = function(x) fun_of_replicates(x, "cv") |> setDT()) |> rbindlist(idcol = "comparison")
    con_data <- lapply(X = data_list, FUN = function(x) fun_of_replicates(x, "mean") |> setDT()) |> rbindlist(idcol = "comparison")

    # FOR CVS
    first_rows <- unique(cvs_data$comparison)[1:(length(unique(cvs_data$comparison))-1)]
    last_rows <- unique(cvs_data$comparison)[length(unique(cvs_data$comparison))]
    first_rows <- cvs_data[comparison %in% first_rows, ]
    last_rows <- cvs_data[comparison == last_rows, ]
    first_rows$comparison <- lapply(X = stri_split(first_rows$comparison, fixed = " - "), FUN = function(x) x[1]) |> unlist()
    first_rows$MP_B <- NULL
    last_rows_1 <- data.table("comparison" = lapply(X = stri_split(last_rows$comparison, fixed = " - "), FUN = function(x) x[1]) |> unlist(),
                              "SampleID" = last_rows$SampleID,
                              "MP_A" = last_rows$MP_A)
    last_rows_2 <- data.table("comparison" = lapply(X = stri_split(last_rows$comparison, fixed = " - "), FUN = function(x) x[2]) |> unlist(),
                              "SampleID" = last_rows$SampleID,
                              "MP_A" = last_rows$MP_B)
    cvs_data_transformed <- rbind(first_rows, last_rows_1, last_rows_2) |> unique()
    names(cvs_data_transformed) <- c("MS", "SampleID", "y")

    # FOR CON
    first_rows <- unique(con_data$comparison)[1:(length(unique(con_data$comparison))-1)]
    last_rows <- unique(con_data$comparison)[length(unique(con_data$comparison))]
    first_rows <- con_data[comparison %in% first_rows, ]
    last_rows <- con_data[comparison == last_rows, ]
    first_rows$comparison <- lapply(X = stri_split(first_rows$comparison, fixed = " - "), FUN = function(x) x[1]) |> unlist()
    first_rows$MP_B <- NULL
    last_rows_1 <- data.table("comparison" = lapply(X = stri_split(last_rows$comparison, fixed = " - "), FUN = function(x) x[1]) |> unlist(),
                              "SampleID" = last_rows$SampleID,
                              "MP_A" = last_rows$MP_A)
    last_rows_2 <- data.table("comparison" = lapply(X = stri_split(last_rows$comparison, fixed = " - "), FUN = function(x) x[2]) |> unlist(),
                              "SampleID" = last_rows$SampleID,
                              "MP_A" = last_rows$MP_B)
    con_data_transformed <- rbind(first_rows, last_rows_1, last_rows_2) |> unique()
    names(con_data_transformed) <- c("MS", "SampleID", "x")

    plotting_data <- merge(cvs_data_transformed, con_data_transformed, by = c("MS","SampleID"))
  }
  else if(type == "residual_histogram"){
    residual_data <- mapply(FUN = function(x, y) residuals_eqa(data = fun_of_replicates(x),
                                                               imprecision_estimates = y,
                                                               method = method,
                                                               studentize = 1L),
                            data_list, impr_list, SIMPLIFY = FALSE)

    for(i in 1:length(data_list)){
      plotting_data[[i]] <- list("comparison" = rep(names(data_list)[i], length(residual_data[[i]]$residuals)),
                                 "x" = residual_data[[i]]$residuals) |> setDT()

    }

    plotting_data <- rbindlist(l = plotting_data, idcol = FALSE)
  }
  else{

    warning(paste0("type = ", type, " is not recongnized. See ?plot_assessment_plots() for valid inputs for type. 'residual_plot' is used because of this lack of recongnition."))
    residual_data <- mapply(FUN = function(x, y) residuals_eqa(data = fun_of_replicates(x),
                                                               imprecision_estimates = y,
                                                               method = method,
                                                               studentize = 1L),
                            data_list, impr_list, SIMPLIFY = FALSE)

    for(i in 1:length(data_list)){
      plotting_data[[i]] <- list("comparison" = rep(names(data_list)[i], length(residual_data[[i]]$residuals)),
                                 "x" = residual_data[[i]]$fitted,
                                 "y" = residual_data[[i]]$residuals) |> setDT()

    }

    plotting_data <- rbindlist(l = plotting_data, idcol = FALSE)
  }

  if(isTRUE(testing)){

    test_object <- list("plotting_data" = plotting_data,
                        "additional_arguments" = additional_arguments,
                        "plot_theme" = plot_theme,
                        "theme_arguments" = if(any(plot_theme == c("default", "noklus", "soft", "depression", "happy"))){theme_arguments}else{NULL},
                        "other_inputs" = list("method" = method, "type" = type, "draw_curves" = draw_curves))

    return(test_object)
  }

  if(any(type == "residual_plot", type == "qq_plot")){
    if(draw_curves == "loess"){
      base <- ggplot() +
        geom_abline(slope = 1, intercept = 0, alpha = if(type=="qq_plot"){1}else{0}) +
        geom_smooth(data = plotting_data, mapping = aes(x = x, y = y),
                    method = "loess",
                    formula = if(any("curve_formula" == names(additional_arguments))){additional_arguments$curve_formula}else{"y ~ x"},
                    se = if(any("curve_se" == names(additional_arguments))){additional_arguments$curve_se}else{FALSE},
                    fill = if(any("curve_se_fill" == names(additional_arguments))){additional_arguments$curve_se_fill}else{"#f9f9f9"},
                    color = if(any("curve_color" == names(additional_arguments))){additional_arguments$curve_color}else{"black"},
                    span = if(any("loess_span" == names(additional_arguments))){additional_arguments$loess_span}else{9/10}) +
        geom_point(data = plotting_data,
                                    mapping = aes(x = x, y = y),
                                    shape = if(any("point_shape" == names(additional_arguments))){
                                      if(additional_arguments$point_shape=="circle"){
                                      21
                                      }
                                      else if(additional_arguments$point_shape=="square"){
                                        22
                                      }
                                      else if(additional_arguments$point_shape=="diamond"){
                                        23
                                      }
                                      else if(additional_arguments$point_shape=="triangle"){
                                        24
                                      }
                                    }
                                    else{
                                      21
                                    },
                                    fill = if(any("point_fill" == names(additional_arguments))){additional_arguments$point_fill}else{"violet"},
                                    color = if(any("point_border" == names(additional_arguments))){additional_arguments$point_border}else{"black"}) +
        facet_wrap(facets = . ~ comparison, scales = "free") +
        labs(title = if(any("main_title" == names(additional_arguments))){if(isTRUE(additional_arguments$main_title == "")){"Assessment plots for all IVD-MD comparisons"}else{additional_arguments$main_title}}else{"Assessment plots for all IVD-MD comparisons"},
             subtitle = if(any("sub_title" == names(additional_arguments))){additional_arguments$sub_title}else{NULL})

      if(type == "residual_plot"){
        plot_out <- base +
          scale_x_continuous(name = if(any("x_name" == names(additional_arguments))){if(isTRUE(additional_arguments$x_name == "")){"Fitted values"}else{additional_arguments$x_name}}else{"Fitted values"},
                             n.breaks = if(any("n_breaks" == names(additional_arguments))){additional_arguments$n_breaks}else{6}) +
          scale_y_continuous(name = if(any("y_name" == names(additional_arguments))){if(isTRUE(additional_arguments$y_name == "")){"Standardized residuals"}else{additional_arguments$y_name}}else{"Standardized residuals"},
                             n.breaks = if(any("n_breaks" == names(additional_arguments))){additional_arguments$n_breaks}else{6}) +
          theme_bw() +
          theme(plot.title = element_text(face = "bold",
                                          hjust = 0.5,
                                          color = if(any("title_color" == names(additional_arguments))){additional_arguments$title_color}else{"black"}),
                plot.subtitle = element_text(hjust = 0.5,
                                             color = if(any("sub_title_color" == names(additional_arguments))){additional_arguments$sub_title_color}else{"black"}),
                strip.background = element_rect(fill = if(any("strip_fill" == names(additional_arguments))){additional_arguments$strip_fill}else{"#55CDEC"}),
                strip.text = element_text(face = "bold",
                                          color = if(any("strip_text_color" == names(additional_arguments))){additional_arguments$strip_text_color}else{"black"}))

        if(testing=="plot"){
          test_object <- list("curves" = TRUE, "which_curves" = "loess", "which_plot" = "residual_plot")
          return(test_object)
        }

      }

      else if(type == "qq_plot"){
        plot_out <- base +
          scale_x_continuous(name = if(any("x_name" == names(additional_arguments))){if(isTRUE(additional_arguments$x_name == "")){"Theoretical N(0,1) quantiles"}else{additional_arguments$x_name}}else{"Theoretical N(0,1) quantiles"},
                             n.breaks = if(any("n_breaks" == names(additional_arguments))){additional_arguments$n_breaks}else{6}) +
          scale_y_continuous(name = if(any("y_name" == names(additional_arguments))){if(isTRUE(additional_arguments$y_name == "")){"Empirical quantiles of standardized residuals"}else{additional_arguments$y_name}}else{"Empirical quantiles of standardized residuals"},
                             n.breaks = if(any("n_breaks" == names(additional_arguments))){additional_arguments$n_breaks}else{6}) +
          theme_bw() +
          theme(plot.title = element_text(face = "bold",
                                          hjust = 0.5,
                                          color = if(any("title_color" == names(additional_arguments))){additional_arguments$title_color}else{"black"}),
                plot.subtitle = element_text(hjust = 0.5,
                                             color = if(any("sub_title_color" == names(additional_arguments))){additional_arguments$sub_title_color}else{"black"}),
                strip.background = element_rect(fill = if(any("strip_fill" == names(additional_arguments))){additional_arguments$strip_fill}else{"#55CDEC"}),
                strip.text = element_text(face = "bold",
                                          color = if(any("strip_text_color" == names(additional_arguments))){additional_arguments$strip_text_color}else{"black"}))

        if(testing=="plot"){
          test_object <- list("curves" = TRUE, "which_curves" = "loess", "which_plot" = "qq_plot")
          return(test_object)
        }

      }

      plot_out
    }
    else if(draw_curves == "lm"){
      base <- ggplot() +
        geom_abline(slope = 1, intercept = 0, alpha = if(type=="qq_plot"){1}else{0}) +
        geom_smooth(data = plotting_data, mapping = aes(x = x, y = y),
                    method = "lm",
                    formula = if(any("curve_formula" == names(additional_arguments))){additional_arguments$curve_formula}else{"y ~ x"},
                    se = if(any("curve_se" == names(additional_arguments))){additional_arguments$curve_se}else{FALSE},
                    fill = if(any("curve_se_fill" == names(additional_arguments))){additional_arguments$curve_se_fill}else{"#f9f9f9"},
                    color = if(any("curve_color" == names(additional_arguments))){additional_arguments$curve_color}else{"black"}) +
        geom_point(data = plotting_data,
                   mapping = aes(x = x, y = y),
                   shape = if(any("point_shape" == names(additional_arguments))){
                     if(additional_arguments$point_shape=="circle"){
                       21
                     }
                     else if(additional_arguments$point_shape=="square"){
                       22
                     }
                     else if(additional_arguments$point_shape=="diamond"){
                       23
                     }
                     else if(additional_arguments$point_shape=="triangle"){
                       24
                     }
                   }
                   else{
                     21
                   },
                   fill = if(any("point_fill" == names(additional_arguments))){additional_arguments$point_fill}else{"violet"},
                   color = if(any("point_border" == names(additional_arguments))){additional_arguments$point_border}else{"black"}) +
        facet_wrap(facets = . ~ comparison, scales = "free") +
        labs(title = if(any("main_title" == names(additional_arguments))){if(isTRUE(additional_arguments$main_title == "")){"Assessment plots for all IVD-MD comparisons"}else{additional_arguments$main_title}}else{"Assessment plots for all IVD-MD comparisons"},
             subtitle = if(any("sub_title" == names(additional_arguments))){additional_arguments$sub_title}else{NULL})

      if(type == "residual_plot"){
        plot_out <- base +
          scale_x_continuous(name = if(any("x_name" == names(additional_arguments))){if(isTRUE(additional_arguments$x_name == "")){"Fitted values"}else{additional_arguments$x_name}}else{"Fitted values"},
                             n.breaks = if(any("n_breaks" == names(additional_arguments))){additional_arguments$n_breaks}else{6}) +
          scale_y_continuous(name = if(any("y_name" == names(additional_arguments))){if(isTRUE(additional_arguments$y_name == "")){"Standardized residuals"}else{additional_arguments$y_name}}else{"Standardized residuals"},
                             n.breaks = if(any("n_breaks" == names(additional_arguments))){additional_arguments$n_breaks}else{6}) +
          theme_bw() +
          theme(plot.title = element_text(face = "bold",
                                          hjust = 0.5,
                                          color = if(any("title_color" == names(additional_arguments))){additional_arguments$title_color}else{"black"}),
                plot.subtitle = element_text(hjust = 0.5,
                                             color = if(any("sub_title_color" == names(additional_arguments))){additional_arguments$sub_title_color}else{"black"}),
                strip.background = element_rect(fill = if(any("strip_fill" == names(additional_arguments))){additional_arguments$strip_fill}else{"#55CDEC"}),
                strip.text = element_text(face = "bold",
                                          color = if(any("strip_text_color" == names(additional_arguments))){additional_arguments$strip_text_color}else{"black"}))

        if(testing=="plot"){
          test_object <- list("curves" = TRUE, "which_curves" = "lm", "which_plot" = "residual_plot")
          return(test_object)
        }

      }

      else if(type == "qq_plot"){
        plot_out <- base +
          scale_x_continuous(name = if(any("x_name" == names(additional_arguments))){if(isTRUE(additional_arguments$x_name == "")){"Theoretical N(0,1) quantiles"}else{additional_arguments$x_name}}else{"Theoretical N(0,1) quantiles"},
                             n.breaks = if(any("n_breaks" == names(additional_arguments))){additional_arguments$n_breaks}else{6}) +
          scale_y_continuous(name = if(any("y_name" == names(additional_arguments))){if(isTRUE(additional_arguments$y_name == "")){"Empirical quantiles of standardized residuals"}else{additional_arguments$y_name}}else{"Empirical quantiles of standardized residuals"},
                             n.breaks = if(any("n_breaks" == names(additional_arguments))){additional_arguments$n_breaks}else{6}) +
          theme_bw() +
          theme(plot.title = element_text(face = "bold",
                                          hjust = 0.5,
                                          color = if(any("title_color" == names(additional_arguments))){additional_arguments$title_color}else{"black"}),
                plot.subtitle = element_text(hjust = 0.5,
                                             color = if(any("sub_title_color" == names(additional_arguments))){additional_arguments$sub_title_color}else{"black"}),
                strip.background = element_rect(fill = if(any("strip_fill" == names(additional_arguments))){additional_arguments$strip_fill}else{"#55CDEC"}),
                strip.text = element_text(face = "bold",
                                          color = if(any("strip_text_color" == names(additional_arguments))){additional_arguments$strip_text_color}else{"black"}))

        if(testing=="plot"){
          test_object <- list("curves" = TRUE, "which_curves" = "lm", "which_plot" = "qq_plot")
          return(test_object)
        }


      }

      plot_out
    }
    else if(draw_curves == "none"){
      base <- ggplot() +
        geom_abline(slope = 1, intercept = 0, alpha = if(type=="qq_plot"){1}else{0}) +
        geom_point(data = plotting_data,
                   mapping = aes(x = x, y = y),
                   shape = if(any("point_shape" == names(additional_arguments))){
                     if(additional_arguments$point_shape=="circle"){
                       21
                     }
                     else if(additional_arguments$point_shape=="square"){
                       22
                     }
                     else if(additional_arguments$point_shape=="diamond"){
                       23
                     }
                     else if(additional_arguments$point_shape=="triangle"){
                       24
                     }
                   }
                   else{
                     21
                   },
                   fill = if(any("point_fill" == names(additional_arguments))){additional_arguments$point_fill}else{"violet"},
                   color = if(any("point_border" == names(additional_arguments))){additional_arguments$point_border}else{"black"}) +
        facet_wrap(facets = . ~ comparison, scales = "free") +
        labs(title = if(any("main_title" == names(additional_arguments))){if(isTRUE(additional_arguments$main_title == "")){"Assessment plots for all IVD-MD comparisons"}else{additional_arguments$main_title}}else{"Assessment plots for all IVD-MD comparisons"},
             subtitle = if(any("sub_title" == names(additional_arguments))){additional_arguments$sub_title}else{NULL})

      if(type == "residual_plot"){
        plot_out <- base +
          scale_x_continuous(name = if(any("x_name" == names(additional_arguments))){if(isTRUE(additional_arguments$x_name == "")){"Fitted values"}else{additional_arguments$x_name}}else{"Fitted values"},
                             n.breaks = if(any("n_breaks" == names(additional_arguments))){additional_arguments$n_breaks}else{6}) +
          scale_y_continuous(name = if(any("y_name" == names(additional_arguments))){if(isTRUE(additional_arguments$y_name == "")){"Standardized residuals"}else{additional_arguments$y_name}}else{"Standardized residuals"},
                             n.breaks = if(any("n_breaks" == names(additional_arguments))){additional_arguments$n_breaks}else{6}) +
          theme_bw() +
          theme(plot.title = element_text(face = "bold",
                                          hjust = 0.5,
                                          color = if(any("title_color" == names(additional_arguments))){additional_arguments$title_color}else{"black"}),
                plot.subtitle = element_text(hjust = 0.5,
                                             color = if(any("sub_title_color" == names(additional_arguments))){additional_arguments$sub_title_color}else{"black"}),
                strip.background = element_rect(fill = if(any("strip_fill" == names(additional_arguments))){additional_arguments$strip_fill}else{"#55CDEC"}),
                strip.text = element_text(face = "bold",
                                          color = if(any("strip_text_color" == names(additional_arguments))){additional_arguments$strip_text_color}else{"black"}))


        if(testing=="plot"){
          test_object <- list("curves" = FALSE, "which_curves" = "none", "which_plot" = "residual_plot")
          return(test_object)
        }

      }

      else if(type == "qq_plot"){
        plot_out <- base +
          scale_x_continuous(name = if(any("x_name" == names(additional_arguments))){if(isTRUE(additional_arguments$x_name == "")){"Theoretical N(0,1) quantiles"}else{additional_arguments$x_name}}else{"Theoretical N(0,1) quantiles"},
                             n.breaks = if(any("n_breaks" == names(additional_arguments))){additional_arguments$n_breaks}else{6}) +
          scale_y_continuous(name = if(any("y_name" == names(additional_arguments))){if(isTRUE(additional_arguments$y_name == "")){"Empirical quantiles of standardized residuals"}else{additional_arguments$y_name}}else{"Empirical quantiles of standardized residuals"},
                             n.breaks = if(any("n_breaks" == names(additional_arguments))){additional_arguments$n_breaks}else{6}) +
          theme_bw() +
          theme(plot.title = element_text(face = "bold",
                                          hjust = 0.5,
                                          color = if(any("title_color" == names(additional_arguments))){additional_arguments$title_color}else{"black"}),
                plot.subtitle = element_text(hjust = 0.5,
                                             color = if(any("sub_title_color" == names(additional_arguments))){additional_arguments$sub_title_color}else{"black"}),
                strip.background = element_rect(fill = if(any("strip_fill" == names(additional_arguments))){additional_arguments$strip_fill}else{"#55CDEC"}),
                strip.text = element_text(face = "bold",
                                          color = if(any("strip_text_color" == names(additional_arguments))){additional_arguments$strip_text_color}else{"black"}))

        if(testing=="plot"){
          test_object <- list("curves" = FALSE, "which_curves" = "none", "which_plot" = "qq_plot")
          return(test_object)
        }



      }

      plot_out
    }
    else{
      warning(paste0("draw_curves = '",draw_curves,"' is not a valid input. Valid inputs are 'none', 'loess' and 'lm'. 'loess' is used by default"))
      base <- ggplot() +
        geom_abline(slope = 1, intercept = 0, alpha = if(type=="qq_plot"){1}else{0}) +
        geom_smooth(data = plotting_data, mapping = aes(x = x, y = y),
                    method = "loess",
                    formula = if(any("curve_formula" == names(additional_arguments))){additional_arguments$curve_formula}else{"y ~ x"},
                    se = if(any("curve_se" == names(additional_arguments))){additional_arguments$curve_se}else{FALSE},
                    fill = if(any("curve_se_fill" == names(additional_arguments))){additional_arguments$curve_se_fill}else{"#f9f9f9"},
                    color = if(any("curve_color" == names(additional_arguments))){additional_arguments$curve_color}else{"black"},
                    span = if(any("loess_span" == names(additional_arguments))){additional_arguments$loess_span}else{9/10}) +
        geom_point(data = plotting_data,
                   mapping = aes(x = x, y = y),
                   shape = if(any("point_shape" == names(additional_arguments))){
                     if(additional_arguments$point_shape=="circle"){
                       21
                     }
                     else if(additional_arguments$point_shape=="square"){
                       22
                     }
                     else if(additional_arguments$point_shape=="diamond"){
                       23
                     }
                     else if(additional_arguments$point_shape=="triangle"){
                       24
                     }
                   }
                   else{
                     21
                   },
                   fill = if(any("point_fill" == names(additional_arguments))){additional_arguments$point_fill}else{"violet"},
                   color = if(any("point_border" == names(additional_arguments))){additional_arguments$point_border}else{"black"}) +
        facet_wrap(facets = . ~ comparison, scales = "free") +
        labs(title = if(any("main_title" == names(additional_arguments))){additional_arguments$main_title}else{"Assessment plots for all IVD-MD comparisons"},
             subtitle = if(any("sub_title" == names(additional_arguments))){additional_arguments$sub_title}else{NULL})

      if(type == "residual_plot"){
        plot_out <- base +
          scale_x_continuous(name = if(any("x_name" == names(additional_arguments))){additional_arguments$x_name}else{"Fitted values"},
                             n.breaks = if(any("n_breaks" == names(additional_arguments))){additional_arguments$n_breaks}else{6}) +
          scale_y_continuous(name = if(any("y_name" == names(additional_arguments))){additional_arguments$y_name}else{"Standardized residuals"},
                             n.breaks = if(any("n_breaks" == names(additional_arguments))){additional_arguments$n_breaks}else{6}) +
          theme_bw() +
          theme(plot.title = element_text(face = "bold",
                                          hjust = 0.5,
                                          color = if(any("title_color" == names(additional_arguments))){additional_arguments$title_color}else{"black"}),
                plot.subtitle = element_text(hjust = 0.5,
                                             color = if(any("sub_title_color" == names(additional_arguments))){additional_arguments$sub_title_color}else{"black"}),
                strip.background = element_rect(fill = if(any("strip_fill" == names(additional_arguments))){additional_arguments$strip_fill}else{"#55CDEC"}),
                strip.text = element_text(face = "bold",
                                          color = if(any("strip_text_color" == names(additional_arguments))){additional_arguments$strip_text_color}else{"black"}))
      }

      else if(type == "qq_plot"){
        plot_out <- base +
          scale_x_continuous(name = if(any("x_name" == names(additional_arguments))){additional_arguments$x_name}else{"Theoretical N(0,1) quantiles"},
                             n.breaks = if(any("n_breaks" == names(additional_arguments))){additional_arguments$n_breaks}else{6}) +
          scale_y_continuous(name = if(any("y_name" == names(additional_arguments))){additional_arguments$y_name}else{"Empirical quantiles of standardized residuals"},
                             n.breaks = if(any("n_breaks" == names(additional_arguments))){additional_arguments$n_breaks}else{6}) +
          theme_bw() +
          theme(plot.title = element_text(face = "bold",
                                          hjust = 0.5,
                                          color = if(any("title_color" == names(additional_arguments))){additional_arguments$title_color}else{"black"}),
                plot.subtitle = element_text(hjust = 0.5,
                                             color = if(any("sub_title_color" == names(additional_arguments))){additional_arguments$sub_title_color}else{"black"}),
                strip.background = element_rect(fill = if(any("strip_fill" == names(additional_arguments))){additional_arguments$strip_fill}else{"#55CDEC"}),
                strip.text = element_text(face = "bold",
                                          color = if(any("strip_text_color" == names(additional_arguments))){additional_arguments$strip_text_color}else{"black"}))
      }

      plot_out
    }
  }
  else if(any(type == "sd_vs_concentration", type == "cv_vs_concentration")){

    default_y_name <- if(type == "sd_vs_concentration"){"CS-wise standard deviation estimates"}else{"CS-wise coefficient of variation estimates"}

    if(any("cv_percent" == names(additional_arguments))){
      if(type == "cv_vs_concentration" & isTRUE(additional_arguments$cv_percent)){
        plotting_data$y <- plotting_data$y * 100
        default_y_name <- "CS-wise coefficient of variation estimates (%)"

        if(testing=="transformation"){
          test_object <- "cvs are given in percent"
          return(test_object)
        }

      }
    }
    if(any("var_instead" == names(additional_arguments))){
      if(type == "sd_vs_concentration" & isTRUE(additional_arguments$var_instead)){
        plotting_data$y <- plotting_data$y ** 2
        default_y_name <- "CS-wise variance estimates"

        if(testing=="transformation"){
          test_object <- "sds are replaced by variances"
          return(test_object)
        }

      }
    }

    if(draw_curves == "loess"){
      base <- ggplot() +
        geom_smooth(data = plotting_data, mapping = aes(x = x, y = y),
                    method = "loess",
                    formula = if(any("curve_formula" == names(additional_arguments))){additional_arguments$curve_formula}else{"y ~ x"},
                    se = if(any("curve_se" == names(additional_arguments))){additional_arguments$curve_se}else{FALSE},
                    fill = if(any("curve_se_fill" == names(additional_arguments))){additional_arguments$curve_se_fill}else{"#f9f9f9"},
                    color = if(any("curve_color" == names(additional_arguments))){additional_arguments$curve_color}else{"black"},
                    span = if(any("loess_span" == names(additional_arguments))){additional_arguments$loess_span}else{9/10}) +
        geom_point(data = plotting_data,
                   mapping = aes(x = x, y = y),
                   shape = if(any("point_shape" == names(additional_arguments))){
                     if(additional_arguments$point_shape=="circle"){
                       21
                     }
                     else if(additional_arguments$point_shape=="square"){
                       22
                     }
                     else if(additional_arguments$point_shape=="diamond"){
                       23
                     }
                     else if(additional_arguments$point_shape=="triangle"){
                       24
                     }
                   }
                   else{
                     21
                   },
                   fill = if(any("point_fill" == names(additional_arguments))){additional_arguments$point_fill}else{"violet"},
                   color = if(any("point_border" == names(additional_arguments))){additional_arguments$point_border}else{"black"}) +
        facet_wrap(facets = . ~ MS, scales = "free") +
        labs(title = if(any("main_title" == names(additional_arguments))){if(isTRUE(additional_arguments$main_title == "")){"Assessment plots for all IVD-MDs"}else{additional_arguments$main_title}}else{"Assessment plots for all IVD-MDs"},
             subtitle = if(any("sub_title" == names(additional_arguments))){additional_arguments$sub_title}else{NULL})

      if(type == "sd_vs_concentration"){
        plot_out <- base +
          scale_x_continuous(name = if(any("x_name" == names(additional_arguments))){if(isTRUE(additional_arguments$x_name == "")){"Concentration values"}else{additional_arguments$x_name}}else{"Concentration values"},
                             n.breaks = if(any("n_breaks" == names(additional_arguments))){additional_arguments$n_breaks}else{6}) +
          scale_y_continuous(name = if(any("y_name" == names(additional_arguments))){if(isTRUE(additional_arguments$y_name == "")){default_y_name}else{additional_arguments$y_name}}else{default_y_name},
                             n.breaks = if(any("n_breaks" == names(additional_arguments))){additional_arguments$n_breaks}else{6}) +
          theme_bw() +
          theme(plot.title = element_text(face = "bold",
                                          hjust = 0.5,
                                          color = if(any("title_color" == names(additional_arguments))){additional_arguments$title_color}else{"black"}),
                plot.subtitle = element_text(hjust = 0.5,
                                             color = if(any("sub_title_color" == names(additional_arguments))){additional_arguments$sub_title_color}else{"black"}),
                strip.background = element_rect(fill = if(any("strip_fill" == names(additional_arguments))){additional_arguments$strip_fill}else{"#55CDEC"}),
                strip.text = element_text(face = "bold",
                                          color = if(any("strip_text_color" == names(additional_arguments))){additional_arguments$strip_text_color}else{"black"}))

        if(testing=="plot"){
          test_object <- list("curves" = TRUE, "which_curves" = "loess", "which_plot" = "sd_vs_concentration")
          return(test_object)
        }




      }

      else if(type == "cv_vs_concentration"){
        plot_out <- base +
          scale_x_continuous(name = if(any("x_name" == names(additional_arguments))){if(isTRUE(additional_arguments$x_name == "")){"Concentration values"}else{additional_arguments$x_name}}else{"Concentration values"},
                             n.breaks = if(any("n_breaks" == names(additional_arguments))){additional_arguments$n_breaks}else{6}) +
          scale_y_continuous(name = if(any("y_name" == names(additional_arguments))){if(isTRUE(additional_arguments$y_name == "")){default_y_name}else{additional_arguments$y_name}}else{default_y_name},
                             n.breaks = if(any("n_breaks" == names(additional_arguments))){additional_arguments$n_breaks}else{6}) +
          theme_bw() +
          theme(plot.title = element_text(face = "bold",
                                          hjust = 0.5,
                                          color = if(any("title_color" == names(additional_arguments))){additional_arguments$title_color}else{"black"}),
                plot.subtitle = element_text(hjust = 0.5,
                                             color = if(any("sub_title_color" == names(additional_arguments))){additional_arguments$sub_title_color}else{"black"}),
                strip.background = element_rect(fill = if(any("strip_fill" == names(additional_arguments))){additional_arguments$strip_fill}else{"#55CDEC"}),
                strip.text = element_text(face = "bold",
                                          color = if(any("strip_text_color" == names(additional_arguments))){additional_arguments$strip_text_color}else{"black"}))

        if(testing=="plot"){
          test_object <- list("curves" = TRUE, "which_curves" = "loess", "which_plot" = "cv_vs_concentration")
          return(test_object)
        }




      }

      plot_out
    }
    else if(draw_curves == "lm"){
      base <- ggplot() +
        geom_smooth(data = plotting_data, mapping = aes(x = x, y = y),
                    method = "lm",
                    formula = if(any("curve_formula" == names(additional_arguments))){additional_arguments$curve_formula}else{"y ~ x"},
                    se = if(any("curve_se" == names(additional_arguments))){additional_arguments$curve_se}else{FALSE},
                    fill = if(any("curve_se_fill" == names(additional_arguments))){additional_arguments$curve_se_fill}else{"#f9f9f9"},
                    color = if(any("curve_color" == names(additional_arguments))){additional_arguments$curve_color}else{"black"}) +
        geom_point(data = plotting_data,
                   mapping = aes(x = x, y = y),
                   shape = if(any("point_shape" == names(additional_arguments))){
                     if(additional_arguments$point_shape=="circle"){
                       21
                     }
                     else if(additional_arguments$point_shape=="square"){
                       22
                     }
                     else if(additional_arguments$point_shape=="diamond"){
                       23
                     }
                     else if(additional_arguments$point_shape=="triangle"){
                       24
                     }
                   }
                   else{
                     21
                   },
                   fill = if(any("point_fill" == names(additional_arguments))){additional_arguments$point_fill}else{"violet"},
                   color = if(any("point_border" == names(additional_arguments))){additional_arguments$point_border}else{"black"}) +
        facet_wrap(facets = . ~ MS, scales = "free") +
        labs(title = if(any("main_title" == names(additional_arguments))){if(isTRUE(additional_arguments$main_title == "")){"Assessment plots for all IVD-MDs"}else{additional_arguments$main_title}}else{"Assessment plots for all IVD-MDs"},
             subtitle = if(any("sub_title" == names(additional_arguments))){additional_arguments$sub_title}else{NULL})

      if(type == "sd_vs_concentration"){
        plot_out <- base +
          scale_x_continuous(name = if(any("x_name" == names(additional_arguments))){if(isTRUE(additional_arguments$x_name == "")){"Concentration values"}else{additional_arguments$x_name}}else{"Concentration values"},
                             n.breaks = if(any("n_breaks" == names(additional_arguments))){additional_arguments$n_breaks}else{6}) +
          scale_y_continuous(name = if(any("y_name" == names(additional_arguments))){if(isTRUE(additional_arguments$y_name == "")){default_y_name}else{additional_arguments$y_name}}else{default_y_name},
                             n.breaks = if(any("n_breaks" == names(additional_arguments))){additional_arguments$n_breaks}else{6}) +
          theme_bw() +
          theme(plot.title = element_text(face = "bold",
                                          hjust = 0.5,
                                          color = if(any("title_color" == names(additional_arguments))){additional_arguments$title_color}else{"black"}),
                plot.subtitle = element_text(hjust = 0.5,
                                             color = if(any("sub_title_color" == names(additional_arguments))){additional_arguments$sub_title_color}else{"black"}),
                strip.background = element_rect(fill = if(any("strip_fill" == names(additional_arguments))){additional_arguments$strip_fill}else{"#55CDEC"}),
                strip.text = element_text(face = "bold",
                                          color = if(any("strip_text_color" == names(additional_arguments))){additional_arguments$strip_text_color}else{"black"}))

        if(testing=="plot"){
          test_object <- list("curves" = TRUE, "which_curves" = "lm", "which_plot" = "sd_vs_concentration")
          return(test_object)
        }


      }

      else if(type == "cv_vs_concentration"){
        plot_out <- base +
          scale_x_continuous(name = if(any("x_name" == names(additional_arguments))){if(isTRUE(additional_arguments$x_name == "")){"Concentration values"}else{additional_arguments$x_name}}else{"Concentration values"},
                             n.breaks = if(any("n_breaks" == names(additional_arguments))){additional_arguments$n_breaks}else{6}) +
          scale_y_continuous(name = if(any("y_name" == names(additional_arguments))){if(isTRUE(additional_arguments$y_name == "")){default_y_name}else{additional_arguments$y_name}}else{default_y_name},
                             n.breaks = if(any("n_breaks" == names(additional_arguments))){additional_arguments$n_breaks}else{6}) +
          theme_bw() +
          theme(plot.title = element_text(face = "bold",
                                          hjust = 0.5,
                                          color = if(any("title_color" == names(additional_arguments))){additional_arguments$title_color}else{"black"}),
                plot.subtitle = element_text(hjust = 0.5,
                                             color = if(any("sub_title_color" == names(additional_arguments))){additional_arguments$sub_title_color}else{"black"}),
                strip.background = element_rect(fill = if(any("strip_fill" == names(additional_arguments))){additional_arguments$strip_fill}else{"#55CDEC"}),
                strip.text = element_text(face = "bold",
                                          color = if(any("strip_text_color" == names(additional_arguments))){additional_arguments$strip_text_color}else{"black"}))

        if(testing=="plot"){
          test_object <- list("curves" = TRUE, "which_curves" = "lm", "which_plot" = "cv_vs_concentration")
          return(test_object)
        }

      }

      plot_out
    }
    else if(draw_curves == "none"){
      base <- ggplot() +
        geom_point(data = plotting_data,
                   mapping = aes(x = x, y = y),
                   shape = if(any("point_shape" == names(additional_arguments))){
                     if(additional_arguments$point_shape=="circle"){
                       21
                     }
                     else if(additional_arguments$point_shape=="square"){
                       22
                     }
                     else if(additional_arguments$point_shape=="diamond"){
                       23
                     }
                     else if(additional_arguments$point_shape=="triangle"){
                       24
                     }
                   }
                   else{
                     21
                   },
                   fill = if(any("point_fill" == names(additional_arguments))){additional_arguments$point_fill}else{"violet"},
                   color = if(any("point_border" == names(additional_arguments))){additional_arguments$point_border}else{"black"}) +
        facet_wrap(facets = . ~ MS, scales = "free") +
        labs(title = if(any("main_title" == names(additional_arguments))){if(isTRUE(additional_arguments$main_title == "")){"Assessment plots for all IVD-MDs"}else{additional_arguments$main_title}}else{"Assessment plots for all IVD-MDs"},
             subtitle = if(any("sub_title" == names(additional_arguments))){additional_arguments$sub_title}else{NULL})

      if(type == "sd_vs_concentration"){
        plot_out <- base +
          scale_x_continuous(name = if(any("x_name" == names(additional_arguments))){if(isTRUE(additional_arguments$x_name == "")){"Concentration values"}else{additional_arguments$x_name}}else{"Concentration values"},
                             n.breaks = if(any("n_breaks" == names(additional_arguments))){additional_arguments$n_breaks}else{6}) +
          scale_y_continuous(name = if(any("y_name" == names(additional_arguments))){if(isTRUE(additional_arguments$y_name == "")){default_y_name}else{additional_arguments$y_name}}else{default_y_name},
                             n.breaks = if(any("n_breaks" == names(additional_arguments))){additional_arguments$n_breaks}else{6}) +
          theme_bw() +
          theme(plot.title = element_text(face = "bold",
                                          hjust = 0.5,
                                          color = if(any("title_color" == names(additional_arguments))){additional_arguments$title_color}else{"black"}),
                plot.subtitle = element_text(hjust = 0.5,
                                             color = if(any("sub_title_color" == names(additional_arguments))){additional_arguments$sub_title_color}else{"black"}),
                strip.background = element_rect(fill = if(any("strip_fill" == names(additional_arguments))){additional_arguments$strip_fill}else{"#55CDEC"}),
                strip.text = element_text(face = "bold",
                                          color = if(any("strip_text_color" == names(additional_arguments))){additional_arguments$strip_text_color}else{"black"}))

        if(testing=="plot"){
          test_object <- list("curves" = FALSE, "which_curves" = "none", "which_plot" = "sd_vs_concentration")
          return(test_object)
        }



      }

      else if(type == "cv_vs_concentration"){
        plot_out <- base +
          scale_x_continuous(name = if(any("x_name" == names(additional_arguments))){if(isTRUE(additional_arguments$x_name == "")){"Concentration values"}else{additional_arguments$x_name}}else{"Concentration values"},
                             n.breaks = if(any("n_breaks" == names(additional_arguments))){additional_arguments$n_breaks}else{6}) +
          scale_y_continuous(name = if(any("y_name" == names(additional_arguments))){if(isTRUE(additional_arguments$y_name == "")){default_y_name}else{additional_arguments$y_name}}else{default_y_name},
                             n.breaks = if(any("n_breaks" == names(additional_arguments))){additional_arguments$n_breaks}else{6}) +
          theme_bw() +
          theme(plot.title = element_text(face = "bold",
                                          hjust = 0.5,
                                          color = if(any("title_color" == names(additional_arguments))){additional_arguments$title_color}else{"black"}),
                plot.subtitle = element_text(hjust = 0.5,
                                             color = if(any("sub_title_color" == names(additional_arguments))){additional_arguments$sub_title_color}else{"black"}),
                strip.background = element_rect(fill = if(any("strip_fill" == names(additional_arguments))){additional_arguments$strip_fill}else{"#55CDEC"}),
                strip.text = element_text(face = "bold",
                                          color = if(any("strip_text_color" == names(additional_arguments))){additional_arguments$strip_text_color}else{"black"}))

        if(testing=="plot"){
          test_object <- list("curves" = FALSE, "which_curves" = "none", "which_plot" = "cv_vs_concentration")
          return(test_object)
        }



      }

      plot_out
    }
  }
  else if(type == "residual_histogram"){
    if(isFALSE(draw_curves == "none")){

      # Optimal number of bins using FD
      temp_data <- split(x = plotting_data, by = "comparison", keep.by = FALSE)
      opt_bins <- sapply(temp_data, function(x) nclass.FD(x = x$x), simplify = TRUE) |> mean() |> ceiling()

      plot_out <- ggplot() +
        geom_histogram(data = plotting_data, bins = opt_bins,
                       mapping = aes(x = x, y = ..density..),
                       fill = if(any("histogram_fill" == names(additional_arguments))){additional_arguments$histogram_fill}else{"#F9F9F9"},
                       color = if(any("histogram_border" == names(additional_arguments))){additional_arguments$histogram_border}else{"black"}) +
        geom_density(data = plotting_data,
                     mapping = aes(x = x),
                     color = if(any("curve_color" == names(additional_arguments))){additional_arguments$curve_color}else{"black"}) +
        facet_wrap(facets = . ~ comparison, scales = "free") +
        labs(title = if(any("main_title" == names(additional_arguments))){if(isTRUE(additional_arguments$main_title == "")){"Residual histograms for all IVD-MD comparisons"}else{additional_arguments$main_title}}else{"Residual histograms for all IVD-MD comparisons"},
             subtitle = if(any("sub_title" == names(additional_arguments))){additional_arguments$sub_title}else{NULL},
             color = "lines") +
        scale_x_continuous(name = if(any("x_name" == names(additional_arguments))){if(isTRUE(additional_arguments$x_name == "")){"Standardized residuals"}else{additional_arguments$x_name}}else{"Standardized residuals"},
                           n.breaks = if(any("n_breaks" == names(additional_arguments))){additional_arguments$n_breaks}else{6}) +
        scale_y_continuous(name = if(any("y_name" == names(additional_arguments))){if(isTRUE(additional_arguments$y_name == "")){"Empirical density"}else{additional_arguments$y_name}}else{"Empirical density"},
                           n.breaks = if(any("n_breaks" == names(additional_arguments))){additional_arguments$n_breaks}else{6}) +
        theme_bw() +
        theme(plot.title = element_text(face = "bold",
                                        hjust = 0.5,
                                        color = if(any("title_color" == names(additional_arguments))){additional_arguments$title_color}else{"black"}),
              plot.subtitle = element_text(hjust = 0.5,
                                           color = if(any("sub_title_color" == names(additional_arguments))){additional_arguments$sub_title_color}else{"black"}),
              strip.background = element_rect(fill = if(any("strip_fill" == names(additional_arguments))){additional_arguments$strip_fill}else{"#55CDEC"}),
              strip.text = element_text(face = "bold",
                                        color = if(any("strip_text_color" == names(additional_arguments))){additional_arguments$strip_text_color}else{"black"}))

      if(testing=="plot"){
        test_object <- list("curves" = TRUE, "which_curves" = "density", "which_plot" = "residual_histogram")
        return(test_object)
      }



      plot_out
    }
    else{

      temp_data <- split(x = plotting_data, by = "comparison", keep.by = FALSE)
      opt_bins <- sapply(temp_data, function(x) nclass.FD(x = x$x), simplify = TRUE) |> mean() |> ceiling()

      plot_out <- ggplot() +
        geom_histogram(data = plotting_data, bins = opt_bins,
                       mapping = aes(x = x),
                       fill = if(any("histogram_fill" == names(additional_arguments))){additional_arguments$histogram_fill}else{"#F9F9F9"},
                       color = if(any("histogram_border" == names(additional_arguments))){additional_arguments$histogram_border}else{"black"}) +
        facet_wrap(facets = . ~ comparison, scales = "free") +
        labs(title = if(any("main_title" == names(additional_arguments))){if(isTRUE(additional_arguments$main_title == "")){"Residual histograms for all IVD-MD comparisons"}else{additional_arguments$main_title}}else{"Residual histograms for all IVD-MD comparisons"},
             subtitle = if(any("sub_title" == names(additional_arguments))){additional_arguments$sub_title}else{NULL},
             color = "lines") +
        scale_x_continuous(name = if(any("x_name" == names(additional_arguments))){if(isTRUE(additional_arguments$x_name == "")){"Standardized residuals"}else{additional_arguments$x_name}}else{"Standardized residuals"},
                           n.breaks = if(any("n_breaks" == names(additional_arguments))){additional_arguments$n_breaks}else{6}) +
        scale_y_continuous(name = if(any("y_name" == names(additional_arguments))){if(isTRUE(additional_arguments$y_name == "")){"Empirical density"}else{additional_arguments$y_name}}else{"Empirical density"},
                           n.breaks = if(any("n_breaks" == names(additional_arguments))){additional_arguments$n_breaks}else{6}) +
        theme_bw() +
        theme(plot.title = element_text(face = "bold",
                                        hjust = 0.5,
                                        color = if(any("title_color" == names(additional_arguments))){additional_arguments$title_color}else{"black"}),
              plot.subtitle = element_text(hjust = 0.5,
                                           color = if(any("sub_title_color" == names(additional_arguments))){additional_arguments$sub_title_color}else{"black"}),
              strip.background = element_rect(fill = if(any("strip_fill" == names(additional_arguments))){additional_arguments$strip_fill}else{"#55CDEC"}),
              strip.text = element_text(face = "bold",
                                        color = if(any("strip_text_color" == names(additional_arguments))){additional_arguments$strip_text_color}else{"black"}))


      if(testing=="plot"){
        test_object <- list("curves" = FALSE, "which_curves" = "none", "which_plot" = "residual_histogram")
        return(test_object)
      }

      plot_out
    }
  }
}
