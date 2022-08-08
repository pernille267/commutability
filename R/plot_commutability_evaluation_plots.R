#' Plots commutability evaluation plots based on ce_data, pb_data and ce_data
#'
#' @param cs_data A \code{list}, \code{data table} or \code{data frame} - Data containing clinical samples' measurements
#' @param pb_data A \code{list}, \code{data table} or \code{data frame} - Data containing prediction band data
#' @param ce_data A \code{list}, \code{data table} or \code{data frame} - Data containing prediction evaluation data for evaluated control materials
#'
#' @return \code{ggplot2} object that is a grid of plots having dimensions corresponding to the number of unique IVD-MD comparisons
#' @export
#'
#' @examples print(1)

plot_commutability_evaluation_plots <- function(cs_data, pb_data, ce_data){
  ggplot() + geom_ribbon(data = pb_data, mapping = aes(x = predictor, ymin = pi_lwr, ymax = pi_upr, fill = dins_conclusion), color = "black", na.rm = TRUE, outline.type = "full") +
    facet_wrap(facets = . ~ comparison, scales = "free") + geom_point(data = cs_data, mapping = aes(x = MP_B, y = MP_A), shape = 20, alpha = 0.5) +
    geom_point(data = ce_data, mapping = aes(x = MS_B, y = MS_A, color = pi_inside, shape = SampleID)) + theme_bw()
}
