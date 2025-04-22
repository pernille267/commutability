library(testthat)
library(readxl)
library(data.table)
library(fasteqa)
library(smooth.commutability)

test_cs_data <- copy(crp_cs_data)

plot_assessment_plots(data = get_comparison_data(commutability_cs_data, reference = "TetraCore"),
                      method = "ols",
                      plot_theme = "default",
                      additional_arguments = list(loess_span = 0.95,
                                                  point_size = 1.25,
                                                  point_shape = "triangle",
                                                  n_breaks = 6,
                                                  cv_percent = FALSE),
                      type = "cv_vs_concentration")




