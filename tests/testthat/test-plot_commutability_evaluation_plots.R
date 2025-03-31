library(testthat)
library(readxl)
library(data.table)
library(fasteqa)
library(stringi)
library(smooth.commutability)

# Read data to be tested
test_cs_data <- copy(crp_cs_data)
test_eq_data <- copy(crp_eqam_data)
test_cs_data_2 <- copy(commutability_cs_data)
test_eq_data_2 <- copy(commutability_eq_data)[stri_detect(str = SampleID,
                                                          fixed = "RM")]

# TEST first data
test_that(desc = "Check stuff 1", code = {

  # Pick random theme
  plot_theme <- sample(x = c("default", "noklus", "depression", "soft", "happy"),
                       size = 1)


  # Commutability Evaluation Components
  dce_1 <- do_commutability_evaluation(data = test_cs_data,
                                       new_data = test_eq_data,
                                       B = 2000L,
                                       N = 1000L,
                                       method_pi = "fg",
                                       method_bs = "BCa",
                                       upper_zeta = 3.49,
                                       level_pi = 0.99,
                                       level_bs = 0.95,
                                       output = "sufficient")

  # Mor data
  test_mor_cs_data <- test_cs_data[, fun_of_replicates(.SD), by = comparison]

  # Expect to run without errors
  expect_no_condition(object = plot_commutability_evaluation_plots(
    cs_data = test_mor_cs_data,
    pb_data = dce_1$merged_pb_data,
    ce_data = dce_1$merged_ce_data,
    exclude_rings = FALSE,
    exclude_cs = FALSE,
    plot_theme = plot_theme,
    additional_arguments = NULL)
  )

  actual_1 <- plot_commutability_evaluation_plots(cs_data = test_mor_cs_data,
                                                  pb_data = dce_1$merged_pb_data,
                                                  ce_data = dce_1$merged_ce_data,
                                                  exclude_rings = FALSE,
                                                  exclude_cs = FALSE,
                                                  plot_theme = plot_theme,
                                                  additional_arguments = NULL)

  actual_1_labels <- names(actual_1$labels)

  # Expect to find alpha because exclude_rings = FALSE
  expect_true(object = any("alpha" == actual_1_labels))

  # Expect to find shape because number of evaluated materials are <= 6
  expect_true(object = any("shape" == actual_1_labels))

  # Expect ten layers with these settings
  expect_length(object = actual_1$layers, n = 10)

  # Expect to run without errors
  expect_no_condition(object = plot_commutability_evaluation_plots(
    cs_data = test_mor_cs_data,
    pb_data = dce_1$merged_pb_data,
    ce_data = dce_1$merged_ce_data,
    exclude_rings = TRUE,
    exclude_cs = TRUE,
    plot_theme = plot_theme,
    additional_arguments = NULL)
  )

  actual_2 <- plot_commutability_evaluation_plots(cs_data = test_mor_cs_data,
                                                  pb_data = dce_1$merged_pb_data,
                                                  ce_data = dce_1$merged_ce_data,
                                                  exclude_rings = TRUE,
                                                  exclude_cs = TRUE,
                                                  plot_theme = plot_theme,
                                                  additional_arguments = NULL)

  actual_2_labels <- names(actual_2$labels)

  # Expect not to find alpha because exclude_rings = TRUE
  expect_true(object = !any("alpha" == actual_2_labels))

  # Expect to find shape because number of evaluated materials are <= 6
  expect_true(object = any("shape" == actual_2_labels))

  # Expect eight layers with these settings
  expect_length(object = actual_2$layers, n = 8)


})

# TEST second data
test_that(desc = "Check stuff 2", code = {

  # Pick random theme
  plot_theme <- sample(x = c("default", "noklus", "depression", "soft", "happy"),
                       size = 1)

  # Repair and transform data
  test_cs_data_2 <- repair_data(test_cs_data_2, type = "cs")
  test_eq_data_2 <- repair_data(test_eq_data_2, type = "eqam")
  test_cs_data_2 <- transform_data(get_comparison_data(data = test_cs_data_2, reference = "Thermo"))
  test_eq_data_2 <- transform_data(get_comparison_data(data = test_eq_data_2, reference = "Thermo"))

  # Mor data
  test_mor_cs_data_2 <- na.omit(test_cs_data_2)[, fun_of_replicates(.SD), by = comparison]

  # Commutability Evaluation Components
  dce_2 <- do_commutability_evaluation(data = test_cs_data_2,
                                       new_data = test_eq_data_2,
                                       B = 51L,
                                       N = 100L,
                                       method_pi = "ssw",
                                       method_bs = "BCa",
                                       upper_zeta = 8.77,
                                       level_pi = 0.99,
                                       level_bs = 0.95,
                                       output = "sufficient")

  # Expect no error, warning or message
  expect_no_condition(object = plot_commutability_evaluation_plots(
    cs_data = test_mor_cs_data_2,
    pb_data = dce_2$merged_pb_data,
    ce_data = dce_2$merged_ce_data,
    exclude_rings = FALSE,
    exclude_cs = TRUE,
    plot_theme = plot_theme,
    additional_arguments = list(
      main_title = "This is a title",
      sub_title = "This is a subtitle",
      x_name = "Thermo measurements",
      y_name = "Response measurements"
      )
    )
  )

  actual_1 <- plot_commutability_evaluation_plots(
    cs_data = test_mor_cs_data_2,
    pb_data = dce_2$merged_pb_data,
    ce_data = dce_2$merged_ce_data,
    exclude_rings = FALSE,
    exclude_cs = TRUE,
    plot_theme = plot_theme,
    additional_arguments = list(
      main_title = "This is a title",
      sub_title = "This is a subtitle",
      x_name = "Thermo measurements",
      y_name = "Response measurements"
    )
  )

  actual_1_labels <- names(actual_1$labels)
  actual_1_label_values <- actual_1$labels

  # Expect to find alpha because exclude_rings = FALSE
  expect_true(object = any("alpha" == actual_1_labels))

  # Expect to not find shape because number of evaluated materials are > 6
  expect_true(object = !any("shape" == actual_1_labels))

  # Expect five layers with these settings
  expect_length(object = actual_1$layers, n = 5)

  # Expect custom main_title
  expect_equal(object = actual_1_label_values$title,
               expected = "This is a title")

  # Expect custom sub_title
  expect_equal(object = actual_1_label_values$subtitle,
               expected = "This is a subtitle")

})








