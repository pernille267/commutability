library(testthat)
library(commutability)
library(readxl)
library(data.table)
library(fasteqa)

set.seed(1)
test_data_1 <- read_excel(path = "~/datasets to be tested on/test_data_1.xlsx")
test_data_2 <- read_excel(path = "~/datasets to be tested on/test_data_2.xlsx")
test_data_3 <- read_excel(path = "~/datasets to be tested on/test_data_3.xlsx")
test_data_4 <- read_excel(path = "~/datasets to be tested on/test_data_4.xlsx")
test_data_5 <- read_excel(path = "~/datasets to be tested on/test_data_5.xlsx")
test_data_6 <- read_excel(path = "~/datasets to be tested on/test_data_6.xlsx")
test_data_7 <- read_excel(path = "~/datasets to be tested on/test_data_7.xlsx")

check_data_1 <- check_data(test_data_1)
check_data_2 <- check_data(test_data_2)
check_data_3 <- check_data(test_data_3)
check_data_4 <- check_data(test_data_4)
check_data_5 <- check_data(test_data_5)
check_data_6 <- check_data(test_data_6)
check_data_7 <- check_data(test_data_7)

test_cs_data_1 <- repair_data(data = test_data_1, check_data_1)[!SampleID %in% c("19","22"),] |> MS_wise()
test_eq_data_1 <- repair_data(data = test_data_1, check_data_1)[SampleID %in% c("19","22"),] |> MS_wise()
test_mo_data_1 <- lapply(X = split(test_cs_data_1, by = "comparison", keep.by = FALSE),
                         FUN = fun_of_replicates) |> rbindlist(idcol = "comparison")

test_cs_data_2 <- repair_data(data = test_data_2, check_data_2)[!SampleID %in% c("10","7"),] |> MS_wise()
test_eq_data_2 <- repair_data(data = test_data_2, check_data_2)[SampleID %in% c("10","7"),] |> MS_wise()
test_mo_data_2 <- lapply(X = split(test_cs_data_2, by = "comparison", keep.by = FALSE),
                         FUN = fun_of_replicates) |> rbindlist(idcol = "comparison")

test_cs_data_3 <- repair_data(data = test_data_6, check_data_6)[!SampleID %in% c("sample id 1","sample id 2"),] |> MS_wise() |> na.omit()
test_eq_data_3 <- repair_data(data = test_data_6, check_data_6)[SampleID %in% c("sample id 1","sample id 2"),] |> MS_wise() |> na.omit()
test_mo_data_3 <- lapply(X = split(test_cs_data_3, by = "comparison", keep.by = FALSE),
                         FUN = fun_of_replicates) |> rbindlist(idcol = "comparison")


test_plot_input_1 <- do_commutability_evaluation(data = test_cs_data_1,
                                                 new_data = test_eq_data_1,
                                                 B = 50,
                                                 N = 50,
                                                 method_pi = "fg",
                                                 upper_zeta = 2.25)

test_plot_input_2 <- do_commutability_evaluation(data = test_cs_data_2,
                                                 new_data = test_eq_data_2,
                                                 B = 50,
                                                 N = 50,
                                                 method_pi = "fg",
                                                 upper_zeta = 2.25)

test_plot_input_3 <- do_commutability_evaluation(data = test_cs_data_3,
                                                 new_data = test_eq_data_3,
                                                 B = 50,
                                                 N = 50,
                                                 method_pi = "fg",
                                                 upper_zeta = 2.25)

test_that(desc = "Testing some of the warnings", code = {


  expect_warning(plot_commutability_evaluation_plots(cs_data = test_mo_data_1,
                                                     pb_data = test_plot_input_1$merged_pb_data,
                                                     ce_data = test_plot_input_1$merged_ce_data,
                                                     exclude_cs = TRUE,
                                                     exclude_rings = TRUE,
                                                     plot_theme = "default",
                                                     additional_arguments = list("sub_title" = 1:10)))

  expect_warning(plot_commutability_evaluation_plots(cs_data = test_mo_data_1,
                                                     pb_data = test_plot_input_1$merged_pb_data,
                                                     ce_data = test_plot_input_1$merged_ce_data,
                                                     exclude_cs = TRUE,
                                                     exclude_rings = TRUE,
                                                     plot_theme = "default",
                                                     additional_arguments = list("main_title" = 2)))
  expect_warning(plot_commutability_evaluation_plots(cs_data = test_mo_data_1,
                                                     pb_data = test_plot_input_1$merged_pb_data,
                                                     ce_data = test_plot_input_1$merged_ce_data,
                                                     exclude_cs = TRUE,
                                                     exclude_rings = TRUE,
                                                     plot_theme = "custom",
                                                     additional_arguments = list(pb_fill = "#FFFFFFF")))

  expect_warning(plot_commutability_evaluation_plots(cs_data = test_mo_data_1,
                                                     pb_data = test_plot_input_1$merged_pb_data,
                                                     ce_data = test_plot_input_1$merged_ce_data,
                                                     exclude_cs = TRUE,
                                                     exclude_rings = TRUE,
                                                     plot_theme = "custom",
                                                     additional_arguments = list(title_color = "green",
                                                                                 sub_title_color = "blux")))

  expect_warning(plot_commutability_evaluation_plots(cs_data = test_mo_data_1,
                                                     pb_data = test_plot_input_1$merged_pb_data,
                                                     ce_data = test_plot_input_1$merged_ce_data,
                                                     exclude_cs = TRUE,
                                                     exclude_rings = TRUE,
                                                     plot_theme = "custom",
                                                     additional_arguments = list(n_breaks = ",")))


})

actual_1 <- plot_commutability_evaluation_plots(cs_data = test_mo_data_1,
                                                pb_data = test_plot_input_1$merged_pb_data,
                                                ce_data = test_plot_input_1$merged_ce_data,
                                                exclude_cs = FALSE,
                                                exclude_rings = FALSE,
                                                plot_theme = "depression",
                                                additional_arguments = list("main_title" = "Commutability evaluation plots",
                                                                            "sub_title" = "Here are the plots",
                                                                            "x_name" = "Measurements from MS x",
                                                                            "y_name" = "Measurements from MS y"))

actual_2 <- plot_commutability_evaluation_plots(cs_data = test_mo_data_2,
                                                pb_data = test_plot_input_2$merged_pb_data,
                                                ce_data = test_plot_input_2$merged_ce_data,
                                                exclude_cs = FALSE,
                                                exclude_rings = FALSE,
                                                plot_theme = "custom",
                                                additional_arguments = list("sub_title" = "Here are the plots again",
                                                                            "legend_fill" = "green",
                                                                            "legend_text_color" = "red",
                                                                            "title_color" = "blue2",
                                                                            "sub_title_color" = "cyan",
                                                                            "x_name" = "X",
                                                                            "y_name" = "Y"))


test_that(desc = "Testing if some arguments are registered", code = {
  expect_equal(object = actual_1$labels$title, expected = "Commutability evaluation plots")
  expect_equal(object = actual_1$labels$subtitle, expected = "Here are the plots")
  expect_equal(object = actual_1$theme$plot.title$hjust, expected = 0.5)
  expect_equal(object = actual_1$theme$plot.subtitle$hjust, expected = 0.5)
  expect_equal(object = actual_1$theme$title$face, expected = "bold")

  expect_equal(object = actual_2$labels$title, expected = "Commutability evaluation plots for all unique IVD-MD comparisons")
  expect_equal(object = actual_2$labels$subtitle, expected = "Here are the plots again")
  expect_equal(object = actual_2$theme$legend.background$fill, expected = "green")
  expect_equal(object = actual_2$theme$legend.background$color, expected = NULL)
  expect_equal(object = actual_2$theme$legend.text$colour, expected = "red")
  expect_equal(object = actual_2$theme$legend.key$fill, expected = "white")
  expect_equal(object = actual_2$theme$legend.key$colour, expected = "black")
  expect_equal(object = actual_2$theme$title$colour, expected = "red")
  expect_equal(object = actual_2$theme$strip.background$fill, expected = "#5BCEFA")
  expect_equal(object = actual_2$theme$strip.background$colour, expected = "#000000")
})


plot_commutability_evaluation_plots(cs_data = test_mo_data_1,
                                    pb_data = test_plot_input_1$merged_pb_data,
                                    ce_data = test_plot_input_1$merged_ce_data,
                                    exclude_cs = TRUE,
                                    exclude_rings = FALSE,
                                    plot_theme = "default",
                                    additional_arguments = list("sub_title" = 1:10))




