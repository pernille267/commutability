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

test_data_1 <- repair_data(data = test_data_1, check_data_1) |> MS_wise()
test_data_2 <- repair_data(data = test_data_2, check_data_2) |> MS_wise()
test_data_3 <- repair_data(data = test_data_3, check_data_3) |> MS_wise()
test_data_4 <- repair_data(data = test_data_4, check_data_4) |> MS_wise()
test_data_5 <- repair_data(data = test_data_5, check_data_5) |> MS_wise()
test_data_6 <- repair_data(data = test_data_6, check_data_6) |> MS_wise()
test_data_7 <- repair_data(data = test_data_7, check_data_7) |> MS_wise()


# Testing for draw_curves = "loess"

actual_output_1 <- plot_assessment_plots(data = test_data_1, method = "fg", type = "residual_plot", draw_curves = "loess", plot_theme = "default", testing = TRUE)
actual_output_2 <- plot_assessment_plots(data = test_data_7, method = "fg", type = "qq_plot", draw_curves = "loess", plot_theme = "default", testing = TRUE)
actual_output_3 <- plot_assessment_plots(data = test_data_5, method = "fg", type = "sd_vs_concentration", draw_curves = "loess", plot_theme = "default", testing = TRUE)
actual_output_4 <- plot_assessment_plots(data = test_data_4, method = "fg", type = "cv_vs_concentration", draw_curves = "loess", plot_theme = "default", testing = TRUE)
actual_output_5 <- plot_assessment_plots(data = test_data_2, method = "fg", type = "residual_histogram", draw_curves = "loess", plot_theme = "default", testing = TRUE)

actual_output_6 <- plot_assessment_plots(data = test_data_1, method = "clsi", type = "residual_plot", draw_curves = "loess", plot_theme = "default", testing = TRUE)
actual_output_7 <- plot_assessment_plots(data = test_data_7, method = "clsi", type = "qq_plot", draw_curves = "loess", plot_theme = "default", testing = TRUE)
actual_output_8 <- plot_assessment_plots(data = test_data_5, method = "clsi", type = "sd_vs_concentration", draw_curves = "loess", plot_theme = "default", testing = TRUE)
actual_output_9 <- plot_assessment_plots(data = test_data_4, method = "clsi", type = "cv_vs_concentration", draw_curves = "loess", plot_theme = "default", testing = TRUE)
actual_output_10 <- plot_assessment_plots(data = test_data_2, method = "clsi", type = "residual_histogram", draw_curves = "loess", plot_theme = "default", testing = TRUE)

actual_output_11 <- plot_assessment_plots(data = test_data_1, method = "ols", type = "residual_plot", draw_curves = "loess", plot_theme = "default", testing = TRUE)
actual_output_12 <- plot_assessment_plots(data = test_data_7, method = "ols", type = "qq_plot", draw_curves = "loess", plot_theme = "default", testing = TRUE)
actual_output_13 <- plot_assessment_plots(data = test_data_5, method = "ols", type = "sd_vs_concentration", draw_curves = "loess", plot_theme = "default", testing = TRUE)
actual_output_14 <- plot_assessment_plots(data = test_data_4, method = "ols", type = "cv_vs_concentration", draw_curves = "loess", plot_theme = "default", testing = TRUE)
actual_output_15 <- plot_assessment_plots(data = test_data_2, method = "ols", type = "residual_histogram", draw_curves = "loess", plot_theme = "default", testing = TRUE)

test_that(desc = "testing output class of plotting_data", code = {
  expect_s3_class(object = actual_output_1$plotting_data, class = "data.table")
  expect_s3_class(object = actual_output_2$plotting_data, class = "data.table")
  expect_s3_class(object = actual_output_3$plotting_data, class = "data.table")
  expect_s3_class(object = actual_output_4$plotting_data, class = "data.table")
  expect_s3_class(object = actual_output_5$plotting_data, class = "data.table")
  expect_s3_class(object = actual_output_6$plotting_data, class = "data.table")
  expect_s3_class(object = actual_output_7$plotting_data, class = "data.table")
  expect_s3_class(object = actual_output_8$plotting_data, class = "data.table")
  expect_s3_class(object = actual_output_9$plotting_data, class = "data.table")
  expect_s3_class(object = actual_output_10$plotting_data, class = "data.table")
  expect_s3_class(object = actual_output_11$plotting_data, class = "data.table")
  expect_s3_class(object = actual_output_12$plotting_data, class = "data.table")
  expect_s3_class(object = actual_output_13$plotting_data, class = "data.table")
  expect_s3_class(object = actual_output_14$plotting_data, class = "data.table")
  expect_s3_class(object = actual_output_15$plotting_data, class = "data.table")
})

test_that(desc = "testing if plotting inputs is registered by the function", code = {
  expect_equal(object = actual_output_1$other_inputs, expected = list("method" = "fg", "type" = "residual_plot", "draw_curves" = "loess"))
  expect_equal(object = actual_output_2$other_inputs, expected = list("method" = "fg", "type" = "qq_plot", "draw_curves" = "loess"))
  expect_equal(object = actual_output_3$other_inputs, expected = list("method" = "fg", "type" = "sd_vs_concentration", "draw_curves" = "loess"))
  expect_equal(object = actual_output_4$other_inputs, expected = list("method" = "fg", "type" = "cv_vs_concentration", "draw_curves" = "loess"))
  expect_equal(object = actual_output_5$other_inputs, expected = list("method" = "fg", "type" = "residual_histogram", "draw_curves" = "loess"))
  expect_equal(object = actual_output_6$other_inputs, expected = list("method" = "clsi", "type" = "residual_plot", "draw_curves" = "loess"))
  expect_equal(object = actual_output_7$other_inputs, expected = list("method" = "clsi", "type" = "qq_plot", "draw_curves" = "loess"))
  expect_equal(object = actual_output_8$other_inputs, expected = list("method" = "clsi", "type" = "sd_vs_concentration", "draw_curves" = "loess"))
  expect_equal(object = actual_output_9$other_inputs, expected = list("method" = "clsi", "type" = "cv_vs_concentration", "draw_curves" = "loess"))
  expect_equal(object = actual_output_10$other_inputs, expected = list("method" = "clsi", "type" = "residual_histogram", "draw_curves" = "loess"))
  expect_equal(object = actual_output_11$other_inputs, expected = list("method" = "ols", "type" = "residual_plot", "draw_curves" = "loess"))
  expect_equal(object = actual_output_12$other_inputs, expected = list("method" = "ols", "type" = "qq_plot", "draw_curves" = "loess"))
  expect_equal(object = actual_output_13$other_inputs, expected = list("method" = "ols", "type" = "sd_vs_concentration", "draw_curves" = "loess"))
  expect_equal(object = actual_output_14$other_inputs, expected = list("method" = "ols", "type" = "cv_vs_concentration", "draw_curves" = "loess"))
  expect_equal(object = actual_output_15$other_inputs, expected = list("method" = "ols", "type" = "residual_histogram", "draw_curves" = "loess"))
})

test_that(desc = "Testing if theme_arguments = additional_arguments, when additional_arugments are unspecified", code = {
  expect_equal(object = actual_output_1$additional_arguments, expected = actual_output_1$theme_arguments)
  expect_equal(object = actual_output_2$additional_arguments, expected = actual_output_2$theme_arguments)
  expect_equal(object = actual_output_3$additional_arguments, expected = actual_output_3$theme_arguments)
  expect_equal(object = actual_output_4$additional_arguments, expected = actual_output_4$theme_arguments)
  expect_equal(object = actual_output_5$additional_arguments, expected = actual_output_5$theme_arguments)
  expect_equal(object = actual_output_6$additional_arguments, expected = actual_output_6$theme_arguments)
  expect_equal(object = actual_output_7$additional_arguments, expected = actual_output_7$theme_arguments)
  expect_equal(object = actual_output_8$additional_arguments, expected = actual_output_8$theme_arguments)
  expect_equal(object = actual_output_9$additional_arguments, expected = actual_output_9$theme_arguments)
  expect_equal(object = actual_output_10$additional_arguments, expected = actual_output_10$theme_arguments)
  expect_equal(object = actual_output_11$additional_arguments, expected = actual_output_11$theme_arguments)
  expect_equal(object = actual_output_12$additional_arguments, expected = actual_output_12$theme_arguments)
  expect_equal(object = actual_output_13$additional_arguments, expected = actual_output_13$theme_arguments)
  expect_equal(object = actual_output_14$additional_arguments, expected = actual_output_14$theme_arguments)
  expect_equal(object = actual_output_15$additional_arguments, expected = actual_output_15$theme_arguments)
})


# Testing for draw_curves = "lm"

actual_output_1 <- plot_assessment_plots(data = test_data_1, method = "fg", type = "residual_plot", draw_curves = "lm", plot_theme = "default", testing = TRUE)
actual_output_2 <- plot_assessment_plots(data = test_data_7, method = "fg", type = "qq_plot", draw_curves = "lm", plot_theme = "default", testing = TRUE)
actual_output_3 <- plot_assessment_plots(data = test_data_5, method = "fg", type = "sd_vs_concentration", draw_curves = "lm", plot_theme = "default", testing = TRUE)
actual_output_4 <- plot_assessment_plots(data = test_data_4, method = "fg", type = "cv_vs_concentration", draw_curves = "lm", plot_theme = "default", testing = TRUE)
actual_output_5 <- plot_assessment_plots(data = test_data_2, method = "fg", type = "residual_histogram", draw_curves = "lm", plot_theme = "default", testing = TRUE)

actual_output_6 <- plot_assessment_plots(data = test_data_1, method = "clsi", type = "residual_plot", draw_curves = "lm", plot_theme = "default", testing = TRUE)
actual_output_7 <- plot_assessment_plots(data = test_data_7, method = "clsi", type = "qq_plot", draw_curves = "lm", plot_theme = "default", testing = TRUE)
actual_output_8 <- plot_assessment_plots(data = test_data_5, method = "clsi", type = "sd_vs_concentration", draw_curves = "lm", plot_theme = "default", testing = TRUE)
actual_output_9 <- plot_assessment_plots(data = test_data_4, method = "clsi", type = "cv_vs_concentration", draw_curves = "lm", plot_theme = "default", testing = TRUE)
actual_output_10 <- plot_assessment_plots(data = test_data_2, method = "clsi", type = "residual_histogram", draw_curves = "lm", plot_theme = "default", testing = TRUE)

actual_output_11 <- plot_assessment_plots(data = test_data_1, method = "ols", type = "residual_plot", draw_curves = "lm", plot_theme = "default", testing = TRUE)
actual_output_12 <- plot_assessment_plots(data = test_data_7, method = "ols", type = "qq_plot", draw_curves = "lm", plot_theme = "default", testing = TRUE)
actual_output_13 <- plot_assessment_plots(data = test_data_5, method = "ols", type = "sd_vs_concentration", draw_curves = "lm", plot_theme = "default", testing = TRUE)
actual_output_14 <- plot_assessment_plots(data = test_data_4, method = "ols", type = "cv_vs_concentration", draw_curves = "lm", plot_theme = "default", testing = TRUE)
actual_output_15 <- plot_assessment_plots(data = test_data_2, method = "ols", type = "residual_histogram", draw_curves = "lm", plot_theme = "default", testing = TRUE)

test_that(desc = "testing output class of plotting_data", code = {
  expect_s3_class(object = actual_output_1$plotting_data, class = "data.table")
  expect_s3_class(object = actual_output_2$plotting_data, class = "data.table")
  expect_s3_class(object = actual_output_3$plotting_data, class = "data.table")
  expect_s3_class(object = actual_output_4$plotting_data, class = "data.table")
  expect_s3_class(object = actual_output_5$plotting_data, class = "data.table")
  expect_s3_class(object = actual_output_6$plotting_data, class = "data.table")
  expect_s3_class(object = actual_output_7$plotting_data, class = "data.table")
  expect_s3_class(object = actual_output_8$plotting_data, class = "data.table")
  expect_s3_class(object = actual_output_9$plotting_data, class = "data.table")
  expect_s3_class(object = actual_output_10$plotting_data, class = "data.table")
  expect_s3_class(object = actual_output_11$plotting_data, class = "data.table")
  expect_s3_class(object = actual_output_12$plotting_data, class = "data.table")
  expect_s3_class(object = actual_output_13$plotting_data, class = "data.table")
  expect_s3_class(object = actual_output_14$plotting_data, class = "data.table")
  expect_s3_class(object = actual_output_15$plotting_data, class = "data.table")
})

test_that(desc = "testing if plotting inputs is registered by the function", code = {
  expect_equal(object = actual_output_1$other_inputs, expected = list("method" = "fg", "type" = "residual_plot", "draw_curves" = "lm"))
  expect_equal(object = actual_output_2$other_inputs, expected = list("method" = "fg", "type" = "qq_plot", "draw_curves" = "lm"))
  expect_equal(object = actual_output_3$other_inputs, expected = list("method" = "fg", "type" = "sd_vs_concentration", "draw_curves" = "lm"))
  expect_equal(object = actual_output_4$other_inputs, expected = list("method" = "fg", "type" = "cv_vs_concentration", "draw_curves" = "lm"))
  expect_equal(object = actual_output_5$other_inputs, expected = list("method" = "fg", "type" = "residual_histogram", "draw_curves" = "lm"))
  expect_equal(object = actual_output_6$other_inputs, expected = list("method" = "clsi", "type" = "residual_plot", "draw_curves" = "lm"))
  expect_equal(object = actual_output_7$other_inputs, expected = list("method" = "clsi", "type" = "qq_plot", "draw_curves" = "lm"))
  expect_equal(object = actual_output_8$other_inputs, expected = list("method" = "clsi", "type" = "sd_vs_concentration", "draw_curves" = "lm"))
  expect_equal(object = actual_output_9$other_inputs, expected = list("method" = "clsi", "type" = "cv_vs_concentration", "draw_curves" = "lm"))
  expect_equal(object = actual_output_10$other_inputs, expected = list("method" = "clsi", "type" = "residual_histogram", "draw_curves" = "lm"))
  expect_equal(object = actual_output_11$other_inputs, expected = list("method" = "ols", "type" = "residual_plot", "draw_curves" = "lm"))
  expect_equal(object = actual_output_12$other_inputs, expected = list("method" = "ols", "type" = "qq_plot", "draw_curves" = "lm"))
  expect_equal(object = actual_output_13$other_inputs, expected = list("method" = "ols", "type" = "sd_vs_concentration", "draw_curves" = "lm"))
  expect_equal(object = actual_output_14$other_inputs, expected = list("method" = "ols", "type" = "cv_vs_concentration", "draw_curves" = "lm"))
  expect_equal(object = actual_output_15$other_inputs, expected = list("method" = "ols", "type" = "residual_histogram", "draw_curves" = "lm"))
})

test_that(desc = "Testing if theme_arguments = additional_arguments, when additional_arugments are unspecified", code = {
  expect_equal(object = actual_output_1$additional_arguments, expected = actual_output_1$theme_arguments)
  expect_equal(object = actual_output_2$additional_arguments, expected = actual_output_2$theme_arguments)
  expect_equal(object = actual_output_3$additional_arguments, expected = actual_output_3$theme_arguments)
  expect_equal(object = actual_output_4$additional_arguments, expected = actual_output_4$theme_arguments)
  expect_equal(object = actual_output_5$additional_arguments, expected = actual_output_5$theme_arguments)
  expect_equal(object = actual_output_6$additional_arguments, expected = actual_output_6$theme_arguments)
  expect_equal(object = actual_output_7$additional_arguments, expected = actual_output_7$theme_arguments)
  expect_equal(object = actual_output_8$additional_arguments, expected = actual_output_8$theme_arguments)
  expect_equal(object = actual_output_9$additional_arguments, expected = actual_output_9$theme_arguments)
  expect_equal(object = actual_output_10$additional_arguments, expected = actual_output_10$theme_arguments)
  expect_equal(object = actual_output_11$additional_arguments, expected = actual_output_11$theme_arguments)
  expect_equal(object = actual_output_12$additional_arguments, expected = actual_output_12$theme_arguments)
  expect_equal(object = actual_output_13$additional_arguments, expected = actual_output_13$theme_arguments)
  expect_equal(object = actual_output_14$additional_arguments, expected = actual_output_14$theme_arguments)
  expect_equal(object = actual_output_15$additional_arguments, expected = actual_output_15$theme_arguments)
})


# Testing for draw_curves = "none"

actual_output_1 <- plot_assessment_plots(data = test_data_1, method = "fg", type = "residual_plot", draw_curves = "none", plot_theme = "default", testing = TRUE)
actual_output_2 <- plot_assessment_plots(data = test_data_7, method = "fg", type = "qq_plot", draw_curves = "none", plot_theme = "default", testing = TRUE)
actual_output_3 <- plot_assessment_plots(data = test_data_5, method = "fg", type = "sd_vs_concentration", draw_curves = "none", plot_theme = "default", testing = TRUE)
actual_output_4 <- plot_assessment_plots(data = test_data_4, method = "fg", type = "cv_vs_concentration", draw_curves = "none", plot_theme = "default", testing = TRUE)
actual_output_5 <- plot_assessment_plots(data = test_data_2, method = "fg", type = "residual_histogram", draw_curves = "none", plot_theme = "default", testing = TRUE)

actual_output_6 <- plot_assessment_plots(data = test_data_1, method = "clsi", type = "residual_plot", draw_curves = "none", plot_theme = "default", testing = TRUE)
actual_output_7 <- plot_assessment_plots(data = test_data_7, method = "clsi", type = "qq_plot", draw_curves = "none", plot_theme = "default", testing = TRUE)
actual_output_8 <- plot_assessment_plots(data = test_data_5, method = "clsi", type = "sd_vs_concentration", draw_curves = "none", plot_theme = "default", testing = TRUE)
actual_output_9 <- plot_assessment_plots(data = test_data_4, method = "clsi", type = "cv_vs_concentration", draw_curves = "none", plot_theme = "default", testing = TRUE)
actual_output_10 <- plot_assessment_plots(data = test_data_2, method = "clsi", type = "residual_histogram", draw_curves = "none", plot_theme = "default", testing = TRUE)

actual_output_11 <- plot_assessment_plots(data = test_data_1, method = "ols", type = "residual_plot", draw_curves = "none", plot_theme = "default", testing = TRUE)
actual_output_12 <- plot_assessment_plots(data = test_data_7, method = "ols", type = "qq_plot", draw_curves = "none", plot_theme = "default", testing = TRUE)
actual_output_13 <- plot_assessment_plots(data = test_data_5, method = "ols", type = "sd_vs_concentration", draw_curves = "none", plot_theme = "default", testing = TRUE)
actual_output_14 <- plot_assessment_plots(data = test_data_4, method = "ols", type = "cv_vs_concentration", draw_curves = "none", plot_theme = "default", testing = TRUE)
actual_output_15 <- plot_assessment_plots(data = test_data_2, method = "ols", type = "residual_histogram", draw_curves = "none", plot_theme = "default", testing = TRUE)

test_that(desc = "testing output class of plotting_data", code = {
  expect_s3_class(object = actual_output_1$plotting_data, class = "data.table")
  expect_s3_class(object = actual_output_2$plotting_data, class = "data.table")
  expect_s3_class(object = actual_output_3$plotting_data, class = "data.table")
  expect_s3_class(object = actual_output_4$plotting_data, class = "data.table")
  expect_s3_class(object = actual_output_5$plotting_data, class = "data.table")
  expect_s3_class(object = actual_output_6$plotting_data, class = "data.table")
  expect_s3_class(object = actual_output_7$plotting_data, class = "data.table")
  expect_s3_class(object = actual_output_8$plotting_data, class = "data.table")
  expect_s3_class(object = actual_output_9$plotting_data, class = "data.table")
  expect_s3_class(object = actual_output_10$plotting_data, class = "data.table")
  expect_s3_class(object = actual_output_11$plotting_data, class = "data.table")
  expect_s3_class(object = actual_output_12$plotting_data, class = "data.table")
  expect_s3_class(object = actual_output_13$plotting_data, class = "data.table")
  expect_s3_class(object = actual_output_14$plotting_data, class = "data.table")
  expect_s3_class(object = actual_output_15$plotting_data, class = "data.table")
})

test_that(desc = "testing if plotting inputs is registered by the function", code = {
  expect_equal(object = actual_output_1$other_inputs, expected = list("method" = "fg", "type" = "residual_plot", "draw_curves" = "none"))
  expect_equal(object = actual_output_2$other_inputs, expected = list("method" = "fg", "type" = "qq_plot", "draw_curves" = "none"))
  expect_equal(object = actual_output_3$other_inputs, expected = list("method" = "fg", "type" = "sd_vs_concentration", "draw_curves" = "none"))
  expect_equal(object = actual_output_4$other_inputs, expected = list("method" = "fg", "type" = "cv_vs_concentration", "draw_curves" = "none"))
  expect_equal(object = actual_output_5$other_inputs, expected = list("method" = "fg", "type" = "residual_histogram", "draw_curves" = "none"))
  expect_equal(object = actual_output_6$other_inputs, expected = list("method" = "clsi", "type" = "residual_plot", "draw_curves" = "none"))
  expect_equal(object = actual_output_7$other_inputs, expected = list("method" = "clsi", "type" = "qq_plot", "draw_curves" = "none"))
  expect_equal(object = actual_output_8$other_inputs, expected = list("method" = "clsi", "type" = "sd_vs_concentration", "draw_curves" = "none"))
  expect_equal(object = actual_output_9$other_inputs, expected = list("method" = "clsi", "type" = "cv_vs_concentration", "draw_curves" = "none"))
  expect_equal(object = actual_output_10$other_inputs, expected = list("method" = "clsi", "type" = "residual_histogram", "draw_curves" = "none"))
  expect_equal(object = actual_output_11$other_inputs, expected = list("method" = "ols", "type" = "residual_plot", "draw_curves" = "none"))
  expect_equal(object = actual_output_12$other_inputs, expected = list("method" = "ols", "type" = "qq_plot", "draw_curves" = "none"))
  expect_equal(object = actual_output_13$other_inputs, expected = list("method" = "ols", "type" = "sd_vs_concentration", "draw_curves" = "none"))
  expect_equal(object = actual_output_14$other_inputs, expected = list("method" = "ols", "type" = "cv_vs_concentration", "draw_curves" = "none"))
  expect_equal(object = actual_output_15$other_inputs, expected = list("method" = "ols", "type" = "residual_histogram", "draw_curves" = "none"))
})

test_that(desc = "Testing if theme_arguments = additional_arguments, when additional_arugments are unspecified", code = {
  expect_equal(object = actual_output_1$additional_arguments, expected = actual_output_1$theme_arguments)
  expect_equal(object = actual_output_2$additional_arguments, expected = actual_output_2$theme_arguments)
  expect_equal(object = actual_output_3$additional_arguments, expected = actual_output_3$theme_arguments)
  expect_equal(object = actual_output_4$additional_arguments, expected = actual_output_4$theme_arguments)
  expect_equal(object = actual_output_5$additional_arguments, expected = actual_output_5$theme_arguments)
  expect_equal(object = actual_output_6$additional_arguments, expected = actual_output_6$theme_arguments)
  expect_equal(object = actual_output_7$additional_arguments, expected = actual_output_7$theme_arguments)
  expect_equal(object = actual_output_8$additional_arguments, expected = actual_output_8$theme_arguments)
  expect_equal(object = actual_output_9$additional_arguments, expected = actual_output_9$theme_arguments)
  expect_equal(object = actual_output_10$additional_arguments, expected = actual_output_10$theme_arguments)
  expect_equal(object = actual_output_11$additional_arguments, expected = actual_output_11$theme_arguments)
  expect_equal(object = actual_output_12$additional_arguments, expected = actual_output_12$theme_arguments)
  expect_equal(object = actual_output_13$additional_arguments, expected = actual_output_13$theme_arguments)
  expect_equal(object = actual_output_14$additional_arguments, expected = actual_output_14$theme_arguments)
  expect_equal(object = actual_output_15$additional_arguments, expected = actual_output_15$theme_arguments)
})


# Testing additivity of additional arguments and theme arguments

actual_output_1 <- plot_assessment_plots(data = test_data_1, method = "fg", type = "residual_plot", draw_curves = "loess", plot_theme = "default", additional_arguments = list(main_title = "Some amazing title", sub_title = "Some lame subtitle"), testing = TRUE)
actual_output_2 <- plot_assessment_plots(data = test_data_7, method = "fg", type = "qq_plot", draw_curves = "loess", plot_theme = "default", additional_arguments = list(n_breaks = 8, curve_color = "green"), testing = TRUE)
actual_output_3 <- plot_assessment_plots(data = test_data_5, method = "fg", type = "sd_vs_concentration", draw_curves = "loess", plot_theme = "default", additional_arguments = list(curve_se_fill = "blue", curve_se = TRUE, main_title = "bla bla"), testing = TRUE)
actual_output_4 <- plot_assessment_plots(data = test_data_4, method = "fg", type = "cv_vs_concentration", draw_curves = "loess", plot_theme = "default", additional_arguments = list(curve_se_fill = "blue", histogram_fill = "green"), testing = TRUE)
actual_output_5 <- plot_assessment_plots(data = test_data_2, method = "fg", type = "residual_histogram", draw_curves = "loess", plot_theme = "default", additional_arguments = list(histogram_border = "yellow", n_breaks = 13, curve_se = TRUE), testing = TRUE)

test_that(desc = "Testing additivity of additional arguments and theme arguments", code = {
  expect_true(object = length(actual_output_1$additional_arguments) == length(actual_output_1$theme_arguments) + 2)
  expect_true(object = length(actual_output_2$additional_arguments) == length(actual_output_1$theme_arguments) + 1)
  expect_true(object = length(actual_output_3$additional_arguments) == length(actual_output_1$theme_arguments) + 2)
  expect_true(object = length(actual_output_4$additional_arguments) == length(actual_output_1$theme_arguments))
  expect_true(object = length(actual_output_5$additional_arguments) == length(actual_output_1$theme_arguments) + 3)
})


# Testing whether the correct plots are returned

random_method <- sample(x = c("fg","clsi","ols"), size = 10, replace = TRUE)
random_curves <- sample(x = c("none","loess","lm"), size = 10, replace = TRUE)
random_types <- sample(x = c("residual_plot", "qq_plot", "sd_vs_concentration", "cv_vs_concentration", "residual_histogram"), size = 10, replace = TRUE)
random_themes <- sample(x = c("default", "noklus", "soft", "depression", "happy"), size = 10, replace = TRUE)

actual_output_1 <- plot_assessment_plots(data = test_data_1, method = random_method[1], type = random_types[1], draw_curves = random_curves[1], plot_theme = random_themes[1], testing = "plot")
actual_output_2 <- plot_assessment_plots(data = test_data_7, method = random_method[2], type = random_types[2], draw_curves = random_curves[2], plot_theme = random_themes[2], testing = "plot")
actual_output_3 <- plot_assessment_plots(data = test_data_5, method = random_method[3], type = random_types[3], draw_curves = random_curves[3], plot_theme = random_themes[3], testing = "plot")
actual_output_4 <- plot_assessment_plots(data = test_data_4, method = random_method[4], type = random_types[4], draw_curves = random_curves[4], plot_theme = random_themes[4], testing = "plot")
actual_output_5 <- plot_assessment_plots(data = test_data_2, method = random_method[5], type = random_types[5], draw_curves = random_curves[5], plot_theme = random_themes[5], testing = "plot")

actual_output_6 <- plot_assessment_plots(data = test_data_1, method = random_method[6], type = random_types[6], draw_curves = random_curves[6], plot_theme = random_themes[6], testing = "plot")
actual_output_7 <- plot_assessment_plots(data = test_data_7, method = random_method[7], type = random_types[7], draw_curves = random_curves[7], plot_theme = random_themes[7], testing = "plot")
actual_output_8 <- plot_assessment_plots(data = test_data_5, method = random_method[8], type = random_types[8], draw_curves = random_curves[8], plot_theme = random_themes[8], testing = "plot")
actual_output_9 <- plot_assessment_plots(data = test_data_4, method = random_method[9], type = random_types[9], draw_curves = random_curves[9], plot_theme = random_themes[9], testing = "plot")
actual_output_10 <- plot_assessment_plots(data = test_data_2, method = random_method[10], type = random_types[10], draw_curves = random_curves[10], plot_theme = random_themes[10], testing = "plot")

test_that(desc = "Testing whether plots are correct", code = {
  expect_equal(object = actual_output_1, expected = list("curves" = any(random_curves[1] == c("loess", "lm")), "which_curves" = if(random_types[1] == "residual_histogram" & any(random_curves[1] == c("loess", "lm"))){"density"}else{random_curves[1]}, "which_plot" = random_types[1]))
  expect_equal(object = actual_output_2, expected = list("curves" = any(random_curves[2] == c("loess", "lm")), "which_curves" = if(random_types[2] == "residual_histogram" & any(random_curves[2] == c("loess", "lm"))){"density"}else{random_curves[2]}, "which_plot" = random_types[2]))
  expect_equal(object = actual_output_3, expected = list("curves" = any(random_curves[3] == c("loess", "lm")), "which_curves" = if(random_types[3] == "residual_histogram" & any(random_curves[3] == c("loess", "lm"))){"density"}else{random_curves[3]}, "which_plot" = random_types[3]))
  expect_equal(object = actual_output_4, expected = list("curves" = any(random_curves[4] == c("loess", "lm")), "which_curves" = if(random_types[4] == "residual_histogram" & any(random_curves[4] == c("loess", "lm"))){"density"}else{random_curves[4]}, "which_plot" = random_types[4]))
  expect_equal(object = actual_output_5, expected = list("curves" = any(random_curves[5] == c("loess", "lm")), "which_curves" = if(random_types[5] == "residual_histogram" & any(random_curves[5] == c("loess", "lm"))){"density"}else{random_curves[5]}, "which_plot" = random_types[5]))
  expect_equal(object = actual_output_6, expected = list("curves" = any(random_curves[6] == c("loess", "lm")), "which_curves" = if(random_types[6] == "residual_histogram" & any(random_curves[6] == c("loess", "lm"))){"density"}else{random_curves[6]}, "which_plot" = random_types[6]))
  expect_equal(object = actual_output_7, expected = list("curves" = any(random_curves[7] == c("loess", "lm")), "which_curves" = if(random_types[7] == "residual_histogram" & any(random_curves[7] == c("loess", "lm"))){"density"}else{random_curves[7]}, "which_plot" = random_types[7]))
  expect_equal(object = actual_output_8, expected = list("curves" = any(random_curves[8] == c("loess", "lm")), "which_curves" = if(random_types[8] == "residual_histogram" & any(random_curves[8] == c("loess", "lm"))){"density"}else{random_curves[8]}, "which_plot" = random_types[8]))
  expect_equal(object = actual_output_9, expected = list("curves" = any(random_curves[9] == c("loess", "lm")), "which_curves" = if(random_types[9] == "residual_histogram" & any(random_curves[9] == c("loess", "lm"))){"density"}else{random_curves[9]}, "which_plot" = random_types[9]))
  expect_equal(object = actual_output_10, expected = list("curves" = any(random_curves[10] == c("loess", "lm")), "which_curves" = if(random_types[10] == "residual_histogram" & any(random_curves[10] == c("loess", "lm"))){"density"}else{random_curves[10]}, "which_plot" = random_types[10]))
})

random_method_new <- sample(x = c("fg","clsi","ols"), size = 10, replace = TRUE)
random_curves_new <- sample(x = c("none","loess","lm"), size = 10, replace = TRUE)
random_types_new <- sample(x = c("sd_vs_concentration", "cv_vs_concentration"), size = 10, replace = TRUE)
random_themes <- sample(x = c("default", "noklus", "soft", "depression", "happy"), size = 10, replace = TRUE)
additional_args <- as.list(1:10)
for(i in 1:10){
  if(random_types_new[i] == "sd_vs_concentration"){
    additional_args[[i]] <- list("var_instead" = TRUE)
  }
  else{
    additional_args[[i]] <- list("cv_percent" = TRUE)
  }

}

actual_output_1 <- plot_assessment_plots(data = test_data_1, method = random_method_new[1], type = random_types_new[1], draw_curves = random_curves_new[1], plot_theme = random_themes[1], testing = "transformation", additional_arguments = additional_args[[1]])
actual_output_2 <- plot_assessment_plots(data = test_data_7, method = random_method_new[2], type = random_types_new[2], draw_curves = random_curves_new[2], plot_theme = random_themes[2], testing = "transformation", additional_arguments = additional_args[[2]])
actual_output_3 <- plot_assessment_plots(data = test_data_5, method = random_method_new[3], type = random_types_new[3], draw_curves = random_curves_new[3], plot_theme = random_themes[3], testing = "transformation", additional_arguments = additional_args[[3]])
actual_output_4 <- plot_assessment_plots(data = test_data_4, method = random_method_new[4], type = random_types_new[4], draw_curves = random_curves_new[4], plot_theme = random_themes[4], testing = "transformation", additional_arguments = additional_args[[4]])
actual_output_5 <- plot_assessment_plots(data = test_data_2, method = random_method_new[5], type = random_types_new[5], draw_curves = random_curves_new[5], plot_theme = random_themes[5], testing = "transformation", additional_arguments = additional_args[[5]])

actual_output_6 <- plot_assessment_plots(data = test_data_1, method = random_method_new[6], type = random_types_new[6], draw_curves = random_curves_new[6], plot_theme = random_themes[6], testing = "transformation", additional_arguments = additional_args[[6]])
actual_output_7 <- plot_assessment_plots(data = test_data_7, method = random_method_new[7], type = random_types_new[7], draw_curves = random_curves_new[7], plot_theme = random_themes[7], testing = "transformation", additional_arguments = additional_args[[7]])
actual_output_8 <- plot_assessment_plots(data = test_data_5, method = random_method_new[8], type = random_types_new[8], draw_curves = random_curves_new[8], plot_theme = random_themes[8], testing = "transformation", additional_arguments = additional_args[[8]])
actual_output_9 <- plot_assessment_plots(data = test_data_4, method = random_method_new[9], type = random_types_new[9], draw_curves = random_curves_new[9], plot_theme = random_themes[9], testing = "transformation", additional_arguments = additional_args[[9]])
actual_output_10 <- plot_assessment_plots(data = test_data_2, method = random_method_new[10], type = random_types_new[10], draw_curves = random_curves_new[10], plot_theme = random_themes[10], testing = "transformation", additional_arguments = additional_args[[10]])

test_that(desc = "Testing transformations", code = {
  expect_equal(object = actual_output_1, expected = if(random_types_new[1] == "sd_vs_concentration"){"sds are replaced by variances"}else{"cvs are given in percent"})
  expect_equal(object = actual_output_2, expected = if(random_types_new[2] == "sd_vs_concentration"){"sds are replaced by variances"}else{"cvs are given in percent"})
  expect_equal(object = actual_output_3, expected = if(random_types_new[3] == "sd_vs_concentration"){"sds are replaced by variances"}else{"cvs are given in percent"})
  expect_equal(object = actual_output_4, expected = if(random_types_new[4] == "sd_vs_concentration"){"sds are replaced by variances"}else{"cvs are given in percent"})
  expect_equal(object = actual_output_5, expected = if(random_types_new[5] == "sd_vs_concentration"){"sds are replaced by variances"}else{"cvs are given in percent"})
  expect_equal(object = actual_output_6, expected = if(random_types_new[6] == "sd_vs_concentration"){"sds are replaced by variances"}else{"cvs are given in percent"})
  expect_equal(object = actual_output_7, expected = if(random_types_new[7] == "sd_vs_concentration"){"sds are replaced by variances"}else{"cvs are given in percent"})
  expect_equal(object = actual_output_8, expected = if(random_types_new[8] == "sd_vs_concentration"){"sds are replaced by variances"}else{"cvs are given in percent"})
  expect_equal(object = actual_output_9, expected = if(random_types_new[9] == "sd_vs_concentration"){"sds are replaced by variances"}else{"cvs are given in percent"})
  expect_equal(object = actual_output_10, expected = if(random_types_new[10] == "sd_vs_concentration"){"sds are replaced by variances"}else{"cvs are given in percent"})
})

# Testing custom themes

set.seed(2)
random_method <- sample(x = c("fg","clsi","ols"), size = 10, replace = TRUE)
random_curves <- sample(x = c("none","loess","lm"), size = 10, replace = TRUE)
random_types <- sample(x = c("residual_plot", "qq_plot", "sd_vs_concentration", "cv_vs_concentration", "residual_histogram"), size = 10, replace = TRUE)

# When inputting "custom"
actual_output_1 <- plot_assessment_plots(data = test_data_1, method = random_method_new[1], type = random_types_new[2], draw_curves = random_curves_new[1], plot_theme = "custom", testing = TRUE)
actual_output_2 <- plot_assessment_plots(data = test_data_7, method = random_method_new[2], type = random_types_new[3], draw_curves = random_curves_new[2], plot_theme = "custom", testing = TRUE)
actual_output_3 <- plot_assessment_plots(data = test_data_5, method = random_method_new[3], type = random_types_new[1], draw_curves = random_curves_new[3], plot_theme = "custom", testing = TRUE)
actual_output_4 <- plot_assessment_plots(data = test_data_4, method = random_method_new[4], type = random_types_new[3], draw_curves = random_curves_new[4], plot_theme = "custom", testing = TRUE)
actual_output_5 <- plot_assessment_plots(data = test_data_2, method = random_method_new[5], type = random_types_new[4], draw_curves = random_curves_new[5], plot_theme = "custom", testing = TRUE)

# Without inputting "custom"
actual_output_6 <- plot_assessment_plots(data = test_data_1, method = random_method_new[1], type = random_types_new[2], draw_curves = random_curves_new[1], testing = TRUE)
actual_output_7 <- plot_assessment_plots(data = test_data_7, method = random_method_new[2], type = random_types_new[3], draw_curves = random_curves_new[2], testing = TRUE)
actual_output_8 <- plot_assessment_plots(data = test_data_5, method = random_method_new[3], type = random_types_new[1], draw_curves = random_curves_new[3], testing = TRUE)
actual_output_9 <- plot_assessment_plots(data = test_data_4, method = random_method_new[4], type = random_types_new[3], draw_curves = random_curves_new[4], testing = TRUE)
actual_output_10 <- plot_assessment_plots(data = test_data_2, method = random_method_new[5], type = random_types_new[4], draw_curves = random_curves_new[5], testing = TRUE)

test_that(desc = "Checking if custom is input-independent", code = {
  expect_equal(actual_output_1, actual_output_6)
  expect_equal(actual_output_2, actual_output_7)
  expect_equal(actual_output_3, actual_output_8)
  expect_equal(actual_output_4, actual_output_9)
  expect_equal(actual_output_5, actual_output_10)
})

pair_testing <- sample(c(TRUE, FALSE), 5, replace = TRUE)


test_that(desc = "Checking if theme_arguments is NULL when plot_theme = custom and none arguments given", code = {

  if(pair_testing[1]){
    expect_null(actual_output_1$additional_arguments)
  }
  else{
    expect_null(actual_output_6$additional_arguments)
  }

  if(pair_testing[2]){
    expect_null(actual_output_2$additional_arguments)
  }
  else{
    expect_null(actual_output_7$additional_arguments)
  }

  if(pair_testing[3]){
    expect_null(actual_output_3$additional_arguments)
  }
  else{
    expect_null(actual_output_8$additional_arguments)
  }

  if(pair_testing[4]){
    expect_null(actual_output_4$additional_arguments)
  }
  else{
    expect_null(actual_output_9$additional_arguments)
  }

  if(pair_testing[5]){
    expect_null(actual_output_5$additional_arguments)
  }
  else{
    expect_null(actual_output_10$additional_arguments)
  }


})
