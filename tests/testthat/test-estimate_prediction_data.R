library(testthat)
library(readxl)
library(data.table)
library(smooth.commutability)

test_data_1 <- read_excel(path = "~/Packages/datasets to be tested on/test_data_1.xlsx")
test_data_2 <- read_excel(path = "~/Packages/datasets to be tested on/test_data_2.xlsx")
test_data_3 <- read_excel(path = "~/Packages/datasets to be tested on/test_data_3.xlsx")
test_data_4 <- read_excel(path = "~/Packages/datasets to be tested on/test_data_4.xlsx")
test_data_5 <- read_excel(path = "~/Packages/datasets to be tested on/test_data_5.xlsx")
test_data_6 <- read_excel(path = "~/Packages/datasets to be tested on/test_data_6.xlsx")
test_data_7 <- read_excel(path = "~/Packages/datasets to be tested on/test_data_7.xlsx")

test_data_1 <- MS_wise(repair_data(test_data_1, type = "cs", remove_invalid_methods = TRUE))
test_data_2 <- MS_wise(repair_data(test_data_2, type = "cs", remove_invalid_methods = TRUE))
test_data_3 <- MS_wise(repair_data(test_data_3, type = "cs", remove_invalid_methods = TRUE))
test_data_5 <- MS_wise(repair_data(test_data_5, type = "cs", remove_invalid_methods = TRUE))
test_data_6 <- MS_wise(repair_data(test_data_6, type = "cs", remove_invalid_methods = TRUE))
test_data_7 <- MS_wise(repair_data(test_data_7, type = "cs", remove_invalid_methods = TRUE))

test_that(desc = "Check output names for PB data", code = {

  expected_names <- c("comparison", "predictor", "prediction", "lwr", "upr")

  actual_1 <- estimate_prediction_data(data = test_data_1,
                                       new_data = NULL,
                                       B = NULL,
                                       method = "fg",
                                       level = 0.95,
                                       rounding = 3,
                                       override_R_ratio = NULL,
                                       na_rm = TRUE)

  actual_2 <- estimate_prediction_data(data = test_data_1,
                                       new_data = NULL,
                                       B = NULL,
                                       method = "ssw",
                                       level = 0.95,
                                       rounding = 3,
                                       override_R_ratio = NULL,
                                       na_rm = TRUE)

  actual_3 <- estimate_prediction_data(data = test_data_7,
                                       new_data = NULL,
                                       B = NULL,
                                       method = "clsi",
                                       level = 0.95,
                                       rounding = 3,
                                       override_R_ratio = NULL,
                                       na_rm = TRUE)

  actual_4 <- estimate_prediction_data(data = test_data_7,
                                       new_data = NULL,
                                       B = NULL,
                                       method = "ss",
                                       level = 0.95,
                                       rounding = 3,
                                       override_R_ratio = NULL,
                                       na_rm = TRUE)

  actual_5 <- estimate_prediction_data(data = test_data_7,
                                       new_data = NULL,
                                       B = NULL,
                                       method = "ols",
                                       level = 0.95,
                                       rounding = 3,
                                       override_R_ratio = NULL,
                                       na_rm = TRUE)

  expect_named(object = actual_1, expected = expected_names)
  expect_named(object = actual_2, expected = expected_names)
  expect_named(object = actual_3, expected = expected_names)
  expect_named(object = actual_4, expected = expected_names)
  expect_named(object = actual_5, expected = expected_names)



})

test_that(desc = "Check output names for CE data", code = {

  # Expected names
  expected_names_w0_inside_rates <- c("comparison",
                                      "SampleID",
                                      "MP_B",
                                      "MP_A",
                                      "prediction",
                                      "lwr",
                                      "upr",
                                      "inside",
                                      "extrapolate")
  expected_names_w_inside_rates <- c(expected_names_w0_inside_rates,
                                     "inside_rate")

  # Test data
  test_cs_data <- test_data_2[SampleID != "1"]
  test_eq_data <- test_data_2[SampleID == "1",
                              fun_of_replicates(.SD),
                              by = comparison]

  actual_1 <- estimate_prediction_data(data = test_cs_data,
                                       new_data = test_eq_data,
                                       B = NULL,
                                       method = "fg",
                                       level = 0.99,
                                       rounding = 3L,
                                       override_R_ratio = NULL,
                                       na_rm = TRUE)

  actual_2 <- estimate_prediction_data(data = test_cs_data,
                                       new_data = test_eq_data,
                                       B = NULL,
                                       method = "ss",
                                       level = 0.99,
                                       rounding = 3L,
                                       override_R_ratio = NULL,
                                       na_rm = TRUE)

  actual_3 <- estimate_prediction_data(data = test_cs_data,
                                       new_data = test_eq_data,
                                       B = 20,
                                       method = "clsi",
                                       level = 0.99,
                                       rounding = 3L,
                                       override_R_ratio = NULL,
                                       na_rm = TRUE)

  actual_4 <- estimate_prediction_data(data = test_cs_data,
                                       new_data = test_eq_data,
                                       B = 20,
                                       method = "ssw",
                                       level = 0.99,
                                       rounding = 3L,
                                       override_R_ratio = NULL,
                                       na_rm = TRUE)

  expect_named(object = actual_1, expected = expected_names_w0_inside_rates)
  expect_named(object = actual_2, expected = expected_names_w0_inside_rates)
  expect_named(object = actual_3, expected = expected_names_w_inside_rates)
  expect_named(object = actual_4, expected = expected_names_w_inside_rates)

})

test_that(desc = "Check if comparison order is kept", code = {

  # Test data
  test_cs_data <- test_data_6[SampleID != "sample id 1"]
  test_eq_data <- test_data_6[SampleID == "sample id 1",
                              fun_of_replicates(.SD),
                              by = comparison]

  # Order before
  comparison_order_input <- unique(test_cs_data$comparison)

  actual_1 <- unique(estimate_prediction_data(data = test_cs_data,
                                              new_data = NULL,
                                              B = NULL,
                                              method = "fg")$comparison)
  actual_2 <- unique(estimate_prediction_data(data = test_cs_data,
                                              new_data = NULL,
                                              B = NULL,
                                              method = "ssw")$comparison)
  actual_3 <- unique(estimate_prediction_data(data = test_cs_data,
                                              new_data = test_eq_data,
                                              B = NULL,
                                              method = "clsi")$comparison)
  actual_4 <- unique(estimate_prediction_data(data = test_cs_data,
                                              new_data = test_eq_data,
                                              B = NULL,
                                              method = "ss")$comparison)
  actual_5 <- unique(estimate_prediction_data(data = test_cs_data,
                                              new_data = test_eq_data,
                                              B = 10,
                                              method = "ols")$comparison)
  actual_6 <- unique(estimate_prediction_data(data = test_cs_data,
                                              new_data = test_eq_data,
                                              B = 10,
                                              method = "ssw")$comparison)


  expect_equal(object = actual_1, expected = comparison_order_input)
  expect_equal(object = actual_2, expected = comparison_order_input)
  expect_equal(object = actual_3, expected = comparison_order_input)
  expect_equal(object = actual_4, expected = comparison_order_input)
  expect_equal(object = actual_5, expected = comparison_order_input)
  expect_equal(object = actual_6, expected = comparison_order_input)

})

test_that(desc = "Check if input values are found in output", code = {

  # Test data
  test_cs_data <- test_data_3[SampleID != "Fresh sample 1" & SampleID != "Fresh sample 19"]
  test_eq_data <- test_data_3[SampleID == "Fresh sample 1" | SampleID == "Fresh sample 19"]

  actual_1 <- estimate_prediction_data(data = test_cs_data,
                                       new_data = test_eq_data,
                                       B = NULL,
                                       method = "fg",
                                       rounding = 12)

  for(comp in unique(test_eq_data$comparison)){
    comp_test_eq_data <- test_eq_data[comparison == comp, fun_of_replicates(.SD)]
    comp_actual_1 <- actual_1[comparison == comp]
    expect_setequal(object = comp_actual_1$SampleID,
                    expected = comp_test_eq_data$SampleID)
    expect_setequal(object = comp_actual_1$MP_B,
                    expected = comp_test_eq_data$MP_B)
    expect_setequal(object = comp_actual_1$MP_A,
                    expected = comp_test_eq_data$MP_A)
  }








})

test_that(desc = "Testing some expected errors and warnings", code = {
  expect_error(object = estimate_prediction_data(data = "yes"), regexp = "character")
  expect_error(object = estimate_prediction_data(data = 1L), regexp = "integer")
  expect_error(object = estimate_prediction_data(data = test_data_1[,-c("comparison")]), regexp = "comparison")
  expect_error(object = estimate_prediction_data(data = test_data_1[,-c("comparison", "MP_A")]), regexp = "comparison, MP_A")
  expect_error(object = estimate_prediction_data(data = test_data_1, new_data = "gen_1!00"), regexp = "Invalid input")
  expect_warning(object = estimate_prediction_data(data = test_data_1, new_data = "gen!1A00999B9"), regexp = "Defaulting to n = 100")
  expect_warning(object = estimate_prediction_data(data = test_data_1, new_data = "AB1CltR#gen"), regexp = "Defaulting to n = 100")
})

test_that(desc = "Some general tests", code = {

  # Some real data
  test_cs_data <- copy(crp_cs_data)
  test_eq_data <- copy(crp_eqam_data)
  test_eq_data <- test_eq_data[!(comparison == "AQT90 - Chroma" & SampleID == 1)]

  for(test_method in c("ols", "fg", "clsi", "ss", "ssw")){

    # Expect to run without errors or warnings
    expect_no_error(object = estimate_prediction_data(data = test_cs_data,
                                                      new_data = "gen#100",
                                                      B = 1e4,
                                                      method = test_method,
                                                      level = 0.99,
                                                      rounding = 2L,
                                                      override_R_ratio = NULL,
                                                      na_rm = TRUE))

    # Expect data.table PB data with:
    # (1) Exactly 1000 rows
    # (2) Exactly 5 columns
    # (3) Columns should be named 'comparison', 'predictor', 'prediction', 'lwr', 'upr' in that order
    actual_1 <- estimate_prediction_data(data = test_cs_data,
                                         new_data = "gen#100",
                                         B = 1e4,
                                         method = test_method,
                                         level = 0.99,
                                         rounding = 2L,
                                         override_R_ratio = NULL,
                                         na_rm = TRUE)

    expect_true(object = is.data.table(actual_1))
    expect_true(object = nrow(actual_1) == 1000L)
    expect_true(object = ncol(actual_1) == 5L)
    expect_named(object = actual_1,
                 expected = c('comparison',
                              'predictor',
                              'prediction',
                              'lwr',
                              'upr'),
                 ignore.order = FALSE,
                 ignore.case = FALSE)

    # Expect to run without errors or warnings
    expect_no_error(object = estimate_prediction_data(data = test_cs_data,
                                                      new_data = test_eq_data,
                                                      B = 23,
                                                      method = test_method,
                                                      level = 0.99,
                                                      rounding = 2L,
                                                      override_R_ratio = NULL,
                                                      na_rm = TRUE))

    # Expect data.table CE data with:
    # (1) Exactly 39 rows
    # (2) Exactly 10 columns
    # (3) Columns should be named:
    # 'comparison', 'SampleID', 'MP_B', 'MP_A' 'prediction', 'lwr', 'upr', 'inside', 'extrapolation', 'inside_rate' in that order
    actual_2 <- estimate_prediction_data(data = test_cs_data,
                                         new_data = test_eq_data,
                                         B = 23,
                                         method = test_method,
                                         level = 0.99,
                                         rounding = 2L,
                                         override_R_ratio = NULL,
                                         na_rm = TRUE)

    expected_names <- c("comparison",
                        "SampleID",
                        "MP_B",
                        "MP_A",
                        "prediction",
                        "lwr",
                        "upr",
                        "inside",
                        "extrapolate",
                        "inside_rate")

    expect_true(object = is.data.table(actual_2))
    expect_true(object = nrow(actual_2) == 39L)
    expect_true(object = ncol(actual_2) == 10L)
    expect_named(object = actual_2,
                 expected = expected_names,
                 ignore.order = FALSE,
                 ignore.case = FALSE)

  }
})
