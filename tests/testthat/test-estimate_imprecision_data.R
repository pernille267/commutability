library(testthat)
library(readxl)
library(data.table)
library(fasteqa)

# Testing datasets
T_EPK2020_CS <- setDT(read_excel("~/Packages/datasets to be tested on/W..EPK2020_CS.xlsx"))
T_HB2020_CS <- setDT(read_excel("~/Packages/datasets to be tested on/W..HB2020_CS.xlsx"))
T_LPK2020_CS <- setDT(read_excel("~/Packages/datasets to be tested on/W..LPK2020_CS.xlsx"))
T_TPK2020_CS <- setDT(read_excel("~/Packages/datasets to be tested on/W..TPK2020_CS.xlsx"))
T_HDLCXXX_CS <- setDT(read_excel("~/Packages/datasets to be tested on/HDLC.CS.FILTERED.xlsx"))

# Repaired Datasets
test_data_1 <- get_comparison_data(
  data = repair_data(
    data = T_EPK2020_CS,
    type = "cs",
    remove_invalid_methods = TRUE,
    include_repair_summary = FALSE
  )
)
test_data_2 <- get_comparison_data(
  data = repair_data(
    data = T_HB2020_CS,
    type = "cs",
    remove_invalid_methods = TRUE,
    include_repair_summary = FALSE
  )
)
test_data_3 <- get_comparison_data(
  data = repair_data(
    data = T_LPK2020_CS,
    type = "cs",
    remove_invalid_methods = TRUE,
    include_repair_summary = FALSE
  )
)
test_data_4 <- get_comparison_data(
  data = repair_data(
    data = T_TPK2020_CS,
    type = "cs",
    remove_invalid_methods = TRUE,
    include_repair_summary = FALSE
  )
)
test_data_5 <- get_comparison_data(
  data = repair_data(
    data = T_HDLCXXX_CS,
    type = "cs",
    remove_invalid_methods = TRUE,
    include_repair_summary = FALSE
  )
)

# Checking the following interval
testing_types <- sample(
  x = c("normal", "basic", "percentile", "BCa"),
  size = 5,
  replace = TRUE
)

# Actual output
actual_1 <- estimate_imprecision_data(data = test_data_1,
                                      type = testing_types[1],
                                      B = 2000)
actual_2 <- estimate_imprecision_data(data = test_data_2,
                                      type = testing_types[2],
                                      B = 2000)
actual_3 <- estimate_imprecision_data(data = test_data_3,
                                      type = testing_types[3],
                                      B = 2000)
actual_4 <- estimate_imprecision_data(data = test_data_4,
                                      type = testing_types[4],
                                      B = 2000)
actual_5 <- estimate_imprecision_data(data = test_data_5,
                                      type = testing_types[5],
                                      B = 2000)

test_that(desc = "Testing column names and their order", code = {

  # The expected output names (in this order)
  expected_names <- c("comparison",
                      "CV_A",
                      "CV_A_lwr",
                      "CV_A_upr",
                      "CV_B",
                      "CV_B_lwr",
                      "CV_B_upr",
                      "lambda",
                      "lambda_lwr",
                      "lambda_upr",
                      "Var_A",
                      "Var_A_lwr",
                      "Var_A_upr",
                      "Var_B",
                      "Var_B_lwr",
                      "Var_B_upr")

  # Check names and order of names
  expect_named(object = actual_1,
               expected = expected_names,
               ignore.order = FALSE)
  expect_named(object = actual_2,
               expected = expected_names,
               ignore.order = FALSE)
  expect_named(object = actual_3,
               expected = expected_names,
               ignore.order = FALSE)
  expect_named(object = actual_4,
               expected = expected_names,
               ignore.order = FALSE)
  expect_named(object = actual_5,
               expected = expected_names,
               ignore.order = FALSE)

})

test_that(desc = "Testing output column types", code = {
  expect_true(object = all(unlist(lapply(actual_1[,-"comparison"], is.numeric))))
  expect_true(object = all(unlist(lapply(actual_2[,-"comparison"], is.numeric))))
  expect_true(object = all(unlist(lapply(actual_3[,-"comparison"], is.numeric))))
  expect_true(object = all(unlist(lapply(actual_4[,-"comparison"], is.numeric))))
  expect_true(object = all(unlist(lapply(actual_5[,-"comparison"], is.numeric))))
  expect_type(object = actual_1$comparison, type = "character")
  expect_type(object = actual_2$comparison, type = "character")
  expect_type(object = actual_3$comparison, type = "character")
  expect_type(object = actual_4$comparison, type = "character")
  expect_type(object = actual_5$comparison, type = "character")
})

test_that(desc = "Testing output class", code = {
  expect_s3_class(object = actual_1, class = "data.table")
  expect_s3_class(object = actual_2, class = "data.table")
  expect_s3_class(object = actual_3, class = "data.table")
  expect_s3_class(object = actual_4, class = "data.table")
  expect_s3_class(object = actual_5, class = "data.table")
})

test_that(desc = "Testing for only positive values", code = {
  expect_true(all(unlist(lapply(X = actual_1[,-"comparison"], FUN = function(x) all(x >= 0 | is.na(x))))))
  expect_true(all(unlist(lapply(X = actual_2[,-"comparison"], FUN = function(x) all(x >= 0 | is.na(x))))))
  expect_true(all(unlist(lapply(X = actual_3[,-"comparison"], FUN = function(x) all(x >= 0 | is.na(x))))))
  expect_true(all(unlist(lapply(X = actual_4[,-"comparison"], FUN = function(x) all(x >= 0 | is.na(x))))))
  expect_true(all(unlist(lapply(X = actual_5[,-"comparison"], FUN = function(x) all(x >= 0 | is.na(x))))))
})

test_that(desc = "Testing whether lambda corresponds with the variances", code = {

  # Extract Var_A, Var_B and lambda
  sub_actual_1 <- actual_1[,c("Var_A", "Var_B", "lambda")]
  sub_actual_2 <- actual_2[,c("Var_A", "Var_B", "lambda")]
  sub_actual_3 <- actual_3[,c("Var_A", "Var_B", "lambda")]
  sub_actual_4 <- actual_4[,c("Var_A", "Var_B", "lambda")]
  sub_actual_5 <- actual_5[,c("Var_A", "Var_B", "lambda")]

  # Check relationship beteen Var_A, Var_B and lambda
  expect_true(all(abs(sub_actual_1$Var_A / sub_actual_1$Var_B - sub_actual_1$lambda) < 1e-6))
  expect_true(all(abs(sub_actual_2$Var_A / sub_actual_2$Var_B - sub_actual_2$lambda) < 1e-6))
  expect_true(all(abs(sub_actual_3$Var_A / sub_actual_3$Var_B - sub_actual_3$lambda) < 1e-6))
  expect_true(all(abs(sub_actual_4$Var_A / sub_actual_4$Var_B - sub_actual_4$lambda) < 1e-6))
  expect_true(all(abs(sub_actual_5$Var_A / sub_actual_5$Var_B - sub_actual_5$lambda) < 1e-6))

})
