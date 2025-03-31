library(testthat)
library(readxl)
library(data.table)
library(fasteqa)

# Reproducibility
set.seed(1)

# Read raw testing data
test_data_1 <- read_excel(path = "~/Packages/datasets to be tested on/test_data_1.xlsx")
test_data_2 <- read_excel(path = "~/Packages/datasets to be tested on/test_data_2.xlsx")
test_data_3 <- read_excel(path = "~/Packages/datasets to be tested on/test_data_3.xlsx")
test_data_4 <- read_excel(path = "~/Packages/datasets to be tested on/test_data_4.xlsx")
test_data_5 <- read_excel(path = "~/Packages/datasets to be tested on/test_data_5.xlsx")
test_data_6 <- read_excel(path = "~/Packages/datasets to be tested on/test_data_6.xlsx")
test_data_7 <- read_excel(path = "~/Packages/datasets to be tested on/test_data_7.xlsx")

# Checking the following intervals
testing_types <- sample(
  x = c("normal", "basic", "percentile", "BCa"),
  size = 7,
  replace = TRUE
)

# Checking the following confidence levels
testing_levels <- runif(n = 7, min = 0.80, max = 0.99)

# Get testing data. If statement is there because we then can collapse...
if (1 < 2) {
  test_data_1 <- estimate_imprecision_data(
    data = get_comparison_data(
      data = repair_data(
        data = test_data_1,
        type = "cs",
        remove_invalid_methods = TRUE,
        include_repair_summary = FALSE
      )
    ),
    B = 1000,
    type = testing_types[1],
    level = testing_levels[1],
    invalid_NA = FALSE
  )
  test_data_2 <- estimate_imprecision_data(
    data = get_comparison_data(
      data = repair_data(
        data = test_data_2,
        type = "cs",
        remove_invalid_methods = TRUE,
        include_repair_summary = FALSE
      )
    ),
    B = 1000,
    type = testing_types[2],
    level = testing_levels[2],
    invalid_NA = FALSE
  )
  test_data_3 <- estimate_imprecision_data(
    data = get_comparison_data(
      data = repair_data(
        data = test_data_3,
        type = "cs",
        remove_invalid_methods = TRUE,
        include_repair_summary = FALSE
      )
    ),
    B = 1000,
    type = testing_types[3],
    level = testing_levels[3],
    invalid_NA = FALSE
  )
  test_data_4 <- estimate_imprecision_data(
    data = get_comparison_data(
      data = repair_data(
        data = as.data.table(test_data_4)[, -c("IVD_MD_4")],
        type = "cs",
        remove_invalid_methods = TRUE,
        include_repair_summary = FALSE
      )
    ),
    B = 1000,
    type = testing_types[4],
    level = testing_levels[4],
    invalid_NA = FALSE
  )
  test_data_5 <- estimate_imprecision_data(
    data = get_comparison_data(
      data = repair_data(
        data = test_data_5,
        type = "cs",
        remove_invalid_methods = TRUE,
        include_repair_summary = FALSE
      )
    ),
    B = 1000,
    type = testing_types[5],
    level = testing_levels[5],
    invalid_NA = FALSE
  )
  test_data_6 <- estimate_imprecision_data(
    data = get_comparison_data(
      data = repair_data(
        data = test_data_6,
        type = "cs",
        remove_invalid_methods = TRUE,
        include_repair_summary = FALSE
      )
    ),
    B = 1000,
    type = testing_types[6],
    level = testing_levels[6],
    invalid_NA = FALSE
  )
  test_data_7 <- estimate_imprecision_data(
    data = get_comparison_data(
      data = repair_data(
        data = test_data_7,
        type = "cs",
        remove_invalid_methods = TRUE,
        include_repair_summary = FALSE
      )
    ),
    B = 1000,
    type = testing_types[7],
    level = testing_levels[7],
    invalid_NA = FALSE
  )
}

test_that(desc = "Testing output structure for visual", code = {

  actual_1 <- MS_wise_imprecision(imprecision_data = test_data_1)
  actual_2 <- MS_wise_imprecision(imprecision_data = test_data_2)
  actual_3 <- MS_wise_imprecision(imprecision_data = test_data_3)
  actual_4 <- MS_wise_imprecision(imprecision_data = test_data_4)
  actual_5 <- MS_wise_imprecision(imprecision_data = test_data_5)
  actual_6 <- MS_wise_imprecision(imprecision_data = test_data_6)
  actual_7 <- MS_wise_imprecision(imprecision_data = test_data_7)

  expected_rows_1 <- 4L
  expected_rows_2 <- 4L
  expected_rows_3 <- 6L
  expected_rows_4 <- 5L
  expected_rows_5 <- 6L
  expected_rows_6 <- 6L
  expected_rows_7 <- 4L

  expect_true(object = is.data.table(actual_1))
  expect_true(object = is.data.table(actual_2))
  expect_true(object = is.data.table(actual_3))
  expect_true(object = is.data.table(actual_4))
  expect_true(object = is.data.table(actual_5))
  expect_true(object = is.data.table(actual_6))
  expect_true(object = is.data.table(actual_7))

  expect_true(object = all(lapply(X = actual_1, FUN = is.character) |> unlist()))
  expect_true(object = all(lapply(X = actual_2, FUN = is.character) |> unlist()))
  expect_true(object = all(lapply(X = actual_3, FUN = is.character) |> unlist()))
  expect_true(object = all(lapply(X = actual_4, FUN = is.character) |> unlist()))
  expect_true(object = all(lapply(X = actual_5, FUN = is.character) |> unlist()))
  expect_true(object = all(lapply(X = actual_6, FUN = is.character) |> unlist()))
  expect_true(object = all(lapply(X = actual_7, FUN = is.character) |> unlist()))

  expect_equal(object = length(unique(actual_1$MP)), expected = expected_rows_1)
  expect_equal(object = length(unique(actual_2$MP)), expected = expected_rows_2)
  expect_equal(object = length(unique(actual_3$MP)), expected = expected_rows_3)
  expect_equal(object = length(unique(actual_4$MP)), expected = expected_rows_4)
  expect_equal(object = length(unique(actual_5$MP)), expected = expected_rows_5)
  expect_equal(object = length(unique(actual_6$MP)), expected = expected_rows_6)
  expect_equal(object = length(unique(actual_7$MP)), expected = expected_rows_7)

})

test_that(desc = "Testing output structure for exact", code = {

  # Expected Number of Rows
  expected_rows_1 <- 4L
  expected_rows_2 <- 4L
  expected_rows_3 <- 6L
  expected_rows_4 <- 5L
  expected_rows_5 <- 6L
  expected_rows_6 <- 6L
  expected_rows_7 <- 4L

  # Actual output
  actual_1 <- MS_wise_imprecision(imprecision_data = test_data_1, mode = "exact")
  actual_2 <- MS_wise_imprecision(imprecision_data = test_data_2, mode = "exact")
  actual_3 <- MS_wise_imprecision(imprecision_data = test_data_3, mode = "exact")
  actual_4 <- MS_wise_imprecision(imprecision_data = test_data_4, mode = "exact")
  actual_5 <- MS_wise_imprecision(imprecision_data = test_data_5, mode = "exact")
  actual_6 <- MS_wise_imprecision(imprecision_data = test_data_6, mode = "exact")
  actual_7 <- MS_wise_imprecision(imprecision_data = test_data_7, mode = "exact")

  # Check if output is data.table
  expect_true(object = is.data.table(actual_1))
  expect_true(object = is.data.table(actual_2))
  expect_true(object = is.data.table(actual_3))
  expect_true(object = is.data.table(actual_4))
  expect_true(object = is.data.table(actual_5))
  expect_true(object = is.data.table(actual_6))
  expect_true(object = is.data.table(actual_7))

  # Check if all (expect MP column) are numeric
  expect_true(object = all(lapply(X = actual_1[,-1], FUN = is.numeric) |> unlist()))
  expect_true(object = all(lapply(X = actual_2[,-1], FUN = is.numeric) |> unlist()))
  expect_true(object = all(lapply(X = actual_3[,-1], FUN = is.numeric) |> unlist()))
  expect_true(object = all(lapply(X = actual_4[,-1], FUN = is.numeric) |> unlist()))
  expect_true(object = all(lapply(X = actual_5[,-1], FUN = is.numeric) |> unlist()))
  expect_true(object = all(lapply(X = actual_6[,-1], FUN = is.numeric) |> unlist()))
  expect_true(object = all(lapply(X = actual_7[,-1], FUN = is.numeric) |> unlist()))

  # Check number of rows
  expect_equal(object = length(unique(actual_1$MP)), expected = expected_rows_1)
  expect_equal(object = length(unique(actual_2$MP)), expected = expected_rows_2)
  expect_equal(object = length(unique(actual_3$MP)), expected = expected_rows_3)
  expect_equal(object = length(unique(actual_4$MP)), expected = expected_rows_4)
  expect_equal(object = length(unique(actual_5$MP)), expected = expected_rows_5)
  expect_equal(object = length(unique(actual_6$MP)), expected = expected_rows_6)
  expect_equal(object = length(unique(actual_7$MP)), expected = expected_rows_7)

})

test_that(desc = "Testing output structure for when only one imprecision estimate included", code = {

  # Expected Number of Rows
  expected_rows_1 <- 4L
  expected_rows_2 <- 4L
  expected_rows_3 <- 6L
  expected_rows_4 <- 5L
  expected_rows_5 <- 6L
  expected_rows_6 <- 6L
  expected_rows_7 <- 4L

  # Actual output
  actual_1 <- MS_wise_imprecision(test_data_1[, -c("CV_A","CV_A_lwr","CV_A_upr","CV_B", "CV_B_lwr", "CV_B_upr")], mode = "exact")
  actual_2 <- MS_wise_imprecision(test_data_2[, -c("CV_A","CV_A_lwr","CV_A_upr","CV_B", "CV_B_lwr", "CV_B_upr")])
  actual_3 <- MS_wise_imprecision(test_data_3[, -c("CV_A","CV_A_lwr","CV_A_upr","CV_B", "CV_B_lwr", "CV_B_upr")], mode = "exact")
  actual_4 <- MS_wise_imprecision(test_data_4[, -c("CV_A","CV_A_lwr","CV_A_upr","CV_B", "CV_B_lwr", "CV_B_upr")])
  actual_5 <- MS_wise_imprecision(test_data_5[, -c("Var_A","Var_A_lwr","Var_A_upr","Var_B", "Var_B_lwr", "Var_B_upr")], mode = "exact")
  actual_6 <- MS_wise_imprecision(test_data_6[, -c("Var_A","Var_A_lwr","Var_A_upr","Var_B", "Var_B_lwr", "Var_B_upr")])
  actual_7 <- MS_wise_imprecision(test_data_7[, -c("Var_A","Var_A_lwr","Var_A_upr","Var_B", "Var_B_lwr", "Var_B_upr")], mode = "exact")

  # Check names
  expect_named(object = actual_1, expected = c("MP", "SD", "SD_lwr", "SD_upr"))
  expect_named(object = actual_2, expected = c("MP", "SD (lwr, upr)"))
  expect_named(object = actual_3, expected = c("MP", "SD", "SD_lwr", "SD_upr"))
  expect_named(object = actual_4, expected = c("MP", "SD (lwr, upr)"))
  expect_named(object = actual_5, expected = c("MP", "CV", "CV_lwr", "CV_upr"))
  expect_named(object = actual_6, expected = c("MP", "CV (lwr, upr)"))
  expect_named(object = actual_7, expected = c("MP", "CV", "CV_lwr", "CV_upr"))

  # Check number of rows
  expect_equal(object = length(unique(actual_1$MP)), expected = expected_rows_1)
  expect_equal(object = length(unique(actual_2$MP)), expected = expected_rows_2)
  expect_equal(object = length(unique(actual_3$MP)), expected = expected_rows_3)
  expect_equal(object = length(unique(actual_4$MP)), expected = expected_rows_4)
  expect_equal(object = length(unique(actual_5$MP)), expected = expected_rows_5)
  expect_equal(object = length(unique(actual_6$MP)), expected = expected_rows_6)
  expect_equal(object = length(unique(actual_7$MP)), expected = expected_rows_7)
})

