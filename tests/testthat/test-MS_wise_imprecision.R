library(testthat)
library(commutability)
library(readxl)
suppressWarnings(library(data.table))
library(fasteqa)

set.seed(1)
test_data_1 <- read_excel(path = "~/Packages/datasets to be tested on/test_data_1.xlsx")
test_data_2 <- read_excel(path = "~/Packages/datasets to be tested on/test_data_2.xlsx")
test_data_3 <- read_excel(path = "~/Packages/datasets to be tested on/test_data_3.xlsx")
test_data_4 <- read_excel(path = "~/Packages/datasets to be tested on/test_data_4.xlsx")
test_data_5 <- read_excel(path = "~/Packages/datasets to be tested on/test_data_5.xlsx")
test_data_6 <- read_excel(path = "~/Packages/datasets to be tested on/test_data_6.xlsx")
test_data_7 <- read_excel(path = "~/Packages/datasets to be tested on/test_data_7.xlsx")

check_data_1 <- check_data(test_data_1)
check_data_2 <- check_data(test_data_2)
check_data_3 <- check_data(test_data_3)
check_data_4 <- check_data(test_data_4)
check_data_5 <- check_data(test_data_5)
check_data_6 <- check_data(test_data_6)
check_data_7 <- check_data(test_data_7)

test_data_1 <- repair_data(data = test_data_1, check_data_1) |> MS_wise() |> estimate_imprecision_data(B = 200)
test_data_2 <- repair_data(data = test_data_2, check_data_2) |> MS_wise() |> estimate_imprecision_data(B = 200)
test_data_3 <- repair_data(data = test_data_3, check_data_3) |> MS_wise() |> estimate_imprecision_data(B = 200)
test_data_4 <- repair_data(data = test_data_4, check_data_4) |> MS_wise() |> estimate_imprecision_data(B = 200)
test_data_5 <- repair_data(data = test_data_5, check_data_5) |> MS_wise() |> estimate_imprecision_data(B = 200)
test_data_6 <- repair_data(data = test_data_6, check_data_6) |> MS_wise() |> estimate_imprecision_data(B = 200)
test_data_7 <- repair_data(data = test_data_7, check_data_7) |> MS_wise() |> estimate_imprecision_data(B = 200)

actual_1 <- MS_wise_imprecision(imprecision_data = test_data_1)
actual_2 <- MS_wise_imprecision(imprecision_data = test_data_2)
actual_3 <- MS_wise_imprecision(imprecision_data = test_data_3)
actual_4 <- MS_wise_imprecision(imprecision_data = test_data_4)
actual_5 <- MS_wise_imprecision(imprecision_data = test_data_5)
actual_6 <- MS_wise_imprecision(imprecision_data = test_data_6)
actual_7 <- MS_wise_imprecision(imprecision_data = test_data_7) |> suppressWarnings()

expected_rows_1 <- 4L
expected_rows_2 <- 4L
expected_rows_3 <- 6L
expected_rows_4 <- 5L
expected_rows_5 <- 6L
expected_rows_6 <- 6L
expected_rows_7 <- 4L

test_that(desc = "Testing output structure for visual", code = {
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

  expect_equal(object = length(unique(actual_1$MS)), expected = expected_rows_1)
  expect_equal(object = length(unique(actual_2$MS)), expected = expected_rows_2)
  expect_equal(object = length(unique(actual_3$MS)), expected = expected_rows_3)
  expect_equal(object = length(unique(actual_4$MS)), expected = expected_rows_4)
  expect_equal(object = length(unique(actual_5$MS)), expected = expected_rows_5)
  expect_equal(object = length(unique(actual_6$MS)), expected = expected_rows_6)
  expect_equal(object = length(unique(actual_7$MS)), expected = expected_rows_7)

})

actual_1 <- MS_wise_imprecision(imprecision_data = test_data_1, mode = "exact")
actual_2 <- MS_wise_imprecision(imprecision_data = test_data_2, mode = "exact")
actual_3 <- MS_wise_imprecision(imprecision_data = test_data_3, mode = "exact")
actual_4 <- MS_wise_imprecision(imprecision_data = test_data_4, mode = "exact")
actual_5 <- MS_wise_imprecision(imprecision_data = test_data_5, mode = "exact")
actual_6 <- MS_wise_imprecision(imprecision_data = test_data_6, mode = "exact")
actual_7 <- MS_wise_imprecision(imprecision_data = test_data_7, mode = "exact")

test_that(desc = "Testing output structure for exact", code = {
  expect_true(object = is.data.table(actual_1))
  expect_true(object = is.data.table(actual_2))
  expect_true(object = is.data.table(actual_3))
  expect_true(object = is.data.table(actual_4))
  expect_true(object = is.data.table(actual_5))
  expect_true(object = is.data.table(actual_6))
  expect_true(object = is.data.table(actual_7))

  expect_true(object = all(lapply(X = actual_1[,-1], FUN = is.numeric) |> unlist()))
  expect_true(object = all(lapply(X = actual_2[,-1], FUN = is.numeric) |> unlist()))
  expect_true(object = all(lapply(X = actual_3[,-1], FUN = is.numeric) |> unlist()))
  expect_true(object = all(lapply(X = actual_4[,-1], FUN = is.numeric) |> unlist()))
  expect_true(object = all(lapply(X = actual_5[,-1], FUN = is.numeric) |> unlist()))
  expect_true(object = all(lapply(X = actual_6[,-1], FUN = is.numeric) |> unlist()))
  expect_true(object = all(lapply(X = actual_7[,-1], FUN = is.numeric) |> unlist()))

  expect_equal(object = length(unique(actual_1$MS)), expected = expected_rows_1)
  expect_equal(object = length(unique(actual_2$MS)), expected = expected_rows_2)
  expect_equal(object = length(unique(actual_3$MS)), expected = expected_rows_3)
  expect_equal(object = length(unique(actual_4$MS)), expected = expected_rows_4)
  expect_equal(object = length(unique(actual_5$MS)), expected = expected_rows_5)
  expect_equal(object = length(unique(actual_6$MS)), expected = expected_rows_6)
  expect_equal(object = length(unique(actual_7$MS)), expected = expected_rows_7)

})

actual_1 <- MS_wise_imprecision(test_data_1[, -c("CV_A","CV_A_lwr","CV_A_upr","CV_B", "CV_B_lwr", "CV_B_upr")], mode = "exact")
actual_2 <- MS_wise_imprecision(test_data_2[, -c("CV_A","CV_A_lwr","CV_A_upr","CV_B", "CV_B_lwr", "CV_B_upr")])
actual_3 <- MS_wise_imprecision(test_data_3[, -c("CV_A","CV_A_lwr","CV_A_upr","CV_B", "CV_B_lwr", "CV_B_upr")], mode = "exact")
actual_4 <- MS_wise_imprecision(test_data_4[, -c("CV_A","CV_A_lwr","CV_A_upr","CV_B", "CV_B_lwr", "CV_B_upr")])
actual_5 <- MS_wise_imprecision(test_data_5[, -c("Var_A","Var_A_lwr","Var_A_upr","Var_B", "Var_B_lwr", "Var_B_upr")], mode = "exact")
actual_6 <- MS_wise_imprecision(test_data_6[, -c("Var_A","Var_A_lwr","Var_A_upr","Var_B", "Var_B_lwr", "Var_B_upr")])
actual_7 <- MS_wise_imprecision(test_data_7[, -c("Var_A","Var_A_lwr","Var_A_upr","Var_B", "Var_B_lwr", "Var_B_upr")], mode = "exact")


test_that(desc = "Testing output structure for when only one imprecision estimate included", code = {
  expect_named(object = actual_1, expected = c("MS", "SD", "SD_lwr", "SD_upr"))
  expect_named(object = actual_2, expected = c("MS", "SD (lwr, upr)"))
  expect_named(object = actual_3, expected = c("MS", "SD", "SD_lwr", "SD_upr"))
  expect_named(object = actual_4, expected = c("MS", "SD (lwr, upr)"))
  expect_named(object = actual_5, expected = c("MS", "CV", "CV_lwr", "CV_upr"))
  expect_named(object = actual_6, expected = c("MS", "%CV (lwr, upr)"))
  expect_named(object = actual_7, expected = c("MS", "CV", "CV_lwr", "CV_upr"))

  expect_equal(object = length(unique(actual_1$MS)), expected = expected_rows_1)
  expect_equal(object = length(unique(actual_2$MS)), expected = expected_rows_2)
  expect_equal(object = length(unique(actual_3$MS)), expected = expected_rows_3)
  expect_equal(object = length(unique(actual_4$MS)), expected = expected_rows_4)
  expect_equal(object = length(unique(actual_5$MS)), expected = expected_rows_5)
  expect_equal(object = length(unique(actual_6$MS)), expected = expected_rows_6)
  expect_equal(object = length(unique(actual_7$MS)), expected = expected_rows_7)
})

detail_test_data <- NULL
draw <- sample.int(6,1,F)
if(draw==1){detail_test_data <- test_data_1}
if(draw==2){detail_test_data <- test_data_2}
if(draw==3){detail_test_data <- test_data_3}
if(draw==4){detail_test_data <- test_data_4}
if(draw==5){detail_test_data <- test_data_5}
if(draw==6){detail_test_data <- test_data_6}

actual_detailed_1 <- MS_wise_imprecision(imprecision_data = detail_test_data, mode = "visual")
actual_detailed_2 <- MS_wise_imprecision(imprecision_data = detail_test_data, mode = "exact")
actual_detailed_3 <- MS_wise_imprecision(imprecision_data = detail_test_data[,-c("CV_A","CV_A_lwr","CV_A_upr","CV_B", "CV_B_lwr", "CV_B_upr")], mode = "visual")
actual_detailed_4 <- MS_wise_imprecision(imprecision_data = detail_test_data[,-c("CV_A","CV_A_lwr","CV_A_upr","CV_B", "CV_B_lwr", "CV_B_upr")], mode = "exact")

test_that(desc = "Testing details on random test data", code = {
  expect_true(object = all(actual_detailed_1$MS %in% (stri_split(str = detail_test_data$comparison, fixed = " - ") |> unlist() |> unique())))
  expect_named(object = actual_detailed_1, expected = c("MS","%CV (lwr, upr)","SD (lwr, upr)"))
  expect_true(object = all(unique(100*c(detail_test_data$CV_A, detail_test_data$CV_B)) %in% actual_detailed_2$CV))
  expect_true(object = all(actual_detailed_4$SD_upr >= actual_detailed_4$SD_lwr | is.na(actual_detailed_4$SD_upr)))
})

