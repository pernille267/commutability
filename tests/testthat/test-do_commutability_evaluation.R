library(testthat)
library(commutability)
library(readxl)
library(data.table)
library(fasteqa)

test_data_1 <- read_excel(path = "~/datasets to be tested on/test_data_2.xlsx")
test_data_2 <- read_excel(path = "~/datasets to be tested on/test_data_7.xlsx")

check_data_1 <- check_data(test_data_1)
check_data_2 <- check_data(test_data_2)

test_data_1 <- repair_data(data = test_data_1, check_data_1)
test_data_2 <- repair_data(data = test_data_2, check_data_2)
test_eqam_1 <- test_data_1[SampleID %in% c("23", "6"), ] |> MS_wise()
test_eqam_2 <- test_data_2[SampleID %in% c("29", "15"), ] |> MS_wise()
test_data_1 <- test_data_1[!SampleID %in% c("23", "6"), ] |> MS_wise()
test_data_2 <- test_data_2[!SampleID %in% c("29", "15"), ] |> MS_wise()

imprecision_data_1 <- estimate_imprecision_data(data = test_data_1, B = 2e2)[,-c("Var_A","Var_A_lwr","Var_A_upr", "Var_B","Var_B_lwr","Var_B_upr")]
imprecision_data_2 <- estimate_imprecision_data(data = test_data_2, B = 2e2)[,-c("Var_A","Var_A_lwr","Var_A_upr", "Var_B","Var_B_lwr","Var_B_upr")]

zeta_data_1 <- estimate_zeta_data(data = test_data_1, B = 2e2, zeta_critical = 2.16)
zeta_data_2 <- estimate_zeta_data(data = test_data_2 |> na.omit(), B = 2e2, zeta_critical = 2.01)

prediction_ce_data_1 <- estimate_prediction_data(data = test_data_1, new_data = test_eqam_1, B = 2e3)
prediction_ce_data_2 <- estimate_prediction_data(data = test_data_2 |> na.omit(), new_data = test_eqam_2, B = 2e2)

prediction_pb_data_1 <- estimate_prediction_data(data = test_data_1, new_data = "gen_250")
prediction_pb_data_2 <- estimate_prediction_data(data = test_data_2 |> na.omit(), new_data = "gen_250")

dce_1 <- do_commutability_evaluation(data = test_data_1, new_data = test_eqam_1, upper_zeta = 2.16)
dce_2 <- do_commutability_evaluation(data = test_data_2 |> na.omit(), new_data = test_eqam_2, upper_zeta = 2.01)

expected_1 <- merge(zeta_data_1, prediction_ce_data_1, by = "comparison")
old_names <- c("lwr.x", "upr.x","zeta_critical","zeta_conclusion","MP_B","MP_A","lwr.y","upr.y", "inside")
new_names <- c("zeta_ci_lwr", "zeta_ci_upr","zeta_upper","dins_conclusion","MS_B","MS_A","pi_lwr","pi_upr","pi_inside")
setnames(x = expected_1, old = old_names, new = new_names)
actual_1 <- dce_1$merged_ce_data
expected_1 <- lapply(X = expected_1, FUN = function(x) if(!any(is.integer(x), is.character(x))){round(x,3L)}else{x}) |> setDT() |> setcolorder(neworder = names(actual_1))

expected_2 <- merge(zeta_data_2, prediction_ce_data_2, by = "comparison")
old_names <- c("lwr.x", "upr.x","zeta_critical","zeta_conclusion","MP_B","MP_A","lwr.y","upr.y","inside")
new_names <- c("zeta_ci_lwr", "zeta_ci_upr","zeta_upper","dins_conclusion","MS_B","MS_A","pi_lwr","pi_upr","pi_inside")
setnames(x = expected_2, old = old_names, new = new_names)
actual_2 <- dce_2$merged_ce_data
expected_2 <- lapply(X = expected_2, FUN = function(x) if(!any(is.integer(x), is.character(x))){round(x,3L)}else{x}) |> setDT() |> setcolorder(neworder = names(actual_2))

expected_3 <- merge(zeta_data_1, prediction_pb_data_1, by = "comparison")
old_names <- c("lwr.x", "upr.x","zeta_critical","zeta_conclusion","predictor","prediction","lwr.y","upr.y")
new_names <- c("zeta_ci_lwr", "zeta_ci_upr","zeta_upper","dins_conclusion","predictor","prediction","pi_lwr","pi_upr")
setnames(x = expected_3, old = old_names, new = new_names)
actual_3 <- dce_1$merged_pb_data
expected_3 <- lapply(X = expected_3, FUN = function(x) if(!any(is.integer(x), is.character(x))){round(x,3L)}else{x}) |> setDT() |> setcolorder(neworder = names(actual_3))

expected_4 <- merge(zeta_data_2, prediction_pb_data_2, by = "comparison")
old_names <- c("lwr.x", "upr.x","zeta_critical","zeta_conclusion","predictor","prediction","lwr.y","upr.y")
new_names <- c("zeta_ci_lwr", "zeta_ci_upr","zeta_upper","dins_conclusion","predictor","prediction","pi_lwr","pi_upr")
setnames(x = expected_4, old = old_names, new = new_names)
actual_4 <- dce_2$merged_pb_data
expected_4 <- lapply(X = expected_4, FUN = function(x) if(!any(is.integer(x), is.character(x))){round(x,3L)}else{x}) |> setDT() |> setcolorder(neworder = names(actual_4))

test_that(desc = "Testing if results are correct based based on the output of merge.data.table()", code = {
  expect_true(sum(mapply(FUN = function(x, y) all(if(is.numeric(x) & !is.integer(x)){abs(x/y) >= 0.90 | abs(x/y) <= 1.10}else{x==y}), actual_1, expected_1), na.rm = TRUE) >= 13)
  expect_true(sum(mapply(FUN = function(x, y) all(if(is.numeric(x) & !is.integer(x)){abs(x/y) >= 0.90 | abs(x/y) <= 1.10}else{x==y}), actual_2, expected_2), na.rm = TRUE) >= 13)
  expect_true(sum(mapply(FUN = function(x, y) all(if(is.numeric(x) & !is.integer(x)){abs(x/y) >= 0.90 | abs(x/y) <= 1.10}else{x==y}), actual_3, expected_3), na.rm = TRUE) >= 8)
  expect_true(sum(mapply(FUN = function(x, y) all(if(is.numeric(x) & !is.integer(x)){abs(x/y) >= 0.90 | abs(x/y) <= 1.10}else{x==y}), actual_4, expected_4), na.rm = TRUE) >= 8)
})

dce_3 <- do_commutability_evaluation(data = test_data_1, new_data = test_eqam_1, upper_zeta = 2.16, output = "complete")
dce_4 <- do_commutability_evaluation(data = test_data_2 |> na.omit(), new_data = test_eqam_2, upper_zeta = 2.01, output = "complete")

expected_names_1 <- c("comparison", "SampleID", "zeta", "zeta_ci_lwr", "zeta_ci_upr", "zeta_upper", "dins_conclusion", "MS_B", "MS_A", "prediction", "pi_lwr", "pi_upr", "pi_inside", "CV_A", "CV_A_lwr", "CV_A_upr", "CV_B", "CV_B_lwr", "CV_B_upr", "lambda", "inside_rate")
expected_names_2 <- c("comparison", "SampleID", "zeta", "zeta_ci_lwr", "zeta_ci_upr", "zeta_upper", "dins_conclusion", "MS_B", "MS_A", "prediction", "pi_lwr", "pi_upr", "pi_inside", "CV_A", "CV_A_lwr", "CV_A_upr", "CV_B", "CV_B_lwr", "CV_B_upr", "lambda", "inside_rate")
expected_names_3 <- c("comparison", "zeta", "zeta_ci_lwr", "zeta_ci_upr", "zeta_upper", "dins_conclusion", "predictor", "prediction", "pi_lwr", "pi_upr", "CV_A", "CV_A_lwr", "CV_A_upr", "CV_B", "CV_B_lwr", "CV_B_upr", "lambda", "lambda_lwr", "lambda_upr")
expected_names_4 <- c("comparison", "zeta", "zeta_ci_lwr", "zeta_ci_upr", "zeta_upper", "dins_conclusion", "predictor", "prediction", "pi_lwr", "pi_upr", "CV_A", "CV_A_lwr", "CV_A_upr", "CV_B", "CV_B_lwr", "CV_B_upr", "lambda", "lambda_lwr", "lambda_upr")

test_that(desc = "Testing output names when output = 'complete' ", code = {
  expect_named(object = dce_3$merged_ce_data, expected = expected_names_1)
  expect_named(object = dce_4$merged_ce_data, expected = expected_names_2)
  expect_named(object = dce_3$merged_pb_data, expected = expected_names_3)
  expect_named(object = dce_4$merged_pb_data, expected = expected_names_4)
})

expected_ce_1 <- merge.data.table(x = zeta_data_1, y = prediction_ce_data_1, by = "comparison") |>
  merge.data.table(imprecision_data_1, by = "comparison")
setnames(expected_ce_1, old = c("zeta_critical", "zeta_conclusion", "lwr.x", "upr.x", "lwr.y", "upr.y", "inside","MP_B", "MP_A"), new = c("zeta_upper", "dins_conclusion", "zeta_ci_lwr", "zeta_ci_upr", "pi_lwr", "pi_upr", "pi_inside","MS_B", "MS_A"))
expected_ce_1 <- expected_ce_1[,-c("lambda_lwr", "lambda_upr")]
setcolorder(expected_ce_1, neworder = c("comparison", "SampleID", "zeta", "zeta_ci_lwr", "zeta_ci_upr", "zeta_upper", "dins_conclusion", "MS_B", "MS_A", "prediction", "pi_lwr", "pi_upr", "pi_inside", "CV_A", "CV_A_lwr", "CV_A_upr", "CV_B", "CV_B_lwr", "CV_B_upr", "lambda", "inside_rate"))
expected_ce_1 <- lapply(X = expected_ce_1, FUN = function(x) if(!any(is.integer(x), is.character(x))){round(x,3L)}else{x}) |> setDT()
actual_ce_1 <- dce_3$merged_ce_data

expected_ce_2 <- merge.data.table(x = zeta_data_2, y = prediction_ce_data_2, by = "comparison") |>
  merge.data.table(imprecision_data_2, by = "comparison")
setnames(expected_ce_2, old = c("zeta_critical", "zeta_conclusion", "lwr.x", "upr.x", "lwr.y", "upr.y", "inside","MP_B", "MP_A"), new = c("zeta_upper", "dins_conclusion", "zeta_ci_lwr", "zeta_ci_upr", "pi_lwr", "pi_upr", "pi_inside","MS_B", "MS_A"))
expected_ce_2 <- expected_ce_2[,-c("lambda_lwr", "lambda_upr")]
setcolorder(expected_ce_2, neworder = c("comparison", "SampleID", "zeta", "zeta_ci_lwr", "zeta_ci_upr", "zeta_upper", "dins_conclusion", "MS_B", "MS_A", "prediction", "pi_lwr", "pi_upr", "pi_inside", "CV_A", "CV_A_lwr", "CV_A_upr", "CV_B", "CV_B_lwr", "CV_B_upr", "lambda", "inside_rate"))
expected_ce_2 <- lapply(X = expected_ce_2, FUN = function(x) if(!any(is.integer(x), is.character(x))){round(x,3L)}else{x}) |> setDT()
actual_ce_2 <- dce_4$merged_ce_data

test_that(desc = "Testing correctness of output data when output = 'complete'", code = {
  expect_true(sum(mapply(FUN = function(x, y) all(if(is.numeric(x) & !is.integer(x)){abs(x/y) >= 0.90 | abs(x/y) <= 1.10}else{x==y}), actual_ce_1, expected_ce_1), na.rm = TRUE) >= 18)
  expect_true(sum(mapply(FUN = function(x, y) all(if(is.numeric(x) & !is.integer(x)){abs(x/y) >= 0.90 | abs(x/y) <= 1.10}else{x==y}), actual_ce_2, expected_ce_2), na.rm = TRUE) >= 18)
})
