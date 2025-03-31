library(testthat)
library(readxl)
library(data.table)
library(fasteqa)
library(smooth.commutability)

# Functions required for this testing file
get_expected_ce <- function(zeta_data, ce_data, imprecision_data = NULL) {

  temp_merged_data <- merge.data.table(x = zeta_data,
                                       y = ce_data,
                                       by = "comparison")

  setnames(x = temp_merged_data,
           old = c("lwr.x",
                   "upr.x",
                   "zeta_critical",
                   "zeta_conclusion",
                   "lwr.y",
                   "upr.y",
                   "inside"),
           new = c("zeta_ci_lwr",
                   "zeta_ci_upr",
                   "zeta_upper",
                   "dins_conclusion",
                   "pi_lwr",
                   "pi_upr",
                   "pi_inside"),
           skip_absent = TRUE)

  setcolorder(x = temp_merged_data,
              neworder = c("comparison",
                           "SampleID",
                           "zeta",
                           "zeta_ci_lwr",
                           "zeta_ci_upr",
                           "zeta_upper",
                           "dins_conclusion",
                           "MP_B",
                           "MP_A",
                           "prediction",
                           "pi_lwr",
                           "pi_upr",
                           "pi_inside",
                           "extrapolate",
                           "inside_rate"))

  if (!is.null(imprecision_data)) {

    temp_merged_data <- merge.data.table(x = temp_merged_data,
                                         y = imprecision_data,
                                         by = "comparison")

  }


  temp_merged_data <- temp_merged_data[, lapply(X = .SD,
                                                FUN = function(variable) {

                                                  if (is.integer(variable)) {
                                                    variable
                                                  }
                                                  else if (is.numeric(variable)) {
                                                    round(variable, 3L)
                                                  }
                                                  else {
                                                    variable
                                                  }
                                                })]

  return(temp_merged_data)

}

get_expected_pb <- function(zeta_data, pb_data, imprecision_data = NULL) {

  temp_merged_data <- merge.data.table(x = zeta_data,
                                       y = pb_data,
                                       by = "comparison")

  setnames(x = temp_merged_data,
           old = c("lwr.x",
                   "upr.x",
                   "zeta_critical",
                   "zeta_conclusion",
                   "lwr.y",
                   "upr.y"),
           new = c("zeta_ci_lwr",
                   "zeta_ci_upr",
                   "zeta_upper",
                   "dins_conclusion",
                   "pi_lwr",
                   "pi_upr"),
           skip_absent = TRUE)

  setcolorder(x = temp_merged_data,
              neworder = c("comparison",
                           "zeta",
                           "zeta_ci_lwr",
                           "zeta_ci_upr",
                           "zeta_upper",
                           "dins_conclusion",
                           "predictor",
                           "prediction",
                           "pi_lwr",
                           "pi_upr"))

  if (!is.null(imprecision_data)) {

    temp_merged_data <- merge.data.table(x = temp_merged_data,
                                         y = imprecision_data,
                                         by = "comparison")

  }


  temp_merged_data <- temp_merged_data[, lapply(X = .SD,
                                                FUN = function(variable) {

                                                  if (is.integer(variable)) {
                                                    variable
                                                  }
                                                  else if (is.numeric(variable)) {
                                                    round(variable, 3L)
                                                  }
                                                  else {
                                                    variable
                                                  }
                                                })]

  return(temp_merged_data)

}

# Read data to be tested
test_cs_data <- copy(crp_cs_data)
test_eq_data <- copy(crp_eqam_data)

# Exclude variance components
var_comps <- c("Var_A",
               "Var_A_lwr",
               "Var_A_upr",
               "Var_B",
               "Var_B_lwr",
               "Var_B_upr")

# Calculate imprecision data
impr_data_1 <- estimate_imprecision_data(data = test_cs_data,
                                         B = 2e2L,
                                         type = "BCa",
                                         level = 0.95,
                                         invalid_NA = FALSE)
impr_data_1 <- impr_data_1[, -var_comps, with = FALSE]

impr_data_2 <- estimate_imprecision_data(data = test_cs_data,
                                         B = 2e2L,
                                         type = "BCa",
                                         level = 0.95,
                                         invalid_NA = FALSE)
impr_data_2 <- impr_data_2[, -var_comps, with = FALSE]

# Calculate zeta data
zeta_data_1 <- estimate_zeta_data(data = test_cs_data,
                                  B = 2e2L,
                                  type = "BCa",
                                  zeta_critical = 3.32)
zeta_data_2 <- estimate_zeta_data(data = test_cs_data,
                                  B = 2e2L,
                                  type = "BCa",
                                  method = "ssw",
                                  zeta_critical = 2.98)

# Calculate Commutability Evaluation (CE) data
prediction_ce_data_1 <- estimate_prediction_data(data = test_cs_data,
                                                 new_data = test_eq_data,
                                                 method = "fg",
                                                 B = 2e2L)
prediction_ce_data_2 <- estimate_prediction_data(data = test_cs_data,
                                                 new_data = test_eq_data,
                                                 method = "ssw",
                                                 B = 2e2L)

# Calculate Prediction Band (PB) data
prediction_pb_data_1 <- estimate_prediction_data(data = test_cs_data,
                                                 new_data = "gen_250",
                                                 method = "fg")

prediction_pb_data_2 <- estimate_prediction_data(data = test_cs_data,
                                                 new_data = "gen_250",
                                                 method = "ssw")

# Outputs
dce_1 <- do_commutability_evaluation(data = test_cs_data,
                                     new_data = test_eq_data,
                                     upper_zeta = 3.32,
                                     B = 2e2,
                                     N = 2e2,
                                     method_pi = "fg",
                                     method_bs = "BCa",
                                     level_pi = 0.99,
                                     level_bs = 0.95,
                                     M = 0,
                                     output = "sufficient",
                                     avoid_simulations = TRUE)

dce_2 <- do_commutability_evaluation(data = test_cs_data,
                                     new_data = test_eq_data,
                                     upper_zeta = 2.98,
                                     B = 2e2,
                                     N = 2e2,
                                     method_pi = "ssw",
                                     method_bs = "BCa",
                                     level_pi = 0.99,
                                     level_bs = 0.95,
                                     M = 0,
                                     output = "sufficient",
                                     avoid_simulations = TRUE)

# Check if do_commutability_evaluation()$merged_ce_data produce expected results (sufficient)
test_that(desc = "Check if CE output is close enough to expected", code = {

  expected_names <- c("comparison", "SampleID", "zeta", "zeta_ci_lwr",
                      "zeta_ci_upr", "zeta_upper", "dins_conclusion",
                      "MP_B", "MP_A", "prediction", "pi_lwr", "pi_upr",
                      "pi_inside", "extrapolate", "inside_rate")

  # Testing for fg
  expected_ce_1 <- get_expected_ce(zeta_data = zeta_data_1,
                                   ce_data = prediction_ce_data_1,
                                   imprecision_data = NULL)

  # Check names
  expect_named(object = dce_1$merged_ce_data,
               expected = expected_names,
               ignore.order = FALSE,
               ignore.case = FALSE)

  # Check non-random values
  expect_equal(object = dce_1$merged_ce_data$comparison,
               expected = expected_ce_1$comparison)
  expect_equal(object = dce_1$merged_ce_data$SampleID,
               expected = expected_ce_1$SampleID)
  expect_equal(object = dce_1$merged_ce_data$zeta_upper,
               expected = expected_ce_1$zeta_upper)
  expect_equal(object = dce_1$merged_ce_data$dins_conclusion,
               expected = expected_ce_1$dins_conclusion)
  expect_equal(object = dce_1$merged_ce_data$MP_B,
               expected = expected_ce_1$MP_B)
  expect_equal(object = dce_1$merged_ce_data$MP_A,
               expected = expected_ce_1$MP_A)
  expect_equal(object = dce_1$merged_ce_data$prediction,
               expected = expected_ce_1$prediction)
  expect_equal(object = dce_1$merged_ce_data$pi_lwr,
               expected = expected_ce_1$pi_lwr)
  expect_equal(object = dce_1$merged_ce_data$pi_upr,
               expected = expected_ce_1$pi_upr)
  expect_equal(object = dce_1$merged_ce_data$pi_inside,
               expected = expected_ce_1$pi_inside)
  expect_equal(object = dce_1$merged_ce_data$extrapolate,
               expected = expected_ce_1$extrapolate)


  # Testing for ssw
  expected_ce_2 <- get_expected_ce(zeta_data = zeta_data_2,
                                   ce_data = prediction_ce_data_2,
                                   imprecision_data = NULL)

  # Check names
  expect_named(object = dce_2$merged_ce_data,
               expected = expected_names,
               ignore.order = FALSE,
               ignore.case = FALSE)

  # Check non-random values
  expect_equal(object = dce_2$merged_ce_data$comparison,
               expected = expected_ce_2$comparison)
  expect_equal(object = dce_2$merged_ce_data$SampleID,
               expected = expected_ce_2$SampleID)
  expect_equal(object = dce_2$merged_ce_data$zeta_upper,
               expected = expected_ce_2$zeta_upper)
  expect_equal(object = dce_2$merged_ce_data$dins_conclusion,
               expected = expected_ce_2$dins_conclusion)
  expect_equal(object = dce_2$merged_ce_data$MP_B,
               expected = expected_ce_2$MP_B)
  expect_equal(object = dce_2$merged_ce_data$MP_A,
               expected = expected_ce_2$MP_A)
  expect_equal(object = dce_2$merged_ce_data$prediction,
               expected = expected_ce_2$prediction)
  expect_equal(object = dce_2$merged_ce_data$pi_lwr,
               expected = expected_ce_2$pi_lwr)
  expect_equal(object = dce_2$merged_ce_data$pi_upr,
               expected = expected_ce_2$pi_upr)
  expect_equal(object = dce_2$merged_ce_data$pi_inside,
               expected = expected_ce_2$pi_inside)
  expect_equal(object = dce_2$merged_ce_data$extrapolate,
               expected = expected_ce_2$extrapolate)

})

# Check if do_commutability_evaluation()$merged_pb_data produce expected results (sufficient)
test_that(desc = "Check if PB output is close enough to expected", code = {

  expected_names <- c("comparison", "zeta", "zeta_ci_lwr",
                      "zeta_ci_upr", "zeta_upper", "dins_conclusion",
                      "predictor", "prediction", "pi_lwr", "pi_upr")

  # Testing for fg
  expected_pb_1 <- get_expected_pb(zeta_data = zeta_data_1,
                                   pb_data = prediction_pb_data_1,
                                   imprecision_data = NULL)

  # Check names
  expect_named(object = dce_1$merged_pb_data,
               expected = expected_names,
               ignore.order = FALSE,
               ignore.case = FALSE)

  # Check non-random values
  expect_equal(object = dce_1$merged_pb_data$comparison,
               expected = expected_pb_1$comparison)
  expect_equal(object = dce_1$merged_pb_data$zeta_upper,
               expected = expected_pb_1$zeta_upper)
  expect_equal(object = dce_1$merged_pb_data$dins_conclusion,
               expected = expected_pb_1$dins_conclusion)
  expect_equal(object = dce_1$merged_pb_data$predictor,
               expected = expected_pb_1$predictor)
  expect_equal(object = dce_1$merged_pb_data$prediction,
               expected = expected_pb_1$prediction)
  expect_equal(object = dce_1$merged_pb_data$pi_lwr,
               expected = expected_pb_1$pi_lwr)
  expect_equal(object = dce_1$merged_pb_data$pi_upr,
               expected = expected_pb_1$pi_upr)

  # Testing for fg
  expected_pb_2 <- get_expected_pb(zeta_data = zeta_data_2,
                                   pb_data = prediction_pb_data_2,
                                   imprecision_data = NULL)

  # Check names
  expect_named(object = dce_2$merged_pb_data,
               expected = expected_names,
               ignore.order = FALSE,
               ignore.case = FALSE)

  # Check non-random values
  expect_equal(object = dce_2$merged_pb_data$comparison,
               expected = expected_pb_2$comparison)
  expect_equal(object = dce_2$merged_pb_data$zeta_upper,
               expected = expected_pb_2$zeta_upper)
  expect_equal(object = dce_2$merged_pb_data$dins_conclusion,
               expected = expected_pb_2$dins_conclusion)
  expect_equal(object = dce_2$merged_pb_data$predictor,
               expected = expected_pb_2$predictor)
  expect_equal(object = dce_2$merged_pb_data$prediction,
               expected = expected_pb_2$prediction,
               tolerance = 0.1)
  expect_equal(object = dce_2$merged_pb_data$pi_lwr,
               expected = expected_pb_2$pi_lwr,
               tolerance = 0.1)
  expect_equal(object = dce_2$merged_pb_data$pi_upr,
               expected = expected_pb_2$pi_upr,
               tolerance = 0.1)


})

