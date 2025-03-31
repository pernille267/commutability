library(testthat)
library(readxl)
library(data.table)
library(fasteqa)
library(smooth.commutability)

# Read test data
test_cs_data <- copy(crp_cs_data)

# Sprinkle NA-values
na_value_positions_MP_A <- sample(x = seq_len(nrow(test_cs_data)),
                                  size = 20,
                                  replace = FALSE)
na_value_positions_MP_B <- sample(x = seq_len(nrow(test_cs_data)),
                                  size = 20,
                                  replace = FALSE)

test_cs_data$MP_A[na_value_positions_MP_A] <- NA_real_
test_cs_data$MP_B[na_value_positions_MP_B] <- NA_real_

# Test if all transformations work
test_that(desc = "Check if all transformations work", code = {

  expect_no_condition(object = transform_data(test_cs_data,
                                              transformation = "log#e"))
  expect_no_condition(object = transform_data(test_cs_data,
                                              transformation = "log#10"))
  expect_no_condition(object = transform_data(test_cs_data,
                                              transformation = "ln"))
  expect_no_condition(object = transform_data(test_cs_data,
                                              transformation = "log"))
  expect_no_condition(object = transform_data(test_cs_data,
                                              transformation = "pow#2"))
  expect_no_condition(object = transform_data(test_cs_data,
                                              transformation = "pow#3"))
  expect_no_condition(object = transform_data(test_cs_data,
                                              transformation = "root#2"))
  expect_no_condition(object = transform_data(test_cs_data,
                                              transformation = "root#3"))
  expect_no_condition(object = transform_data(test_cs_data,
                                              transformation = "boxcox#0.75"))
  expect_no_condition(object = transform_data(test_cs_data,
                                              transformation = "boxcox#0.25"))
  expect_no_condition(object = transform_data(test_cs_data,
                                              transformation = "unit"))
  expect_no_condition(object = transform_data(test_cs_data,
                                              transformation = "bal"))
  expect_no_condition(object = transform_data(test_cs_data,
                                              transformation = "ba"))
  expect_no_condition(object = transform_data(test_cs_data,
                                              transformation = "identity"))




})

# Test if non-special transformations result in expected values
test_that(desc = "Check the non-special transformation values", code = {

  random_elements_to_be_checked <- sample(
    x = setdiff(x = seq_len(nrow(test_cs_data)),
                y = union(x = na_value_positions_MP_A,
                          y = na_value_positions_MP_B)),
    size = 5
  )

  expected_raw <- copy(test_cs_data)[random_elements_to_be_checked]


  # Check ln-transformation
  actual_1 <- transform_data(data = test_cs_data,
                             transformation = "log")[random_elements_to_be_checked]
  expected_1 <- expected_raw[, list(MP_A = log(MP_A),
                                    MP_B = log(MP_B)),
                             by = c("comparison", "SampleID", "ReplicateID")]

  expect_equal(object = actual_1$MP_A,
               expected = expected_1$MP_A)
  expect_equal(object = actual_1$MP_B,
               expected = expected_1$MP_B)

  # Check pow-transformation
  random_pow_parameter <- runif(n = 1, min = 1, max = 5)

  actual_2 <- transform_data(
    data = test_cs_data,
    transformation = paste0("pow#", random_pow_parameter, collapse = "")
  )[random_elements_to_be_checked]

  expected_2 <- expected_raw[, list(MP_A = MP_A ** random_pow_parameter,
                                    MP_B = MP_B ** random_pow_parameter),
                             by = c("comparison", "SampleID", "ReplicateID")]

  expect_equal(object = actual_2$MP_A,
               expected = expected_2$MP_A)
  expect_equal(object = actual_2$MP_B,
               expected = expected_2$MP_B)

  # Check root-transformations
  random_root_parameter <- sample(x = c(2, 3, 4),
                                  size = 1)

  actual_3 <- transform_data(
    data = test_cs_data,
    transformation = paste0("root#", random_root_parameter, collapse = "")
  )[random_elements_to_be_checked]

  expected_3 <- expected_raw[, list(MP_A = MP_A ** (1 / random_root_parameter),
                                    MP_B = MP_B ** (1 / random_root_parameter)),
                             by = c("comparison", "SampleID", "ReplicateID")]

  expect_equal(object = actual_3$MP_A,
               expected = expected_3$MP_A)
  expect_equal(object = actual_3$MP_B,
               expected = expected_3$MP_B)

  # Check boxcox-transformations
  random_boxcox_parameter <- runif(n = 1)

  actual_4 <- transform_data(
    data = test_cs_data,
    transformation = paste0("boxcox#", random_boxcox_parameter, collapse = "")
  )[random_elements_to_be_checked]

  expected_4 <- expected_raw[, list(MP_A = (MP_A ** random_boxcox_parameter - 1) / random_boxcox_parameter,
                                    MP_B = (MP_B ** random_boxcox_parameter - 1) / random_boxcox_parameter),
                             by = c("comparison", "SampleID", "ReplicateID")]

  expect_equal(object = actual_4$MP_A,
               expected = expected_4$MP_A)
  expect_equal(object = actual_4$MP_B,
               expected = expected_4$MP_B)




})

# Test if special transformations  result in expected values
test_that(desc = "Check the special transformation values", code = {

  # Get expected results
  guv <- function(x) {
     return((x - min(x, na.rm = TRUE)) / diff(range(x, na.rm = TRUE)))
  }

  expected_unit <- test_cs_data[, list(SampleID = SampleID,
                                       ReplicateID = ReplicateID,
                                       MP_A = guv(MP_A),
                                       MP_B = guv(MP_B)),
                                by = "comparison"]
  expected_ba <- test_cs_data[, list(SampleID = SampleID,
                                     ReplicateID = ReplicateID,
                                     y = MP_A - MP_B,
                                     x = (MP_A + MP_B) / 2),
                              by = "comparison"]
  names(expected_ba)[match(c("y", "x"), names(expected_ba))] <- c("MP_A", "MP_B")
  expected_bal <- test_cs_data[, list(SampleID = SampleID,
                                      ReplicateID = ReplicateID,
                                      y = log(MP_A) - log(MP_B),
                                      x = (MP_A + MP_B) / 2),
                               by = "comparison"]
  names(expected_bal)[match(c("y", "x"), names(expected_bal))] <- c("MP_A", "MP_B")

  # Get actual results
  actual_unit <- transform_data(test_cs_data, transformation = "unit")
  actual_ba <- transform_data(test_cs_data, transformation = "ba")
  actual_bal <- transform_data(test_cs_data, transformation = "bal")

  # Check if equal
  expect_equal(object = actual_unit,
               expected = expected_unit)
  expect_equal(object = actual_ba,
               expected = expected_ba)
  expect_equal(object = actual_bal,
               expected = expected_bal)




})
