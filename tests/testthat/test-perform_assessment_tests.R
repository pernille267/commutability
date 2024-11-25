library(testthat)
library(commutability)
library(readxl)
suppressWarnings(library(data.table))
library(fasteqa)

test_data_1 <- read_excel(path = "~/Packages/datasets to be tested on/test_data_1.xlsx")
test_data_2 <- read_excel(path = "~/Packages/datasets to be tested on/test_data_2.xlsx")
test_data_3 <- read_excel(path = "~/Packages/datasets to be tested on/test_data_3.xlsx")
test_data_4 <- read_excel(path = "~/Packages/datasets to be tested on/test_data_4.xlsx")
test_data_5 <- read_excel(path = "~/Packages/datasets to be tested on/test_data_5.xlsx")
test_data_6 <- read_excel(path = "~/Packages/datasets to be tested on/test_data_6.xlsx")
test_data_7 <- read_excel(path = "~/Packages/datasets to be tested on/test_data_7.xlsx")

check_data_1 <- check_data(data = test_data_1, type = "cs")
check_data_2 <- check_data(data = test_data_2, type = "cs")
check_data_3 <- check_data(data = test_data_3, type = "cs")
check_data_4 <- check_data(data = test_data_4, type = "cs")
check_data_5 <- check_data(data = test_data_5, type = "cs")
check_data_6 <- check_data(data = test_data_6, type = "cs")
check_data_7 <- check_data(data = test_data_7, type = "cs")

test_data_repaired_1 <- repair_data(data = test_data_1, data_check = check_data_1) |> MS_wise()
test_data_repaired_2 <- repair_data(data = test_data_2, data_check = check_data_2) |> MS_wise()
test_data_repaired_3 <- repair_data(data = test_data_3, data_check = check_data_3) |> MS_wise()
test_data_repaired_4 <- repair_data(data = test_data_4, data_check = check_data_4) |> MS_wise()
test_data_repaired_5 <- repair_data(data = test_data_5, data_check = check_data_5) |> MS_wise()
test_data_repaired_6 <- repair_data(data = test_data_6, data_check = check_data_6) |> MS_wise()
test_data_repaired_7 <- repair_data(data = test_data_7, data_check = check_data_7) |> MS_wise()


actual_1_1A <- perform_assessment_tests(data = test_data_repaired_1, B = 0, method = "fg", level = 0.95, include_rejection_rates = FALSE, simultaneous_testing = TRUE, silence = 1L)
actual_1_1B <- perform_assessment_tests(data = test_data_repaired_1, B = 0, method = "clsi", level = 0.95, include_rejection_rates = FALSE, simultaneous_testing = TRUE, silence = 1L)
actual_1_1C <- perform_assessment_tests(data = test_data_repaired_1, B = 0, method = "ols", level = 0.95, include_rejection_rates = FALSE, simultaneous_testing = TRUE, silence = 1L)

actual_1_2A <- perform_assessment_tests(data = test_data_repaired_2, B = 0, method = "fg", level = 0.95, include_rejection_rates = FALSE, simultaneous_testing = TRUE, silence = 1L)
actual_1_2B <- perform_assessment_tests(data = test_data_repaired_2, B = 0, method = "clsi", level = 0.95, include_rejection_rates = FALSE, simultaneous_testing = TRUE, silence = 1L)
actual_1_2C <- perform_assessment_tests(data = test_data_repaired_2, B = 0, method = "ols", level = 0.95, include_rejection_rates = FALSE, simultaneous_testing = TRUE, silence = 1L)

# Testing output data.table structure and contents assuming that 'include_rejection_rates' is set to 'FALSE'
expected_1_names <- c("comparison", "test_name", "test", "p.value", "conclusion")
expected_1_classes <- rep("character", length(expected_1_names))
expected_1_comparison <- rep(unique(test_data_repaired_1$comparison), 2L)
expected_1_test_name <- rep(c("Shapiro-Wilk", "Breusch-Pagan"), each = length(unique(test_data_repaired_1$comparison)))
expected_1_test <- rep(c("normality", "variance homogeneity"), each = length(unique(test_data_repaired_1$comparison)))

test_that(desc = "Testing output names (1-6), column classes (7-12) and predictable column contents (13-30)", code = {
  expect_named(object = actual_1_1A, expected = expected_1_names, ignore.order = FALSE, ignore.case = FALSE)
  expect_named(object = actual_1_1B, expected = expected_1_names, ignore.order = FALSE, ignore.case = FALSE)
  expect_named(object = actual_1_1C, expected = expected_1_names, ignore.order = FALSE, ignore.case = FALSE)
  expect_named(object = actual_1_2A, expected = expected_1_names, ignore.order = FALSE, ignore.case = FALSE)
  expect_named(object = actual_1_2B, expected = expected_1_names, ignore.order = FALSE, ignore.case = FALSE)
  expect_named(object = actual_1_2C, expected = expected_1_names, ignore.order = FALSE, ignore.case = FALSE)

  expect_equal(object = unlist(lapply(X = actual_1_1A, class), use.names = FALSE), expected = expected_1_classes)
  expect_equal(object = unlist(lapply(X = actual_1_1B, class), use.names = FALSE), expected = expected_1_classes)
  expect_equal(object = unlist(lapply(X = actual_1_1C, class), use.names = FALSE), expected = expected_1_classes)
  expect_equal(object = unlist(lapply(X = actual_1_2A, class), use.names = FALSE), expected = expected_1_classes)
  expect_equal(object = unlist(lapply(X = actual_1_2B, class), use.names = FALSE), expected = expected_1_classes)
  expect_equal(object = unlist(lapply(X = actual_1_2C, class), use.names = FALSE), expected = expected_1_classes)

  expect_equal(object = actual_1_1A$comparison, expected = expected_1_comparison)
  expect_equal(object = actual_1_1B$comparison, expected = expected_1_comparison)
  expect_equal(object = actual_1_1C$comparison, expected = expected_1_comparison)
  expect_equal(object = actual_1_2A$comparison, expected = expected_1_comparison)
  expect_equal(object = actual_1_2B$comparison, expected = expected_1_comparison)
  expect_equal(object = actual_1_2C$comparison, expected = expected_1_comparison)

  expect_equal(object = actual_1_1A$test_name, expected = expected_1_test_name)
  expect_equal(object = actual_1_1B$test_name, expected = expected_1_test_name)
  expect_equal(object = actual_1_1C$test_name, expected = expected_1_test_name)
  expect_equal(object = actual_1_2A$test_name, expected = expected_1_test_name)
  expect_equal(object = actual_1_2B$test_name, expected = expected_1_test_name)
  expect_equal(object = actual_1_2C$test_name, expected = expected_1_test_name)

  expect_equal(object = actual_1_1A$test, expected = expected_1_test)
  expect_equal(object = actual_1_1B$test, expected = expected_1_test)
  expect_equal(object = actual_1_1C$test, expected = expected_1_test)
  expect_equal(object = actual_1_2A$test, expected = expected_1_test)
  expect_equal(object = actual_1_2B$test, expected = expected_1_test)
  expect_equal(object = actual_1_2C$test, expected = expected_1_test)
})

# Testing if registered p-values correspond with the test conclusions and if they are between 0 and 1
actual_1_2A_p.values <- ifelse(actual_1_2A$p.value == "< 0.001", "0", actual_1_2A$p.value)
actual_1_2A_p.values <- as.numeric(actual_1_2A_p.values)
actual_1_2A_conclusions <- actual_1_2A$conclusion
expected_conclusions_normality <- ifelse(actual_1_2A_p.values[which(actual_1_2A$test == "normality")] + 5e-4 < 0.05 / length(unique(test_data_repaired_2$comparison)),
                                         "normality rejected",
                                         "normality not rejected")
expected_conclusions_homoscedasticity <- ifelse(actual_1_2A_p.values[which(actual_1_2A$test == "variance homogeneity")] + 5e-4 < 0.05 / length(unique(test_data_repaired_2$comparison)),
                                         "variance homogeneity rejected",
                                         "variance homogeneity not rejected")
expected_conclusions <- c(expected_conclusions_normality, expected_conclusions_homoscedasticity)

test_that(desc = "Testing if conclusions correspond with the p-values (1) and if p-values are between 0 and 1 (2-3)", code = {
  expect_equal(object = actual_1_2A_conclusions, expected = expected_conclusions)
  sapply(actual_1_2A_p.values, expect_gte, expected = 0)
  sapply(actual_1_2A_p.values, expect_lte, expected = 1)
})

actual_2_3 <- perform_assessment_tests(data = test_data_repaired_3, B = 20L, method = sample(c("fg", "clsi", "ols"), 1L), level = 0.90, include_rejection_rates = TRUE, simultaneous_testing = FALSE, silence = 1L)
actual_2_4 <- perform_assessment_tests(data = test_data_repaired_4, B = 50L, method = sample(c("fg", "clsi", "ols"), 1L), level = 0.90, include_rejection_rates = TRUE, simultaneous_testing = FALSE, silence = 1L)

# Testing output names, classes and 'rejection_rate' column values given that 'include_rejection_rates' is set to TRUE with valid integer values of B: B = 20L and B = 50L
expected_2_names <- c("comparison", "test_name", "test", "p.value", "conclusion", "rejection_rate")
expected_2_classes <- c(rep("character", 5L), "numeric")
actual_2_3_valid_rejection_rates <- actual_2_3$rejection_rate >= 0 & actual_2_3$rejection_rate <= 1
actual_2_4_valid_rejection_rates <- actual_2_4$rejection_rate >= 0 & actual_2_4$rejection_rate <= 1

test_that(desc = "Testing output names (1-2), classes (3-4) and 'rejection_rate' column values is between 0 and 1", code = {
  expect_named(object = actual_2_3, expected = expected_2_names, ignore.order = FALSE, ignore.case = FALSE)
  expect_named(object = actual_2_3, expected = expected_2_names, ignore.order = FALSE, ignore.case = FALSE)

  expect_equal(object = unlist(lapply(X = actual_2_3, class), use.names = FALSE), expected = expected_2_classes)
  expect_equal(object = unlist(lapply(X = actual_2_4, class), use.names = FALSE), expected = expected_2_classes)

  sapply(actual_2_3_valid_rejection_rates, function(x) expect_true(isTRUE(x)))
  sapply(actual_2_4_valid_rejection_rates, function(x) expect_true(isTRUE(x)))
})

# Testing error messages for various invalid inputs
invalid_data_1 <- "character_string_data"
invalid_data_2 <- test_data_repaired_5[, c("SampleID", "ReplicateID", "MP_A", "MP_B")]
invalid_data_3 <- test_data_repaired_5[, c("SampleID", "ReplicateID", "MP_B")]
invalid_data_4 <- test_data_1
invalid_data_5 <- NA

invalid_level_1 <- "character_string_level"
invalid_level_2 <- 95
invalid_level_3 <- TRUE
invalid_level_4 <- c(NA, 0.90)
invalid_level_5 <- NULL

invalid_B_1 <- NULL
invalid_B_2 <- "100"
invalid_B_3 <- TRUE
invalid_B_4 <- NaN
invalid_B_5 <- Inf
invalid_B_6 <- -Inf
invalid_B_7 <- -1
invalid_B_8 <- c(-43.213, 900L)
invalid_B_9 <- -1e-16

invalid_include_rejection_rates <- list(NA, "character_string", 1, 0, 3.14, -Inf)
random_invalid_include_rejection_rates <- invalid_include_rejection_rates[[sample.int(6, 1)]]

invalid_simultaneous_testing <- list(NA, "character_string", 1, 0, 3.14, -Inf)
random_invalid_simultaneous_testing <- invalid_simultaneous_testing[[sample.int(6, 1)]]

test_that(desc = "Testing error messages for invalid 'data' input", code = {
  expect_error(object = perform_assessment_tests(data = invalid_data_1), regexp = "Registered input class of 'data': 'character'.")
  expect_error(object = perform_assessment_tests(data = invalid_data_2), regexp = "Some required columns are missing from 'data':")
  expect_error(object = perform_assessment_tests(data = invalid_data_3), regexp = "Some required columns are missing from 'data':")
  expect_error(object = perform_assessment_tests(data = invalid_data_4), regexp = "Some required columns are missing from 'data':")
  expect_error(object = perform_assessment_tests(data = invalid_data_5), regexp = "Registered input class of 'data': 'logical'.")
})

test_that(desc = "Testing error messages for invalid 'level' input, having valid other arguments", code = {
  expect_error(object = perform_assessment_tests(data = test_data_repaired_5, level = invalid_level_1), regexp = "It is expected to be a non-missing numeric value strictly between 0 and 1.")
  expect_error(object = perform_assessment_tests(data = test_data_repaired_5, level = invalid_level_2), regexp = "It is expected to be a non-missing numeric value strictly between 0 and 1.")
  expect_error(object = perform_assessment_tests(data = test_data_repaired_5, level = invalid_level_3), regexp = "It is expected to be a non-missing numeric value strictly between 0 and 1.")
  expect_error(object = perform_assessment_tests(data = test_data_repaired_5, level = invalid_level_4), regexp = "It is expected to be a non-missing numeric value strictly between 0 and 1.")
  expect_error(object = perform_assessment_tests(data = test_data_repaired_5, level = invalid_level_5), regexp = "It is expected to be a non-missing numeric value strictly between 0 and 1.")
})

test_that(desc = "Testing error messages for invalid 'B' input, having valid other arguments", code = {
  expect_error(object = perform_assessment_tests(data = test_data_repaired_5, B = invalid_B_1), regexp = "Invalid input for 'B'. Expected an finite integer greater than or equal to 1, but received")
  expect_error(object = perform_assessment_tests(data = test_data_repaired_5, B = invalid_B_2), regexp = "Invalid input for 'B'. Expected an finite integer greater than or equal to 1, but received")
  expect_error(object = perform_assessment_tests(data = test_data_repaired_5, B = invalid_B_3), regexp = "Invalid input for 'B'. Expected an finite integer greater than or equal to 1, but received")
  expect_error(object = perform_assessment_tests(data = test_data_repaired_5, B = invalid_B_4), regexp = "Invalid input for 'B'. Expected an finite integer greater than or equal to 1, but received")
  expect_error(object = perform_assessment_tests(data = test_data_repaired_5, B = invalid_B_5), regexp = "Invalid input for 'B'. Expected an finite integer greater than or equal to 1, but received")
  expect_error(object = perform_assessment_tests(data = test_data_repaired_5, B = invalid_B_6), regexp = "Invalid input for 'B'. Expected an finite integer greater than or equal to 1, but received")
  expect_error(object = perform_assessment_tests(data = test_data_repaired_5, B = invalid_B_7), regexp = "Invalid input for 'B'. Expected an finite integer greater than or equal to 1, but received")
  expect_error(object = perform_assessment_tests(data = test_data_repaired_5, B = invalid_B_8), regexp = "Invalid input for 'B'. Expected an finite integer greater than or equal to 1, but received")
  expect_error(object = perform_assessment_tests(data = test_data_repaired_5, B = invalid_B_9), regexp = "Invalid input for 'B'. Expected an finite integer greater than or equal to 1, but received")
})

test_that(desc = "Testing error messages for invalid 'include_rejection_rates' input (1) and invalid 'simultaneous_testing' input (2)", code = {
  expect_error(object = perform_assessment_tests(data = test_data_repaired_6, B = 10L, include_rejection_rates = random_invalid_include_rejection_rates, na_rm = TRUE), regexp = "It is expected to be a single, non-missing logical value, i.e., TRUE or FALSE.")
  expect_error(object = perform_assessment_tests(data = test_data_repaired_6, B = 10L, include_rejection_rates = FALSE, simultaneous_testing = random_invalid_simultaneous_testing, na_rm = TRUE), regexp = "It is expected to be a single, non-missing logical value, i.e., TRUE or FALSE.")
})







