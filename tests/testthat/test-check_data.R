library(testthat)
library(readxl)
library(data.table)
library(fasteqa)

# Testing check_data

# General datasets
test_data_1 <- read_excel("~/Packages/datasets to be tested on/W..MCV2020_CS.xlsx")
test_data_2 <- read_excel("~/Packages/datasets to be tested on/HDLC.CS.FILTERED.xlsx")

setDT(test_data_1); setDT(test_data_2)

# Checking if valid datasets are considered valid
test_that(desc = "Test valid datasets", code = {

  # Check if datasets are considered valid even if SampleID and ReplicateID
  # are not exact

  alternative_names_1 <- c("smpl", "repl")
  alternative_names_2 <- c("pasientid", "repeatedid")
  alternative_names_3 <- c("CS...", "dePLicatEID")

  test_data_ugly_ID_columns_1 <- copy(test_data_1)
  test_data_ugly_ID_columns_2 <- copy(test_data_2)
  test_data_ugly_ID_columns_3 <- copy(test_data_2)[, -("Ortho CD Vitros")]

  names(test_data_ugly_ID_columns_1)[1:2] <- alternative_names_1
  names(test_data_ugly_ID_columns_2)[1:2] <- alternative_names_2
  names(test_data_ugly_ID_columns_3)[1:2] <- alternative_names_3

  actual_1 <- check_data(test_data_ugly_ID_columns_1)
  actual_2 <- check_data(test_data_ugly_ID_columns_2)
  actual_3 <- check_data(test_data_ugly_ID_columns_3)

  # Expected to satisfy validity tests
  expect_true(object = all(unlist(actual_1$validity)))
  expect_true(object = all(unlist(actual_2$validity)))
  expect_true(object = all(unlist(actual_3$validity)))

  # Expected scores
  expect_equal(object = actual_1$score, expected = 8)
  expect_equal(object = actual_2$score, expected = 0)
  expect_equal(object = actual_3$score, expected = 5)

  # Expected badges
  expect_true(actual_1$badge == "perfect")
  expect_true(actual_2$badge == "extremely poor")
  expect_true(actual_3$badge == "acceptable")


})

# Checking datasets with invalid ID columns
test_that(desc = "Test datasets with invalid ID columns", code = {

  # Not recongnized SampleID and ReplicateID
  alternative_names_1 <- c("syktpasientproveda",
                           "erdettereplikateller?")

  # Not recongnized SampleID, but ReplicateID is valid
  alternative_names_2 <- c("superduperpasienterosv",
                           sample(typo_suggestions()$ReplicateID, size = 1))

  # Valid SampleID, but ReplicateID is not recongnized
  alternative_names_3 <- c(sample(typo_suggestions()$SampleID, size = 1),
                           "ultrareplikatmåler")

  test_data_invalid_ID_columns_1 <- copy(test_data_1)
  test_data_invalid_ID_columns_2 <- copy(test_data_1)
  test_data_invalid_ID_columns_3 <- copy(test_data_1)

  names(test_data_invalid_ID_columns_1)[1:2] <- alternative_names_1
  names(test_data_invalid_ID_columns_2)[1:2] <- alternative_names_2
  names(test_data_invalid_ID_columns_3)[1:2] <- alternative_names_3

  # Expect that mandatory ID columns test to fail
  expect_true(object = !check_data(test_data_invalid_ID_columns_1)$validity$valid_mandatory_id_columns)
  expect_true(object = !check_data(test_data_invalid_ID_columns_2)$validity$valid_mandatory_id_columns)
  expect_true(object = !check_data(test_data_invalid_ID_columns_3)$validity$valid_mandatory_id_columns)

  # Only numeric columns present
  test_data_invalid_ID_columns_4 <- copy(test_data_1)
  test_data_invalid_ID_columns_4 <- test_data_invalid_ID_columns_4[, -c("SampleID", "ReplicateID")]

  # Expect that mandatory ID columns test to fail
  expect_true(object = !check_data(test_data_invalid_ID_columns_4)$validity$valid_mandatory_id_columns)

})

# Checking datasets with invalid numeric columns
test_that(desc = "Test datasets with invalid numeric columns", code = {

  # One column is character
  test_data_one_nonnumeric_column <- copy(test_data_1)
  test_data_one_nonnumeric_column[, Advia := as.character(Advia)]
  actual_1 <- check_data(test_data_one_nonnumeric_column)

  # Expect that Advia should be converted to numeric
  expect_equal(object = actual_1$repair$convert_these_methods_to_numeric,
               expected = "Advia")

  # Two columns are character
  test_data_two_nonnumeric_column <- copy(test_data_one_nonnumeric_column)
  test_data_two_nonnumeric_column[, ABXmicros := as.character(ABXmicros)]

  actual_2 <-  check_data(test_data_two_nonnumeric_column)

  # Expect that valid_numeric_columns test to fail
  expect_equal(object = actual_2$repair$convert_these_methods_to_numeric,
               expected = c("Advia", "ABXmicros"))

})

# Checking datasets with invalid number of NA values
test_that(desc = "Test datasets with invalid number of NA values", code = {

  # Checking in numeric columns
  test_data_excessive_NA_values <- copy(test_data_2)
  test_data_excessive_NA_values$`Ortho CD Vitros`[c(2:6,
                                                    35:39,
                                                    68:72)] <- NA_real_

  actual_1 <- check_data(test_data_excessive_NA_values)
  expect_true(object = !actual_1$validity$valid_number_nas)
  expect_equal(object = actual_1$badge, expected = "not acceptable")

  # Checking if correct IDs are recommended to take away in repair
  test_data_ID_NA_values <- copy(test_data_1)
  test_data_ID_NA_values$SampleID[c(1, 9, 17)] <- NA
  test_data_ID_NA_values$ReplicateID[c(8, 9, 16)] <- NA

  expected_to_remove <- c(1, 8, 9, 16, 17)
  expected_to_keep <- setdiff(1:72, expected_to_remove)

  actual_1 <- check_data(test_data_ID_NA_values)

  expect_equal(object = actual_1$repair$keep_these_ID_columns_ids,
               expected = expected_to_keep)

  # A more complex situation
  test_data_numeric_and_ID_NA_values <- copy(test_data_ID_NA_values)
  test_data_numeric_and_ID_NA_values[7:8, 3:6] <- NA_real_
  test_data_numeric_and_ID_NA_values[24, c(4, 6)] <- NA_real_

  expected_ID_IDs_to_keep <- expected_to_keep
  expected_numeric_IDs_to_keep <- setdiff(1:67, 6)

  actual_2 <- check_data(test_data_numeric_and_ID_NA_values)

  expect_equal(object = actual_2$repair$keep_these_ID_columns_ids,
               expected = expected_ID_IDs_to_keep)
  expect_equal(object = actual_2$repair$keep_these_numeric_columns_ids,
               expected = expected_numeric_IDs_to_keep)




})

# Checking datasets with invalid number of numeric columns after potential repair
test_that(desc = "Test datasets with invalid number of remaining numeric columns", code = {

  # Check if data is invalid if fewer than two numeric columns are left after repair
  test_data_too_few_remaining_numeric_columns <- copy(test_data_2)[, c("SampleID",
                                                                       "ReplicateID",
                                                                       "Siemens Vista",
                                                                       "Ortho CD Vitros")]

  actual_1 <- check_data(test_data_too_few_remaining_numeric_columns)

  expect_true(object = !actual_1$validity$valid_number_remaining_numeric)
  expect_equal(object = actual_1$badge, expected = "not acceptable")

})

# Testing repair_data

# Check if the correct repairs are made
test_that(desc = "Testing of correct repairs are made", code = {

  test_data_repair_1 <- copy(test_data_1)
  test_data_repair_2 <- copy(test_data_2)

  actual_1 <- repair_data(test_data_repair_1, include_repair_summary = TRUE)$repair_summary
  actual_2 <- repair_data(test_data_repair_2, include_repair_summary = TRUE)$repair_summary

  expect_equal(object = actual_1$`ID columns`,
               expected = c("correct", "correct", "same as before"))
  expect_equal(object = actual_1$`numeric column order`,
               expected = c("not alphabetic", "alphabetic", "to alphabetic"))
  expect_equal(object = actual_1$`score`,
               expected = c(8, 8, 0))
  expect_equal(object = actual_1$`badge`,
               expected = c("perfect", "perfect", "same as before"))

  expect_equal(object = actual_2$`ID columns`,
               expected = c("correct", "correct", "same as before"))
  expect_equal(object = actual_2$`numeric column order`,
               expected = c("not alphabetic", "alphabetic", "to alphabetic"))
  expect_equal(object = actual_2$`score`,
               expected = c(0, 5, 5))
  expect_equal(object = actual_2$`badge`,
               expected = c("extremely poor", "acceptable", "improved"))


})






