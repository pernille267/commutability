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
test_data_8 <- read_excel(path = "~/Packages/datasets to be tested on/test_data_8.xlsx")

check_data_1 <- check_data(test_data_1)
check_data_2 <- check_data(test_data_2)
check_data_3 <- check_data(test_data_3)
check_data_4 <- check_data(test_data_4)
check_data_5 <- check_data(test_data_5)
check_data_6 <- check_data(test_data_6)
check_data_7 <- check_data(test_data_7)
check_data_8 <- check_data(test_data_8)

actual_1 <- repair_data(data = test_data_1, data_check = check_data_1)
actual_2 <- repair_data(data = test_data_2, data_check = check_data_2)
actual_3 <- repair_data(data = test_data_3, data_check = check_data_3)
actual_4 <- repair_data(data = test_data_4, data_check = check_data_4)
actual_5 <- repair_data(data = test_data_5, data_check = check_data_5)
actual_6 <- repair_data(data = test_data_6, data_check = check_data_6)
actual_7 <- repair_data(data = test_data_7, data_check = check_data_7)
actual_8 <- suppressWarnings(repair_data(data = test_data_8, data_check = check_data_8))

test_that(desc = "Checking warnings", code = {
  expect_warning(object = repair_data(data = test_data_8, data_check = check_data_8))
})

expected_col_nu_1 <- 6L
expected_col_nu_2 <- 6L
expected_col_nu_3 <- 8L
expected_col_nu_4 <- 7L
expected_col_nu_5 <- 8L
expected_col_nu_6 <- 8L
expected_col_nu_7 <- 6L
expected_col_nu_8 <- length(names(test_data_8))

test_that(desc = "Check number of columns", code = {
  expect_equal(object = length(names(actual_1)), expected = expected_col_nu_1)
  expect_equal(object = length(names(actual_2)), expected = expected_col_nu_2)
  expect_equal(object = length(names(actual_3)), expected = expected_col_nu_3)
  expect_equal(object = length(names(actual_4)), expected = expected_col_nu_4)
  expect_equal(object = length(names(actual_5)), expected = expected_col_nu_5)
  expect_equal(object = length(names(actual_6)), expected = expected_col_nu_6)
  expect_equal(object = length(names(actual_7)), expected = expected_col_nu_7)
  expect_equal(object = length(names(actual_8)), expected = expected_col_nu_8)
})

NAs_before_1 <- setDT(lapply(X = test_data_1[, -c(1,2)], FUN = function(x) sum(is.na(x))))
NAs_before_2 <- setDT(lapply(X = test_data_2[, -c(1,2)], FUN = function(x) sum(is.na(x))))
NAs_before_3 <- setDT(lapply(X = test_data_3[, -c(1,2)], FUN = function(x) sum(is.na(x))))
NAs_before_4 <- setDT(lapply(X = test_data_4[, -c(1,2)], FUN = function(x) sum(is.na(x))))[,-"IVD_MD_4"]
NAs_before_5 <- setDT(lapply(X = test_data_5[, -c(1,2)], FUN = function(x) sum(is.na(x))))
NAs_before_6 <- setDT(lapply(X = test_data_6[, -c(1,2)], FUN = function(x) sum(is.na(x))))
NAs_before_7 <- setDT(lapply(X = test_data_7[, -c(1,2)], FUN = function(x) sum(is.na(x))))
NAs_before_8 <- setDT(lapply(X = test_data_8[, -c(1,2)], FUN = function(x) sum(is.na(x))))

NAs_after_1 <- setDT(lapply(X = actual_1[, -c(1,2)], FUN = function(x) sum(is.na(x))))
NAs_after_2 <- setDT(lapply(X = actual_2[, -c(1,2)], FUN = function(x) sum(is.na(x))))
NAs_after_3 <- setDT(lapply(X = actual_3[, -c(1,2)], FUN = function(x) sum(is.na(x))))
NAs_after_4 <- setDT(lapply(X = actual_4[, -c(1,2)], FUN = function(x) sum(is.na(x))))
NAs_after_5 <- setDT(lapply(X = actual_5[, -c(1,2)], FUN = function(x) sum(is.na(x))))
NAs_after_6 <- setDT(lapply(X = actual_6[, -c(1,2)], FUN = function(x) sum(is.na(x))))
NAs_after_7 <- setDT(lapply(X = actual_7[, -c(1,2)], FUN = function(x) sum(is.na(x))))
NAs_after_8 <- setDT(lapply(X = actual_8[, -c(1,2)], FUN = function(x) sum(is.na(x))))

expected_NA_counts_after_6 <- as.list(c(5L, 2L, 2L, 6L, 4L, 3L))
names(expected_NA_counts_after_6) <- names(NAs_after_6); setDT(expected_NA_counts_after_6)

expected_NA_counts_after_7 <- as.list(c(0L, 3L, 9L, 3L))
names(expected_NA_counts_after_7) <- names(NAs_after_7); setDT(expected_NA_counts_after_7)

test_that(desc = "Testing number of NA values before and after repair", code = {
  expect_identical(NAs_before_1, NAs_after_1)
  expect_identical(NAs_before_2, NAs_after_2)
  expect_identical(NAs_before_3, NAs_after_3)
  expect_identical(NAs_before_4, NAs_after_4)
  expect_identical(NAs_before_5, NAs_after_5)
  expect_true(all(NAs_before_6 < NAs_after_6))
  expect_true(all(NAs_before_7 <= NAs_after_7))
  expect_identical(NAs_after_6, expected_NA_counts_after_6)
  expect_identical(NAs_after_7, expected_NA_counts_after_7)
  expect_identical(NAs_before_8, NAs_after_8)
})

test_that(desc = "Testing whether column IDs where fixed when possible", code = {
  expect_length(object = which(names(actual_1) %in% c("SampleID", "ReplicateID")), n = 2)
  expect_length(object = which(names(actual_2) %in% c("SampleID", "ReplicateID")), n = 2)
  expect_length(object = which(names(actual_3) %in% c("SampleID", "ReplicateID")), n = 2)
  expect_length(object = which(names(actual_4) %in% c("SampleID", "ReplicateID")), n = 2)
  expect_length(object = which(names(actual_5) %in% c("SampleID", "ReplicateID")), n = 2)
  expect_length(object = which(names(actual_6) %in% c("SampleID", "ReplicateID")), n = 2)
  expect_length(object = which(names(actual_7) %in% c("SampleID", "ReplicateID")), n = 2)
  expect_length(object = which(names(actual_8) %in% c("SampleID", "ReplicateID")), n = 0)
})

expected_non_id_cols_1 <- 4L
expected_non_id_cols_2 <- 4L
expected_non_id_cols_3 <- 6L
expected_non_id_cols_4 <- 5L
expected_non_id_cols_5 <- 6L
expected_non_id_cols_6 <- 6L
expected_non_id_cols_7 <- 4L

test_that(desc = "Testing whether column non-ID columns are total columns - id cols ", code = {
  expect_length(object = which(!names(actual_1) %in% c("SampleID", "ReplicateID")), n = expected_non_id_cols_1)
  expect_length(object = which(!names(actual_2) %in% c("SampleID", "ReplicateID")), n = expected_non_id_cols_2)
  expect_length(object = which(!names(actual_3) %in% c("SampleID", "ReplicateID")), n = expected_non_id_cols_3)
  expect_length(object = which(!names(actual_4) %in% c("SampleID", "ReplicateID")), n = expected_non_id_cols_4)
  expect_length(object = which(!names(actual_5) %in% c("SampleID", "ReplicateID")), n = expected_non_id_cols_5)
  expect_length(object = which(!names(actual_6) %in% c("SampleID", "ReplicateID")), n = expected_non_id_cols_6)
  expect_length(object = which(!names(actual_7) %in% c("SampleID", "ReplicateID")), n = expected_non_id_cols_7)
})

expected_numeric_cols_1 <- 4L
expected_numeric_cols_2 <- 4L
expected_numeric_cols_3 <- 6L
expected_numeric_cols_4 <- 5L
expected_numeric_cols_5 <- 6L
expected_numeric_cols_6 <- 6L
expected_numeric_cols_7 <- 4L
expected_numeric_cols_8 <- 3L # Because repairing is terminated, so the one character vector could not be fixed

actual_numeric_cols_1 <- sum(unlist(lapply(X = actual_1, FUN = is.numeric)))
actual_numeric_cols_2 <- sum(unlist(lapply(X = actual_2, FUN = is.numeric)))
actual_numeric_cols_3 <- sum(unlist(lapply(X = actual_3, FUN = is.numeric)))
actual_numeric_cols_4 <- sum(unlist(lapply(X = actual_4, FUN = is.numeric)))
actual_numeric_cols_5 <- sum(unlist(lapply(X = actual_5, FUN = is.numeric)))
actual_numeric_cols_6 <- sum(unlist(lapply(X = actual_6, FUN = is.numeric)))
actual_numeric_cols_7 <- sum(unlist(lapply(X = actual_7, FUN = is.numeric)))
actual_numeric_cols_8 <- sum(unlist(lapply(X = actual_8, FUN = is.numeric)))

test_that(desc = "Testing whether column IDs where fixed when possible", code = {
  expect_identical(object = actual_numeric_cols_1, expected = expected_numeric_cols_1)
  expect_identical(object = actual_numeric_cols_2, expected = expected_numeric_cols_2)
  expect_identical(object = actual_numeric_cols_3, expected = expected_numeric_cols_3)
  expect_identical(object = actual_numeric_cols_4, expected = expected_numeric_cols_4)
  expect_identical(object = actual_numeric_cols_5, expected = expected_numeric_cols_5)
  expect_identical(object = actual_numeric_cols_6, expected = expected_numeric_cols_6)
  expect_identical(object = actual_numeric_cols_7, expected = expected_numeric_cols_7)
  expect_identical(object = actual_numeric_cols_8, expected = expected_numeric_cols_8)
})

state_repaired_data_1 <- check_data(actual_1)[["for_human"]][["validity of input data is :"]][1]
state_repaired_data_2 <- check_data(actual_2)[["for_human"]][["validity of input data is :"]][1]
state_repaired_data_3 <- check_data(actual_3)[["for_human"]][["validity of input data is :"]][1]
state_repaired_data_4 <- check_data(actual_4)[["for_human"]][["validity of input data is :"]][1]
state_repaired_data_5 <- check_data(actual_5)[["for_human"]][["validity of input data is :"]][1]
state_repaired_data_6 <- check_data(actual_6)[["for_human"]][["validity of input data is :"]][1]
state_repaired_data_7 <- check_data(actual_7)[["for_human"]][["validity of input data is :"]][1]
state_repaired_data_8 <- check_data(actual_8)[["for_human"]][["validity of input data is :"]][1]

test_that(desc = "Testing whether repairing made the data perfect or not", code = {
  expect_identical(object = state_repaired_data_1, expected = "perfect")
  expect_identical(object = state_repaired_data_2, expected = "perfect")
  expect_identical(object = state_repaired_data_3, expected = "perfect")
  expect_identical(object = state_repaired_data_4, expected = "perfect")
  expect_identical(object = state_repaired_data_5, expected = "perfect")
  expect_identical(object = state_repaired_data_6, expected = "perfect")
  expect_identical(object = state_repaired_data_7, expected = "perfect")
  expect_identical(object = state_repaired_data_8, expected = "not acceptable")
})

test_that(desc = "Testing whether measurement results are positive", code = {
  expect_true(all(unlist(lapply(X = actual_4[, -c(1,2)], FUN = function(x) all(x > 0 | is.na(x))))))
  expect_true(all(unlist(lapply(X = actual_5[, -c(1,2)], FUN = function(x) all(x > 0 | is.na(x))))))
  expect_true(all(unlist(lapply(X = actual_6[, -c(1,2)], FUN = function(x) all(x > 0 | is.na(x))))))
  expect_true(all(unlist(lapply(X = actual_7[, -c(1,2)], FUN = function(x) all(x > 0 | is.na(x))))))
  expect_true(all(unlist(lapply(X = actual_3[, -c(1,2)], FUN = function(x) all(x > 0 | is.na(x))))))
})
