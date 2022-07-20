library(testthat)
library(commutability)
library(readxl)
library(data.table)
library(fasteqa)

test_data_1 <- read_excel("~/datasets to be tested on/W..MCV2020_CS.xlsx")
test_data_2 <- read_excel("~/datasets to be tested on/HDLC.CS.FILTERED.xlsx")
test_data_3 <- test_data_2[,c("SampleID", "ReplicateID", "Ortho CD Vitros", "Siemens Advia")]
test_data_4 <- test_data_2[,c("SampleID", "ReplicateID",
                              "Ortho CD Vitros", "Siemens Advia", "Beckman AU")]

test_data_5 <- test_data_1
test_data_5$SampleID[c(1,3,9,11,12,15)] <- NA
test_data_5$ReplicateID[c(1,2,5,8)] <- NA

test_data_6 <- test_data_1
names(test_data_6)[1:2] <- c("smpl","repl")
test_data_7 <- test_data_1
names(test_data_7)[1:2] <- c("sm","rep")
test_data_8 <- test_data_1
names(test_data_8)[1:2] <- c("CS_ID","Replicate_ID")
test_data_9 <- test_data_1
names(test_data_9)[1:2] <- c("xample","rfreplicatex")
test_data_10 <- test_data_1
names(test_data_10)[1:2] <- c("x","r_efclicate")
test_data_11 <- test_data_2[,-which(names(test_data_2) == "Ortho CD Vitros")]

test_data_12 <- test_data_1
test_data_12$SampleID[c(1,2,3,7,8,9,10,11,12,15,19,20,21,22,31,32,33,34,55,59,61,66,70,71,72)] <- NA

test_data_13 <- test_data_1
test_data_13$ReplicateID[c(1,2,3,7,8,9,10,11,12,15,19,20,21,22,31,32,33,34,55,59,61,66,70,71,72)] <- NA

test_data_14 <- test_data_1
test_data_14$SampleID[c(1,2,3,7,8,9,10,11,34,55,59,61,66,70,71,72)] <- NA
test_data_14$ReplicateID[c(12,15,19,20,21,22,31,32,33)] <- NA

test_data_15 <- test_data_2
test_data_15$`Roche Cobas`[1:60] <- NA
test_data_15$`Ortho CD Vitros`[98] <- "<. "




actual_1 <- check_data(test_data_1,1)
actual_2 <- check_data(test_data_2,1)
actual_3 <- check_data(test_data_3,1)
actual_4 <- check_data(test_data_4,1)
actual_5 <- check_data(test_data_5,1)
actual_6 <- check_data(test_data_6,1)
actual_7 <- check_data(test_data_7,1)
actual_8 <- check_data(test_data_8,1)
actual_9 <- check_data(test_data_9,1)
actual_10 <- check_data(test_data_10,1)
actual_11 <- check_data(test_data_11,1)
actual_12 <- check_data(test_data_12,1)
actual_13 <- check_data(test_data_13,1)
actual_14 <- check_data(test_data_14,1)
actual_15 <- check_data(test_data_15,1)

expected_1_for_computer_checks <- list("valid_mandatory_id_columns" = TRUE,
                                       "valid_numeric_columns" = TRUE,
                                       "valid_number_nas" = TRUE,
                                       "valid_number_remaining_numeric" = TRUE,
                                       "perfect_number_nas_SampleID" = TRUE,
                                       "perfect_number_nas_ReplicateID" = TRUE,
                                       "acceptable_number_nas_SampleID" = TRUE,
                                       "acceptable_number_nas_ReplicateID" = TRUE)

expected_2_for_computer_checks <- list("valid_mandatory_id_columns" = TRUE,
                                       "valid_numeric_columns" = TRUE,
                                       "valid_number_nas" = FALSE,
                                       "valid_number_remaining_numeric" = TRUE,
                                       "perfect_number_nas_SampleID" = TRUE,
                                       "perfect_number_nas_ReplicateID" = TRUE,
                                       "acceptable_number_nas_SampleID" = TRUE,
                                       "acceptable_number_nas_ReplicateID" = TRUE)

expected_3_for_computer_checks <- list("valid_mandatory_id_columns" = TRUE,
                                       "valid_numeric_columns" = TRUE,
                                       "valid_number_nas" = FALSE,
                                       "valid_number_remaining_numeric" = FALSE,
                                       "perfect_number_nas_SampleID" = TRUE,
                                       "perfect_number_nas_ReplicateID" = TRUE,
                                       "acceptable_number_nas_SampleID" = TRUE,
                                       "acceptable_number_nas_ReplicateID" = TRUE)

expected_4_for_computer_checks <- list("valid_mandatory_id_columns" = TRUE,
                                       "valid_numeric_columns" = TRUE,
                                       "valid_number_nas" = FALSE,
                                       "valid_number_remaining_numeric" = TRUE,
                                       "perfect_number_nas_SampleID" = TRUE,
                                       "perfect_number_nas_ReplicateID" = TRUE,
                                       "acceptable_number_nas_SampleID" = TRUE,
                                       "acceptable_number_nas_ReplicateID" = TRUE)

expected_5_for_computer_checks <- list("valid_mandatory_id_columns" = TRUE,
                                       "valid_numeric_columns" = TRUE,
                                       "valid_number_nas" = TRUE,
                                       "valid_number_remaining_numeric" = TRUE,
                                       "perfect_number_nas_SampleID" = TRUE,
                                       "perfect_number_nas_ReplicateID" = TRUE,
                                       "acceptable_number_nas_SampleID" = TRUE,
                                       "acceptable_number_nas_ReplicateID" = TRUE)

expected_6_for_computer_checks <- list("valid_mandatory_id_columns" = TRUE,
                                       "valid_numeric_columns" = TRUE,
                                       "valid_number_nas" = TRUE,
                                       "valid_number_remaining_numeric" = TRUE,
                                       "perfect_number_nas_SampleID" = TRUE,
                                       "perfect_number_nas_ReplicateID" = TRUE,
                                       "acceptable_number_nas_SampleID" = TRUE,
                                       "acceptable_number_nas_ReplicateID" = TRUE)

expected_7_for_computer_checks <- list("valid_mandatory_id_columns" = FALSE,
                                       "valid_numeric_columns" = NA,
                                       "valid_number_nas" = NA,
                                       "valid_number_remaining_numeric" = NA,
                                       "perfect_number_nas_SampleID" = NA,
                                       "perfect_number_nas_ReplicateID" = NA,
                                       "acceptable_number_nas_SampleID" = NA,
                                       "acceptable_number_nas_ReplicateID" = NA)

expected_8_for_computer_checks <- list("valid_mandatory_id_columns" = TRUE,
                                       "valid_numeric_columns" = TRUE,
                                       "valid_number_nas" = TRUE,
                                       "valid_number_remaining_numeric" = TRUE,
                                       "perfect_number_nas_SampleID" = TRUE,
                                       "perfect_number_nas_ReplicateID" = TRUE,
                                       "acceptable_number_nas_SampleID" = TRUE,
                                       "acceptable_number_nas_ReplicateID" = TRUE)

expected_9_for_computer_checks <- list("valid_mandatory_id_columns" = FALSE,
                                       "valid_numeric_columns" = NA,
                                       "valid_number_nas" = NA,
                                       "valid_number_remaining_numeric" = NA,
                                       "perfect_number_nas_SampleID" = NA,
                                       "perfect_number_nas_ReplicateID" = NA,
                                       "acceptable_number_nas_SampleID" = NA,
                                       "acceptable_number_nas_ReplicateID" = NA)

expected_10_for_computer_checks <- list("valid_mandatory_id_columns" = FALSE,
                                        "valid_numeric_columns" = NA,
                                        "valid_number_nas" = NA,
                                        "valid_number_remaining_numeric" = NA,
                                        "perfect_number_nas_SampleID" = NA,
                                        "perfect_number_nas_ReplicateID" = NA,
                                        "acceptable_number_nas_SampleID" = NA,
                                        "acceptable_number_nas_ReplicateID" = NA)

expected_11_for_computer_checks <- list("valid_mandatory_id_columns" = TRUE,
                                        "valid_numeric_columns" = TRUE,
                                        "valid_number_nas" = TRUE,
                                        "valid_number_remaining_numeric" = TRUE,
                                        "perfect_number_nas_SampleID" = TRUE,
                                        "perfect_number_nas_ReplicateID" = TRUE,
                                        "acceptable_number_nas_SampleID" = TRUE,
                                        "acceptable_number_nas_ReplicateID" = TRUE)

expected_12_for_computer_checks <- list("valid_mandatory_id_columns" = TRUE,
                                        "valid_numeric_columns" = TRUE,
                                        "valid_number_nas" = TRUE,
                                        "valid_number_remaining_numeric" = TRUE,
                                        "perfect_number_nas_SampleID" = FALSE,
                                        "perfect_number_nas_ReplicateID" = FALSE,
                                        "acceptable_number_nas_SampleID" = TRUE,
                                        "acceptable_number_nas_ReplicateID" = TRUE)

expected_13_for_computer_checks <- list("valid_mandatory_id_columns" = TRUE,
                                        "valid_numeric_columns" = TRUE,
                                        "valid_number_nas" = TRUE,
                                        "valid_number_remaining_numeric" = TRUE,
                                        "perfect_number_nas_SampleID" = FALSE,
                                        "perfect_number_nas_ReplicateID" = FALSE,
                                        "acceptable_number_nas_SampleID" = TRUE,
                                        "acceptable_number_nas_ReplicateID" = TRUE)

expected_14_for_computer_checks <- list("valid_mandatory_id_columns" = TRUE,
                                        "valid_numeric_columns" = TRUE,
                                        "valid_number_nas" = TRUE,
                                        "valid_number_remaining_numeric" = TRUE,
                                        "perfect_number_nas_SampleID" = FALSE,
                                        "perfect_number_nas_ReplicateID" = FALSE,
                                        "acceptable_number_nas_SampleID" = TRUE,
                                        "acceptable_number_nas_ReplicateID" = TRUE)

expected_15_for_computer_checks <- list("valid_mandatory_id_columns" = TRUE,
                                        "valid_numeric_columns" = FALSE,
                                        "valid_number_nas" = FALSE,
                                        "valid_number_remaining_numeric" = TRUE,
                                        "perfect_number_nas_SampleID" = TRUE,
                                        "perfect_number_nas_ReplicateID" = TRUE,
                                        "acceptable_number_nas_SampleID" = TRUE,
                                        "acceptable_number_nas_ReplicateID" = TRUE)

expected_1_for_human_validity_checks <- "perfect"
expected_2_for_human_validity_checks <- "not acceptable"
expected_3_for_human_validity_checks <- "not acceptable"
expected_4_for_human_validity_checks <- "not acceptable"
expected_5_for_human_validity_checks <- "perfect"
expected_6_for_human_validity_checks <- "perfect"
expected_7_for_human_validity_checks <- "not acceptable"
expected_8_for_human_validity_checks <- "perfect"
expected_9_for_human_validity_checks <- "not acceptable"
expected_10_for_human_validity_checks <- "not acceptable"
expected_11_for_human_validity_checks <- "perfect"
expected_12_for_human_validity_checks <- "acceptable"
expected_13_for_human_validity_checks <- "acceptable"
expected_14_for_human_validity_checks <- "acceptable"
expected_15_for_human_validity_checks <- "not acceptable"

test_that(desc = "Testing for computer checks", code = {
  expect_identical(actual_1$for_computer_checks, expected_1_for_computer_checks)
  expect_identical(actual_2$for_computer_checks, expected_2_for_computer_checks)
  expect_identical(actual_3$for_computer_checks, expected_3_for_computer_checks)
  expect_identical(actual_4$for_computer_checks, expected_4_for_computer_checks)
  expect_identical(actual_5$for_computer_checks, expected_5_for_computer_checks)
  expect_identical(actual_6$for_computer_checks, expected_6_for_computer_checks)
  expect_identical(actual_7$for_computer_checks, expected_7_for_computer_checks)
  expect_identical(actual_8$for_computer_checks, expected_8_for_computer_checks)
  expect_identical(actual_9$for_computer_checks, expected_9_for_computer_checks)
  expect_identical(actual_10$for_computer_checks, expected_10_for_computer_checks)
  expect_identical(actual_11$for_computer_checks, expected_11_for_computer_checks)
  expect_identical(actual_12$for_computer_checks, expected_12_for_computer_checks)
  expect_identical(actual_13$for_computer_checks, expected_13_for_computer_checks)
  expect_identical(actual_14$for_computer_checks, expected_14_for_computer_checks)
  expect_identical(actual_15$for_computer_checks, expected_15_for_computer_checks)
})

test_that(desc = "Testing for human validity checks", code = {
  expect_equal(actual_1$for_human$`validity of input data is :`[1], expected_1_for_human_validity_checks)
  expect_equal(actual_2$for_human$`validity of input data is :`[1], expected_2_for_human_validity_checks)
  expect_equal(actual_3$for_human$`validity of input data is :`[1], expected_3_for_human_validity_checks)
  expect_equal(actual_4$for_human$`validity of input data is :`[1], expected_4_for_human_validity_checks)
  expect_equal(actual_5$for_human$`validity of input data is :`[1], expected_5_for_human_validity_checks)
  expect_equal(actual_6$for_human$`validity of input data is :`[1], expected_6_for_human_validity_checks)
  expect_equal(actual_7$for_human$`validity of input data is :`[1], expected_7_for_human_validity_checks)
  expect_equal(actual_8$for_human$`validity of input data is :`[1], expected_8_for_human_validity_checks)
  expect_equal(actual_9$for_human$`validity of input data is :`[1], expected_9_for_human_validity_checks)
  expect_equal(actual_10$for_human$`validity of input data is :`[1], expected_10_for_human_validity_checks)
  expect_equal(actual_11$for_human$`validity of input data is :`[1], expected_11_for_human_validity_checks)
  expect_equal(actual_12$for_human$`validity of input data is :`[1], expected_12_for_human_validity_checks)
  expect_equal(actual_13$for_human$`validity of input data is :`[1], expected_13_for_human_validity_checks)
  expect_equal(actual_14$for_human$`validity of input data is :`[1], expected_14_for_human_validity_checks)
  expect_equal(actual_15$for_human$`validity of input data is :`[1], expected_15_for_human_validity_checks)
})

expected_exact_2 <- list("exclude_these_numeric_columns" = "Ortho CD Vitros",
                         "NA_indices_of_SampleID" = NA,
                         "NA_indices_of_ReplicateID" = NA,
                         "NA_indices_must_exclude" = NA)

expected_exact_4 <- list("exclude_these_numeric_columns" = "Ortho CD Vitros",
                         "NA_indices_of_SampleID" = NA,
                         "NA_indices_of_ReplicateID" = NA,
                         "NA_indices_must_exclude" = NA)

expected_exact_5 <- list("exclude_these_numeric_columns" = NA,
                         "NA_indices_of_SampleID" = as.integer(c(1, 3, 9, 11, 12, 15)),
                         "NA_indices_of_ReplicateID" = as.integer(c(1, 2, 5, 8)),
                         "NA_indices_must_exclude" = as.integer(c(1, 2, 3, 5, 8, 9, 11, 12, 15)))

expected_exact_12 <- list("exclude_these_numeric_columns" = NA,
                         "NA_indices_of_SampleID" = as.integer(c(1,2,3,7,8,9,10,11,12,15,19,20,21,22,31,32,33,34,55,59,61,66,70,71,72)),
                         "NA_indices_of_ReplicateID" = NA,
                         "NA_indices_must_exclude" = as.integer(c(1,2,3,7,8,9,10,11,12,15,19,20,21,22,31,32,33,34,55,59,61,66,70,71,72)))

expected_exact_13 <- list("exclude_these_numeric_columns" = NA,
                          "NA_indices_of_SampleID" = NA,
                          "NA_indices_of_ReplicateID" = as.integer(c(1,2,3,7,8,9,10,11,12,15,19,20,21,22,31,32,33,34,55,59,61,66,70,71,72)),
                          "NA_indices_must_exclude" = as.integer(c(1,2,3,7,8,9,10,11,12,15,19,20,21,22,31,32,33,34,55,59,61,66,70,71,72)))

expected_exact_14 <- list("exclude_these_numeric_columns" = NA,
                          "NA_indices_of_SampleID" = as.integer(c(1,2,3,7,8,9,10,11,34,55,59,61,66,70,71,72)),
                          "NA_indices_of_ReplicateID" = as.integer(c(12,15,19,20,21,22,31,32,33)),
                          "NA_indices_must_exclude" = as.integer(c(1,2,3,7,8,9,10,11,12,15,19,20,21,22,31,32,33,34,55,59,61,66,70,71,72)))

expected_exact_15 <- list("exclude_these_numeric_columns" = c("Roche Cobas", "Ortho CD Vitros"),
                          "NA_indices_of_SampleID" = NA,
                          "NA_indices_of_ReplicateID" = NA,
                          "NA_indices_must_exclude" = NA)




test_that(desc = "Testing for computer information", code = {
  expect_true(all(unlist(lapply(actual_1$for_computer_information, is.na))))
  expect_true(sum(unlist(lapply(actual_2$for_computer_information, is.na)))==3)
  expect_true(all(unlist(lapply(actual_3$for_computer_information, is.na))))
  expect_true(sum(unlist(lapply(actual_4$for_computer_information, is.na)))==3)
  expect_true(sum(unlist(lapply(actual_5$for_computer_information, is.na)))==1)
  expect_true(all(unlist(lapply(actual_6$for_computer_information, is.na))))
  expect_true(all(unlist(lapply(actual_7$for_computer_information, is.na))))
  expect_true(all(unlist(lapply(actual_8$for_computer_information, is.na))))
  expect_true(all(unlist(lapply(actual_9$for_computer_information, is.na))))
  expect_true(all(unlist(lapply(actual_10$for_computer_information, is.na))))
  expect_true(all(unlist(lapply(actual_11$for_computer_information, is.na))))
  expect_true(sum(unlist(lapply(actual_12$for_computer_information, is.na)))==2)
  expect_true(sum(unlist(lapply(actual_13$for_computer_information, is.na)))==2)
  expect_true(sum(unlist(lapply(actual_14$for_computer_information, is.na)))==1)
  expect_true(sum(unlist(lapply(actual_15$for_computer_information, is.na)))==3)
  expect_identical(actual_2$for_computer_information, expected_exact_2)
  expect_identical(actual_4$for_computer_information, expected_exact_4)
  expect_identical(actual_5$for_computer_information, expected_exact_5)
  expect_identical(actual_12$for_computer_information, expected_exact_12)
  expect_identical(actual_13$for_computer_information, expected_exact_13)
  expect_identical(actual_14$for_computer_information, expected_exact_14)
  expect_identical(actual_15$for_computer_information, expected_exact_15)
})

run_times <- microbenchmark::microbenchmark(check_data(data = test_data_1),
                                            check_data(data = test_data_2),
                                            check_data(data = test_data_3),
                                            check_data(data = test_data_4),
                                            check_data(data = test_data_5),
                                            check_data(data = test_data_6),
                                            check_data(data = test_data_7),
                                            check_data(data = test_data_8),
                                            check_data(data = test_data_9),
                                            check_data(data = test_data_10),
                                            check_data(data = test_data_11),
                                            check_data(data = test_data_12),
                                            check_data(data = test_data_13),
                                            check_data(data = test_data_14),
                                            check_data(data = test_data_15),
                                            times = 2e2)
run_times <- setDT(as.list(run_times))
run_times_list <- split(x = run_times, by = "expr")
run_times_list <- lapply(X = run_times_list, function(x) x$time / 1e6)

all_max_under <- lapply(X = run_times_list, function(x) any(sort(x)[190:200] < 100))
all_min_under <- lapply(X = run_times_list, function(x) any(sort(x)[1:10] < 10))
all_median_under <- lapply(X = run_times_list, function(x) median(x) < 50)

test_that(desc = "Testing performance", code = {
  expect_true(all(unlist(all_max_under)))
  expect_true(all(unlist(all_min_under)))
  expect_true(all(unlist(all_median_under)))
})




