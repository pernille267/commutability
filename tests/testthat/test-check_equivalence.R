suppressWarnings(library(data.table))
library(stringi)
library(commutability)
library(fasteqa)
library(readxl)

test_cs_data_raw <- setDT(read_excel(path = "~/Packages/datasets to be tested on/simulated_cholesterol_cs_data.xlsx"))
test_eq_data_raw <- setDT(read_excel(path = "~/Packages/datasets to be tested on/simulated_cholesterol_eq_data.xlsx"))

test_cs_data_1 <- test_cs_data_raw
test_eq_data_1 <- test_eq_data_raw
test_cs_data_2 <- test_cs_data_raw
names(test_cs_data_2)[which("SampleID" == names(test_cs_data_2))] <- "sampleid"
test_eq_data_2 <- test_eq_data_raw
test_cs_data_3 <- test_cs_data_raw
names(test_cs_data_3)[which("ReplicateID" == names(test_cs_data_3))] <- "replicateid"
test_eq_data_3 <- test_eq_data_raw
test_cs_data_4 <- test_cs_data_raw
test_cs_data_4$`Siemens EXL` <- NULL
test_eq_data_4 <- test_eq_data_raw
test_cs_data_5 <- copy(test_cs_data_4)
test_eq_data_5 <- test_eq_data_4
setcolorder(test_cs_data_5, c("SampleID", "ReplicateID", "Abbott Architect", "Beckman AU", "Beckman DxC", "Ortho CD Vitros", "Roche Cobas", "Siemens Vista", "Siemens Advia"))

actual_1 <- check_equivalence(cs_data = test_cs_data_1, eq_data = test_eq_data_1)
actual_2 <- check_equivalence(cs_data = test_cs_data_2, eq_data = test_eq_data_2)
actual_3 <- check_equivalence(cs_data = test_cs_data_3, eq_data = test_eq_data_3)
actual_4 <- check_equivalence(cs_data = test_cs_data_4, eq_data = test_eq_data_4)
actual_5 <- check_equivalence(cs_data = test_cs_data_5, eq_data = test_eq_data_5)

expected_1 <- list("equivalent_id_columns" = TRUE, "equivalent_numeric_column_names" = FALSE, "equivalent_numeric_column_order" = TRUE)
expected_2 <- list("equivalent_id_columns" = FALSE, "equivalent_numeric_column_names" = FALSE, "equivalent_numeric_column_order" = FALSE)
expected_3 <- list("equivalent_id_columns" = FALSE, "equivalent_numeric_column_names" = FALSE, "equivalent_numeric_column_order" = FALSE)
expected_4 <- list("equivalent_id_columns" = TRUE, "equivalent_numeric_column_names" = TRUE, "equivalent_numeric_column_order" = TRUE)
expected_5 <- list("equivalent_id_columns" = TRUE, "equivalent_numeric_column_names" = TRUE, "equivalent_numeric_column_order" = FALSE)

test_that(desc = "Testing correctness of output", code = {
  expect_identical(object = actual_1, expected = expected_1)
  expect_identical(object = actual_2, expected = expected_2)
  expect_identical(object = actual_3, expected = expected_3)
  expect_identical(object = actual_4, expected = expected_4)
  expect_identical(object = actual_5, expected = expected_5)
})

test_that(desc = "Testing warnings", code = {
  expect_warning(object = check_equivalence(cs_data = test_cs_data_1, eq_data = c(1)), regexp = "eq_data")
  expect_warning(object = check_equivalence(cs_data = test_cs_data_1, eq_data = c(1)), regexp = "numeric")
  expect_warning(object = check_equivalence(cs_data = c("1"), eq_data = test_eq_data_1), regexp = "cs_data")
  expect_warning(object = check_equivalence(cs_data = c("1"), eq_data = test_eq_data_1), regexp = "character")
  expect_warning(object = check_equivalence(cs_data = c("1"), eq_data = c("1")), regexp = "cs_data")
})

actual_1 <- suppressWarnings(check_equivalence(cs_data = test_cs_data_1, eq_data = c(1)))
actual_2 <- suppressWarnings(check_equivalence(cs_data = c("1"), eq_data = test_eq_data_1))
actual_3 <- suppressWarnings(check_equivalence(cs_data = c("1"), eq_data = "THIS IS WRONG!"))

test_that(desc = "Testing output when input is very invalid", code = {
  expect_equal(object = actual_1, expected = expected_2)
  expect_equal(object = actual_2, expected = expected_2)
  expect_equal(object = actual_3, expected = expected_2)
})

test_that(desc = "Testing verbose", code = {
  expect_message(object = check_equivalence(cs_data = test_cs_data_1, eq_data = test_eq_data_1, silence = 0L), regexp = "Siemens EXL")
  expect_message(object = check_equivalence(cs_data = test_cs_data_2, eq_data = test_eq_data_2, silence = 0L), regexp = "required id columns")
})


