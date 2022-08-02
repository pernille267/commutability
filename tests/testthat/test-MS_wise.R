library(testthat)
library(commutability)
library(readxl)
library(data.table)
library(fasteqa)

test_data_1 <- setDT(read_excel("~/datasets to be tested on/W..EPK2020_CS.xlsx"))
test_data_2 <- setDT(read_excel("~/datasets to be tested on/HDLC.CS.FILTERED.xlsx"))
test_data_3 <- setDT(read_excel("~/datasets to be tested on/test_data_7.xlsx"))
test_data_4 <- setDT(read_excel("~/datasets to be tested on/test_data_6.xlsx"))
check_data_3 <- check_data(test_data_3)
check_data_4 <- check_data(test_data_4)
test_data_3 <- repair_data(test_data_3, check_data_3)
test_data_4 <- repair_data(test_data_4, check_data_4)

actual_result_1 <- MS_wise(test_data_1)
actual_result_2 <- MS_wise(test_data_2)

test_that(desc = "Testing for valid names I", code = {
  expect_named(object = actual_result_1, expected = c("comparison", "SampleID", "ReplicateID", "MP_A", "MP_B"), ignore.order = TRUE)
  expect_named(object = actual_result_2, expected = c("comparison", "SampleID", "ReplicateID", "MP_A", "MP_B"), ignore.order = TRUE)
})

names(test_data_1)[1:2] <- c("smPl","REpl")
names(test_data_2)[1:2] <- c("Sample_id","R_e_P_i_D")

actual_result_3 <- MS_wise(test_data_1)
actual_result_4 <- MS_wise(test_data_2)
actual_result_5 <- MS_wise(test_data_3)
actual_result_6 <- MS_wise(test_data_4)

test_that(desc = "Testing for valid names II", code = {
  expect_named(object = actual_result_3, expected = c("comparison", "SampleID", "ReplicateID", "MP_A", "MP_B"), ignore.order = TRUE)
  expect_named(object = actual_result_4, expected = c("comparison", "SampleID", "ReplicateID", "MP_A", "MP_B"), ignore.order = TRUE)
  expect_named(object = actual_result_5, expected = c("comparison", "SampleID", "ReplicateID", "MP_A", "MP_B"), ignore.order = TRUE)
  expect_named(object = actual_result_6, expected = c("comparison", "SampleID", "ReplicateID", "MP_A", "MP_B"), ignore.order = TRUE)
})

test_that(desc = "Testing correct output types", code = {
  expect_true(is.character(actual_result_1$comparison))
  expect_true(is.character(actual_result_1$SampleID))
  expect_true(is.character(actual_result_1$ReplicateID))
  expect_true(is.numeric(actual_result_1$MP_A))
  expect_true(is.numeric(actual_result_1$MP_B))

  expect_true(is.character(actual_result_2$comparison))
  expect_true(is.character(actual_result_2$SampleID))
  expect_true(is.character(actual_result_2$ReplicateID))
  expect_true(is.numeric(actual_result_2$MP_A))
  expect_true(is.numeric(actual_result_2$MP_B))

  expect_true(is.character(actual_result_3$comparison))
  expect_true(is.character(actual_result_3$SampleID))
  expect_true(is.character(actual_result_3$ReplicateID))
  expect_true(is.numeric(actual_result_3$MP_A))
  expect_true(is.numeric(actual_result_3$MP_B))

  expect_true(is.character(actual_result_4$comparison))
  expect_true(is.character(actual_result_4$SampleID))
  expect_true(is.character(actual_result_4$ReplicateID))
  expect_true(is.numeric(actual_result_4$MP_A))
  expect_true(is.numeric(actual_result_4$MP_B))

  expect_true(all(is.character(actual_result_5$comparison),
                  is.character(actual_result_5$SampleID),
                  is.character(actual_result_5$ReplicateID),
                  is.numeric(actual_result_5$MP_A),
                  is.numeric(actual_result_5$MP_B)))

  expect_true(all(is.character(actual_result_6$comparison),
                  is.character(actual_result_6$SampleID),
                  is.character(actual_result_6$ReplicateID),
                  is.numeric(actual_result_6$MP_A),
                  is.numeric(actual_result_6$MP_B)))
})

names(test_data_1)[1:2] <- c("Sysmexc","Adviac")
names(test_data_2)[1:2] <- c("xxxx","yyyy")

test_that(desc = "Testing whether error is thrown", code = {
  expect_error(MS_wise(test_data_1))
  expect_error(MS_wise(test_data_2))
})


