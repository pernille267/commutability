library(testthat)
library(commutability)
library(readxl)
library(data.table)
library(fasteqa)

test_data_1 <- read_excel("~/datasets to be tested on/W..MCV2020_CS.xlsx")
test_data_2 <- read_excel("~/datasets to be tested on/HDLC.CS.FILTERED.xlsx")
test_data_2$`Ortho CD Vitros` <- NULL

check_1 <- check_data(data = test_data_1, silence = 1L, type = "cs")
check_2 <- check_data(data = test_data_2, silence = 1L, type = "cs")

test_data_1 <- repair_data(data = test_data_1, data_check = check_1, silence = 1L) |> MS_wise() |> na.omit()
test_data_2 <- repair_data(data = test_data_2, data_check = check_2, silence = 1L) |> MS_wise() |> na.omit()

# # # # # TEST 1 # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # Testing whether we move around in the function body as we would like # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

random_what <- sample(x = c("raw", "rd", "ad", "ld"), size = 16, replace = T)

actual_11 <- do_outlier_analysis(data = test_data_1, type = "qrange", where = "replicates", what = random_what[1], testing = TRUE)
actual_21 <- do_outlier_analysis(data = test_data_1, type = "burnett", where = "replicates", what = random_what[2], testing = TRUE)
actual_31 <- do_outlier_analysis(data = test_data_1, type = "3sd", where = "replicates", what = random_what[3], testing = TRUE)
actual_41 <- do_outlier_analysis(data = test_data_1, type = "5cv", where = "replicates", what = random_what[4], testing = TRUE)

actual_12 <- do_outlier_analysis(data = test_data_2, type = "qrange", where = "replicates", what = random_what[5], testing = TRUE)
actual_22 <- do_outlier_analysis(data = test_data_2, type = "burnett", where = "replicates", what = random_what[6], testing = TRUE)
actual_32 <- do_outlier_analysis(data = test_data_2, type = "3sd", where = "replicates", what = random_what[7], testing = TRUE)
actual_42 <- do_outlier_analysis(data = test_data_2, type = "5cv", where = "replicates", what = random_what[8], testing = TRUE)

actual_51 <- do_outlier_analysis(data = test_data_1, type = "qrange", where = "samples", what = random_what[9], testing = TRUE)
actual_61 <- do_outlier_analysis(data = test_data_1, type = "burnett", where = "samples", what = random_what[10], testing = TRUE)
actual_71 <- do_outlier_analysis(data = test_data_1, type = "3sd", where = "samples", what = random_what[11], testing = TRUE)
actual_81 <- do_outlier_analysis(data = test_data_1, type = "5cv", where = "samples", what = random_what[12], testing = TRUE)

actual_52 <- do_outlier_analysis(data = test_data_2, type = "qrange", where = "samples", what = random_what[13], testing = TRUE)
actual_62 <- do_outlier_analysis(data = test_data_2, type = "burnett", where = "samples", what = random_what[14], testing = TRUE)
actual_72 <- do_outlier_analysis(data = test_data_2, type = "3sd", where = "samples", what = random_what[15], testing = TRUE)
actual_82 <- do_outlier_analysis(data = test_data_2, type = "5cv", where = "samples", what = random_what[16], testing = TRUE)


test_that(desc = "Location tests", code = {

  expect_equal(object = actual_11, expected = list(type = "qrange", where = "replicates"))
  expect_equal(object = actual_21, expected = list(type = "burnett", where = "replicates"))
  expect_equal(object = actual_31, expected = list(type = "3sd", where = "replicates"))
  expect_equal(object = actual_41, expected = list(type = "5cv", where = "replicates"))

  expect_equal(object = actual_12, expected = list(type = "qrange", where = "replicates"))
  expect_equal(object = actual_22, expected = list(type = "burnett", where = "replicates"))
  expect_equal(object = actual_32, expected = list(type = "3sd", where = "replicates"))
  expect_equal(object = actual_42, expected = list(type = "5cv", where = "replicates"))

  expect_equal(object = actual_51, expected = list(type = "qrange", where = "samples"))
  expect_equal(object = actual_61, expected = list(type = "burnett", where = "samples"))
  expect_equal(object = actual_71, expected = list(type = "3sd", where = "samples"))
  expect_equal(object = actual_81, expected = list(type = "5cv", where = "samples"))

  expect_equal(object = actual_52, expected = list(type = "qrange", where = "samples"))
  expect_equal(object = actual_62, expected = list(type = "burnett", where = "samples"))
  expect_equal(object = actual_72, expected = list(type = "3sd", where = "samples"))
  expect_equal(object = actual_82, expected = list(type = "5cv", where = "samples"))

})

# Free memory by throwing stuff in the garbage!
rm("actual_11", "actual_21", "actual_31", "actual_41")
rm("actual_12", "actual_22", "actual_32", "actual_42")
rm("actual_51", "actual_61", "actual_71", "actual_81")
rm("actual_52", "actual_62", "actual_72", "actual_82")


# # # TEST 2 # # #
# # Testing dimensions # #

expected_dimensions_raw_1 <- test_data_1[, list(nrows = length(unique(SampleID))), by = comparison]
expected_dimensions_raw_1[,ncols := 4L]

expected_dimensions_notraw_1 <- test_data_1[, list(nrows = length(unique(SampleID))), by = comparison]
expected_dimensions_notraw_1[,ncols := 3L]

expected_dimensions_qrange_samples_raw_1 <- test_data_1[, list(nrows = 1), by = comparison]
expected_dimensions_qrange_samples_raw_1[, ncols := 3L]

expected_dimensions_qrange_samples_notraw_1 <- test_data_1[, list(nrows = 1), by = comparison]
expected_dimensions_qrange_samples_notraw_1[, ncols := 2L]

expected_dimensions_raw_2 <- test_data_2[, list(nrows = length(unique(SampleID))), by = comparison]
expected_dimensions_raw_2[,ncols := 4]

expected_dimensions_notraw_2 <- test_data_2[, list(nrows = length(unique(SampleID))), by = comparison]
expected_dimensions_notraw_2[,ncols := 3]

expected_dimensions_qrange_samples_raw_2 <- test_data_2[, list(nrows = 1), by = comparison]
expected_dimensions_qrange_samples_raw_2[, ncols := 3]

expected_dimensions_qrange_samples_notraw_2 <- test_data_2[, list(nrows = 1), by = comparison]
expected_dimensions_qrange_samples_notraw_2[, ncols := 2]


actual_11 <- do_outlier_analysis(data = test_data_1, type = "qrange", where = "replicates", what = "raw", testing = FALSE)
actual_nrows_11 <- lapply(actual_11, function(x) data.table(nrows = nrow(x))) |> rbindlist(idcol = "comparison")
actual_ncols_11 <- lapply(actual_11, function(x) data.table(ncols = ncol(x))) |> rbindlist(idcol = "comparison")
actual_dimensions_11 <- merge.data.table(x = actual_nrows_11, y = actual_ncols_11, by = "comparison", sort = FALSE)

actual_21 <- do_outlier_analysis(data = test_data_1, type = "burnett", where = "samples", what = "ld", testing = FALSE)
actual_nrows_21 <- lapply(actual_21, function(x) data.table(nrows = nrow(x))) |> rbindlist(idcol = "comparison")
actual_ncols_21 <- lapply(actual_21, function(x) data.table(ncols = ncol(x))) |> rbindlist(idcol = "comparison")
actual_dimensions_21 <- merge.data.table(x = actual_nrows_21, y = actual_ncols_21, by = "comparison", sort = FALSE)

actual_31 <- suppressWarnings(do_outlier_analysis(data = test_data_1, type = "3sd", where = "replicates", what = "ad", testing = FALSE))
actual_nrows_31 <- lapply(actual_31, function(x) data.table(nrows = nrow(x))) |> rbindlist(idcol = "comparison")
actual_ncols_31 <- lapply(actual_31, function(x) data.table(ncols = ncol(x))) |> rbindlist(idcol = "comparison")
actual_dimensions_31 <- merge.data.table(x = actual_nrows_31, y = actual_ncols_31, by = "comparison", sort = FALSE)

actual_12 <- do_outlier_analysis(data = test_data_2, type = "qrange", where = "samples", what = "rd", testing = FALSE)
actual_nrows_12 <- lapply(actual_12, function(x) data.table(nrows = nrow(x))) |> rbindlist(idcol = "comparison")
actual_ncols_12 <- lapply(actual_12, function(x) data.table(ncols = ncol(x))) |> rbindlist(idcol = "comparison")
actual_dimensions_12 <- merge.data.table(x = actual_nrows_12, y = actual_ncols_12, by = "comparison", sort = FALSE)

actual_22 <- do_outlier_analysis(data = test_data_2, type = "burnett", where = "replicates", what = "ad", testing = FALSE)
actual_nrows_22 <- lapply(actual_22, function(x) data.table(nrows = nrow(x))) |> rbindlist(idcol = "comparison")
actual_ncols_22 <- lapply(actual_22, function(x) data.table(ncols = ncol(x))) |> rbindlist(idcol = "comparison")
actual_dimensions_22 <- merge.data.table(x = actual_nrows_22, y = actual_ncols_22, by = "comparison", sort = FALSE)

actual_32 <- do_outlier_analysis(data = test_data_2, type = "3sd", where = "samples", what = "raw", testing = FALSE)
actual_nrows_32 <- lapply(actual_32, function(x) data.table(nrows = nrow(x))) |> rbindlist(idcol = "comparison")
actual_ncols_32 <- lapply(actual_32, function(x) data.table(ncols = ncol(x))) |> rbindlist(idcol = "comparison")
actual_dimensions_32 <- merge.data.table(x = actual_nrows_32, y = actual_ncols_32, by = "comparison", sort = FALSE)

test_that(desc = "Testing dimensions of outlier analysis output", code = {
  expect_equal(object = actual_dimensions_11, expected = expected_dimensions_raw_1)
  expect_equal(object = actual_dimensions_21, expected = expected_dimensions_notraw_1)
  expect_equal(object = actual_dimensions_31, expected = expected_dimensions_notraw_1)
  expect_equal(object = actual_dimensions_12, expected = expected_dimensions_qrange_samples_notraw_2)
  expect_equal(object = actual_dimensions_22, expected = expected_dimensions_notraw_2)
  expect_equal(object = actual_dimensions_32, expected = expected_dimensions_raw_2)
})

rm(list=setdiff(ls(), c("test_data_1", "test_data_2")))


# # # Test 3 # # #
# # # Testing errors # # #

names(test_data_1)[1] <- "notcomparison"
test_that(desc = "testing if error is thrown if 'comparion' is not part of data", code = {
  expect_error(object = do_outlier_analysis(data = test_data_1))
})
names(test_data_1)[1] <- "comparison"
names(test_data_1)[2] <- "notSampleID"
test_that(desc = "testing if error is thrown if 'SampleID' is not part of data", code = {
  expect_error(object = do_outlier_analysis(data = test_data_1))
})
names(test_data_1)[2] <- "SampleID"
names(test_data_1)[3] <- "notReplicateID"
test_that(desc = "testing if error is thrown if 'ReplicateID' is not part of data", code = {
  expect_error(object = do_outlier_analysis(data = test_data_1))
})
names(test_data_1)[3] <- "ReplicateID"

# # # Test 4 # # #
# # # Testing safety nets # # #

test_that(desc = "Testing whether passing wrong inputs produce messages if silence < 1L", code = {
  expect_message(object = do_outlier_analysis(data = test_data_1,
                                              type = "brunette",
                                              where = "replicates",
                                              what = "raw",
                                              silence = 0L,
                                              testing = T), regexp = "brunette")
  expect_message(object = do_outlier_analysis(data = test_data_1,
                                              type = "3sd",
                                              where = "samples",
                                              what = NA_integer_,
                                              silence = 0L,
                                              testing = T), regexp = "NA")
  expect_message(object = do_outlier_analysis(data = test_data_1,
                                              type = "5cv",
                                              where = NULL,
                                              what = "rd",
                                              silence = 0L,
                                              testing = T), regexp = "NULL")
})

expected_1 <- do_outlier_analysis(data = test_data_1,
                                  type = "burnett",
                                  where = "replicates",
                                  what = "raw",
                                  testing = TRUE)

expected_2 <- do_outlier_analysis(data = test_data_2,
                                  type = "3sd",
                                  where = "replicates",
                                  what = "raw",
                                  testing = TRUE)

expected_3 <- do_outlier_analysis(data = test_data_2,
                                  type = "burnett",
                                  where = "samples",
                                  what = "raw",
                                  testing = TRUE)

expected_4 <- do_outlier_analysis(data = test_data_1,
                                  type = "qrange",
                                  where = "replicates",
                                  what = "rd",
                                  testing = TRUE)

expected_5 <- do_outlier_analysis(data = test_data_2,
                                  type = "burnett",
                                  where = "replicates",
                                  what = "rd",
                                  testing = TRUE)

actual_1 <- do_outlier_analysis(data = test_data_1,
                                type = NA,
                                where = NA,
                                what = NA,
                                testing = TRUE)

actual_2 <- do_outlier_analysis(data = test_data_2,
                                type = "3sd",
                                where = NA,
                                what = NA,
                                testing = TRUE)

actual_3 <- do_outlier_analysis(data = test_data_2,
                                type = NULL,
                                where = "samples",
                                what = NA,
                                testing = TRUE)

actual_4 <- do_outlier_analysis(data = test_data_1,
                                type = "qrange",
                                where = "somethingcompletelywrong",
                                what = "rd",
                                testing = TRUE)

actual_5 <- do_outlier_analysis(data = test_data_2,
                                type = logical(10),
                                where = "somethingcompletelywrong",
                                what = "rd",
                                testing = TRUE)

test_that(desc = "Testing whether we get the correct output after passing wrong arguments", code = {
  expect_equal(object = actual_1, expected = expected_1)
  expect_equal(object = actual_2, expected = expected_2)
  expect_equal(object = actual_3, expected = expected_3)
  expect_equal(object = actual_4, expected = expected_4)
  expect_equal(object = actual_5, expected = expected_5)
})

rm(list=setdiff(ls(), c("test_data_1", "test_data_2")))

# # # Test 5 # # #
# # # Testing other stuff # # #

actual_1 <- do_outlier_analysis(data = test_data_1, simplify = TRUE)
actual_2 <- do_outlier_analysis(data = test_data_2, simplify = TRUE)

test_that(desc = "Testing 'simplify' functionality", code = {
  expect_true(object = all(unique(actual_1$comparison) == unique(test_data_1$comparison)))
  expect_true(object = all(unique(actual_2$comparison) == unique(test_data_2$comparison)))
})

rm(list = ls())
