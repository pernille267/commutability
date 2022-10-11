library(testthat)
library(commutability)
library(readxl)
library(data.table)
library(fasteqa)

set.seed(1)
test_data_1 <- read_excel(path = "~/datasets to be tested on/test_data_1.xlsx")
test_data_2 <- read_excel(path = "~/datasets to be tested on/test_data_2.xlsx")
test_data_3 <- read_excel(path = "~/datasets to be tested on/test_data_3.xlsx")
test_data_4 <- read_excel(path = "~/datasets to be tested on/test_data_4.xlsx")
test_data_5 <- read_excel(path = "~/datasets to be tested on/test_data_5.xlsx")
test_data_6 <- read_excel(path = "~/datasets to be tested on/test_data_6.xlsx")
test_data_7 <- read_excel(path = "~/datasets to be tested on/test_data_7.xlsx")

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
test_data_repaired_6 <- repair_data(data = test_data_6, data_check = check_data_6) |> MS_wise() |> na.omit()
test_data_repaired_7 <- repair_data(data = test_data_7, data_check = check_data_7) |> MS_wise() |> na.omit()


method <- "fg"

actual_1 <- perform_assessment_tests(data = test_data_repaired_1, B = 0, method = method, level = 0.95, include_rejection_rates = TRUE, simultaneous_testing = TRUE, silence = 1L)
actual_2 <- perform_assessment_tests(data = test_data_repaired_2, B = 0, method = method, level = 0.95, include_rejection_rates = TRUE, simultaneous_testing = TRUE, silence = 1L)
actual_3 <- perform_assessment_tests(data = test_data_repaired_3, B = 0, method = method, level = 0.95, include_rejection_rates = TRUE, simultaneous_testing = TRUE, silence = 1L)
actual_4 <- perform_assessment_tests(data = test_data_repaired_4, B = 0, method = method, level = 0.95, include_rejection_rates = TRUE, simultaneous_testing = TRUE, silence = 1L)
actual_5 <- perform_assessment_tests(data = test_data_repaired_5, B = 0, method = method, level = 0.95, include_rejection_rates = TRUE, simultaneous_testing = TRUE, silence = 1L)
actual_6 <- perform_assessment_tests(data = test_data_repaired_6, B = 0, method = method, level = 0.95, include_rejection_rates = TRUE, simultaneous_testing = TRUE, silence = 1L)
actual_7 <- perform_assessment_tests(data = test_data_repaired_7, B = 0, method = method, level = 0.95, include_rejection_rates = TRUE, simultaneous_testing = TRUE, silence = 1L)

expected_number_comparisons_1 <- unique(test_data_repaired_1$comparison) |> length()
expected_number_comparisons_2 <- unique(test_data_repaired_2$comparison) |> length()
expected_number_comparisons_3 <- unique(test_data_repaired_3$comparison) |> length()
expected_number_comparisons_4 <- unique(test_data_repaired_4$comparison) |> length()
expected_number_comparisons_5 <- unique(test_data_repaired_5$comparison) |> length()
expected_number_comparisons_6 <- unique(test_data_repaired_6$comparison) |> length()
expected_number_comparisons_7 <- unique(test_data_repaired_7$comparison) |> length()


test_that(desc = "Testing output types for B = 0 and method = fg", code = {

  expect_setequal(names(actual_1), c("comparison", "test_name", "test", "p.value", "conclusion"))
  expect_setequal(names(actual_2), c("comparison", "test_name", "test", "p.value", "conclusion"))
  expect_setequal(names(actual_3), c("comparison", "test_name", "test", "p.value", "conclusion"))
  expect_setequal(names(actual_4), c("comparison", "test_name", "test", "p.value", "conclusion"))
  expect_setequal(names(actual_5), c("comparison", "test_name", "test", "p.value", "conclusion"))
  expect_setequal(names(actual_6), c("comparison", "test_name", "test", "p.value", "conclusion"))
  expect_setequal(names(actual_7), c("comparison", "test_name", "test", "p.value", "conclusion"))

  expect_setequal(sapply(actual_1, FUN = class) |> unname(), rep("character", 5))
  expect_setequal(sapply(actual_2, FUN = class) |> unname(), rep("character", 5))
  expect_setequal(sapply(actual_3, FUN = class) |> unname(), rep("character", 5))
  expect_setequal(sapply(actual_4, FUN = class) |> unname(), rep("character", 5))
  expect_setequal(sapply(actual_5, FUN = class) |> unname(), rep("character", 5))
  expect_setequal(sapply(actual_6, FUN = class) |> unname(), rep("character", 5))
  expect_setequal(sapply(actual_7, FUN = class) |> unname(), rep("character", 5))

  expect_length(unique(actual_1$test), 2)
  expect_length(unique(actual_2$test), 2)
  expect_length(unique(actual_3$test), 2)
  expect_length(unique(actual_4$test), 2)
  expect_length(unique(actual_5$test), 2)
  expect_length(unique(actual_6$test), 2)
  expect_length(unique(actual_7$test), 2)

  expect_true((unique(actual_1$conclusion) |> length()) >= 2 &
                (unique(actual_1$conclusion) |> length()) <= 4)
  expect_true((unique(actual_2$conclusion) |> length()) >= 2 &
                (unique(actual_2$conclusion) |> length()) <= 4)
  expect_true((unique(actual_3$conclusion) |> length()) >= 2 &
                (unique(actual_3$conclusion) |> length()) <= 4)
  expect_true((unique(actual_4$conclusion) |> length()) >= 2 &
                (unique(actual_4$conclusion) |> length()) <= 4)
  expect_true((unique(actual_5$conclusion) |> length()) >= 2 &
                (unique(actual_5$conclusion) |> length()) <= 4)
  expect_true((unique(actual_6$conclusion) |> length()) >= 2 &
                (unique(actual_6$conclusion) |> length()) <= 4)
  expect_true((unique(actual_7$conclusion) |> length()) >= 2 &
                (unique(actual_7$conclusion) |> length()) <= 4)

  expect_equal(object = length(unique(actual_1$comparison)), expected_number_comparisons_1)
  expect_equal(object = length(unique(actual_2$comparison)), expected_number_comparisons_2)
  expect_equal(object = length(unique(actual_3$comparison)), expected_number_comparisons_3)
  expect_equal(object = length(unique(actual_4$comparison)), expected_number_comparisons_4)
  expect_equal(object = length(unique(actual_5$comparison)), expected_number_comparisons_5)
  expect_equal(object = length(unique(actual_6$comparison)), expected_number_comparisons_6)
  expect_equal(object = length(unique(actual_7$comparison)), expected_number_comparisons_7)

})

method <- "clsi"

actual_1 <- perform_assessment_tests(data = test_data_repaired_1, B = 0, method = method, level = 0.95, include_rejection_rates = TRUE, simultaneous_testing = TRUE, silence = 1L)
actual_2 <- perform_assessment_tests(data = test_data_repaired_2, B = 0, method = method, level = 0.95, include_rejection_rates = TRUE, simultaneous_testing = TRUE, silence = 1L)
actual_3 <- perform_assessment_tests(data = test_data_repaired_3, B = 0, method = method, level = 0.95, include_rejection_rates = TRUE, simultaneous_testing = TRUE, silence = 1L)
actual_4 <- perform_assessment_tests(data = test_data_repaired_4, B = 0, method = method, level = 0.95, include_rejection_rates = TRUE, simultaneous_testing = TRUE, silence = 1L)
actual_5 <- perform_assessment_tests(data = test_data_repaired_5, B = 0, method = method, level = 0.95, include_rejection_rates = TRUE, simultaneous_testing = TRUE, silence = 1L)
actual_6 <- perform_assessment_tests(data = test_data_repaired_6, B = 0, method = method, level = 0.95, include_rejection_rates = TRUE, simultaneous_testing = TRUE, silence = 1L)
actual_7 <- perform_assessment_tests(data = test_data_repaired_7, B = 0, method = method, level = 0.95, include_rejection_rates = TRUE, simultaneous_testing = TRUE, silence = 1L)

expected_number_comparisons_1 <- unique(test_data_repaired_1$comparison) |> length()
expected_number_comparisons_2 <- unique(test_data_repaired_2$comparison) |> length()
expected_number_comparisons_3 <- unique(test_data_repaired_3$comparison) |> length()
expected_number_comparisons_4 <- unique(test_data_repaired_4$comparison) |> length()
expected_number_comparisons_5 <- unique(test_data_repaired_5$comparison) |> length()
expected_number_comparisons_6 <- unique(test_data_repaired_6$comparison) |> length()
expected_number_comparisons_7 <- unique(test_data_repaired_7$comparison) |> length()


test_that(desc = "Testing output types for B = 0 and method = clsi", code = {

  expect_setequal(names(actual_1), c("comparison", "test_name", "test", "p.value", "conclusion"))
  expect_setequal(names(actual_2), c("comparison", "test_name", "test", "p.value", "conclusion"))
  expect_setequal(names(actual_3), c("comparison", "test_name", "test", "p.value", "conclusion"))
  expect_setequal(names(actual_4), c("comparison", "test_name", "test", "p.value", "conclusion"))
  expect_setequal(names(actual_5), c("comparison", "test_name", "test", "p.value", "conclusion"))
  expect_setequal(names(actual_6), c("comparison", "test_name", "test", "p.value", "conclusion"))
  expect_setequal(names(actual_7), c("comparison", "test_name", "test", "p.value", "conclusion"))

  expect_setequal(sapply(actual_1, FUN = class) |> unname(), rep("character", 5))
  expect_setequal(sapply(actual_2, FUN = class) |> unname(), rep("character", 5))
  expect_setequal(sapply(actual_3, FUN = class) |> unname(), rep("character", 5))
  expect_setequal(sapply(actual_4, FUN = class) |> unname(), rep("character", 5))
  expect_setequal(sapply(actual_5, FUN = class) |> unname(), rep("character", 5))
  expect_setequal(sapply(actual_6, FUN = class) |> unname(), rep("character", 5))
  expect_setequal(sapply(actual_7, FUN = class) |> unname(), rep("character", 5))

  expect_length(unique(actual_1$test), 2)
  expect_length(unique(actual_2$test), 2)
  expect_length(unique(actual_3$test), 2)
  expect_length(unique(actual_4$test), 2)
  expect_length(unique(actual_5$test), 2)
  expect_length(unique(actual_6$test), 2)
  expect_length(unique(actual_7$test), 2)

  expect_true((unique(actual_1$conclusion) |> length()) >= 2 &
                (unique(actual_1$conclusion) |> length()) <= 4)
  expect_true((unique(actual_2$conclusion) |> length()) >= 2 &
                (unique(actual_2$conclusion) |> length()) <= 4)
  expect_true((unique(actual_3$conclusion) |> length()) >= 2 &
                (unique(actual_3$conclusion) |> length()) <= 4)
  expect_true((unique(actual_4$conclusion) |> length()) >= 2 &
                (unique(actual_4$conclusion) |> length()) <= 4)
  expect_true((unique(actual_5$conclusion) |> length()) >= 2 &
                (unique(actual_5$conclusion) |> length()) <= 4)
  expect_true((unique(actual_6$conclusion) |> length()) >= 2 &
                (unique(actual_6$conclusion) |> length()) <= 4)
  expect_true((unique(actual_7$conclusion) |> length()) >= 2 &
                (unique(actual_7$conclusion) |> length()) <= 4)

  expect_equal(object = length(unique(actual_1$comparison)), expected_number_comparisons_1)
  expect_equal(object = length(unique(actual_2$comparison)), expected_number_comparisons_2)
  expect_equal(object = length(unique(actual_3$comparison)), expected_number_comparisons_3)
  expect_equal(object = length(unique(actual_4$comparison)), expected_number_comparisons_4)
  expect_equal(object = length(unique(actual_5$comparison)), expected_number_comparisons_5)
  expect_equal(object = length(unique(actual_6$comparison)), expected_number_comparisons_6)
  expect_equal(object = length(unique(actual_7$comparison)), expected_number_comparisons_7)

})

method <- "ols"

actual_1 <- perform_assessment_tests(data = test_data_repaired_1, B = 0, method = method, level = 0.95, include_rejection_rates = TRUE, simultaneous_testing = TRUE, silence = 1L)
actual_2 <- perform_assessment_tests(data = test_data_repaired_2, B = 0, method = method, level = 0.95, include_rejection_rates = TRUE, simultaneous_testing = TRUE, silence = 1L)
actual_3 <- perform_assessment_tests(data = test_data_repaired_3, B = 0, method = method, level = 0.95, include_rejection_rates = TRUE, simultaneous_testing = TRUE, silence = 1L)
actual_4 <- perform_assessment_tests(data = test_data_repaired_4, B = 0, method = method, level = 0.95, include_rejection_rates = TRUE, simultaneous_testing = TRUE, silence = 1L)
actual_5 <- perform_assessment_tests(data = test_data_repaired_5, B = 0, method = method, level = 0.95, include_rejection_rates = TRUE, simultaneous_testing = TRUE, silence = 1L)
actual_6 <- perform_assessment_tests(data = test_data_repaired_6, B = 0, method = method, level = 0.95, include_rejection_rates = TRUE, simultaneous_testing = TRUE, silence = 1L)
actual_7 <- perform_assessment_tests(data = test_data_repaired_7, B = 0, method = method, level = 0.95, include_rejection_rates = TRUE, simultaneous_testing = TRUE, silence = 1L)

expected_number_comparisons_1 <- unique(test_data_repaired_1$comparison) |> length()
expected_number_comparisons_2 <- unique(test_data_repaired_2$comparison) |> length()
expected_number_comparisons_3 <- unique(test_data_repaired_3$comparison) |> length()
expected_number_comparisons_4 <- unique(test_data_repaired_4$comparison) |> length()
expected_number_comparisons_5 <- unique(test_data_repaired_5$comparison) |> length()
expected_number_comparisons_6 <- unique(test_data_repaired_6$comparison) |> length()
expected_number_comparisons_7 <- unique(test_data_repaired_7$comparison) |> length()


test_that(desc = "Testing output types for B = 0 and method = ols", code = {

  expect_setequal(names(actual_1), c("comparison", "test_name", "test", "p.value", "conclusion"))
  expect_setequal(names(actual_2), c("comparison", "test_name", "test", "p.value", "conclusion"))
  expect_setequal(names(actual_3), c("comparison", "test_name", "test", "p.value", "conclusion"))
  expect_setequal(names(actual_4), c("comparison", "test_name", "test", "p.value", "conclusion"))
  expect_setequal(names(actual_5), c("comparison", "test_name", "test", "p.value", "conclusion"))
  expect_setequal(names(actual_6), c("comparison", "test_name", "test", "p.value", "conclusion"))
  expect_setequal(names(actual_7), c("comparison", "test_name", "test", "p.value", "conclusion"))

  expect_setequal(sapply(actual_1, FUN = class) |> unname(), rep("character", 5))
  expect_setequal(sapply(actual_2, FUN = class) |> unname(), rep("character", 5))
  expect_setequal(sapply(actual_3, FUN = class) |> unname(), rep("character", 5))
  expect_setequal(sapply(actual_4, FUN = class) |> unname(), rep("character", 5))
  expect_setequal(sapply(actual_5, FUN = class) |> unname(), rep("character", 5))
  expect_setequal(sapply(actual_6, FUN = class) |> unname(), rep("character", 5))
  expect_setequal(sapply(actual_7, FUN = class) |> unname(), rep("character", 5))

  expect_length(unique(actual_1$test), 2)
  expect_length(unique(actual_2$test), 2)
  expect_length(unique(actual_3$test), 2)
  expect_length(unique(actual_4$test), 2)
  expect_length(unique(actual_5$test), 2)
  expect_length(unique(actual_6$test), 2)
  expect_length(unique(actual_7$test), 2)

  expect_true((unique(actual_1$conclusion) |> length()) >= 2 &
                (unique(actual_1$conclusion) |> length()) <= 4)
  expect_true((unique(actual_2$conclusion) |> length()) >= 2 &
                (unique(actual_2$conclusion) |> length()) <= 4)
  expect_true((unique(actual_3$conclusion) |> length()) >= 2 &
                (unique(actual_3$conclusion) |> length()) <= 4)
  expect_true((unique(actual_4$conclusion) |> length()) >= 2 &
                (unique(actual_4$conclusion) |> length()) <= 4)
  expect_true((unique(actual_5$conclusion) |> length()) >= 2 &
                (unique(actual_5$conclusion) |> length()) <= 4)
  expect_true((unique(actual_6$conclusion) |> length()) >= 2 &
                (unique(actual_6$conclusion) |> length()) <= 4)
  expect_true((unique(actual_7$conclusion) |> length()) >= 2 &
                (unique(actual_7$conclusion) |> length()) <= 4)

  expect_equal(object = length(unique(actual_1$comparison)), expected_number_comparisons_1)
  expect_equal(object = length(unique(actual_2$comparison)), expected_number_comparisons_2)
  expect_equal(object = length(unique(actual_3$comparison)), expected_number_comparisons_3)
  expect_equal(object = length(unique(actual_4$comparison)), expected_number_comparisons_4)
  expect_equal(object = length(unique(actual_5$comparison)), expected_number_comparisons_5)
  expect_equal(object = length(unique(actual_6$comparison)), expected_number_comparisons_6)
  expect_equal(object = length(unique(actual_7$comparison)), expected_number_comparisons_7)

})

method <- "fg"

actual_1 <- perform_assessment_tests(data = test_data_repaired_1, B = 10, method = method, level = 0.95, include_rejection_rates = TRUE, simultaneous_testing = TRUE, silence = 1L)
actual_2 <- perform_assessment_tests(data = test_data_repaired_2, B = 10, method = method, level = 0.95, include_rejection_rates = TRUE, simultaneous_testing = TRUE, silence = 1L)
actual_3 <- perform_assessment_tests(data = test_data_repaired_3, B = 10, method = method, level = 0.95, include_rejection_rates = TRUE, simultaneous_testing = TRUE, silence = 1L)
actual_4 <- perform_assessment_tests(data = test_data_repaired_4, B = 10, method = method, level = 0.95, include_rejection_rates = TRUE, simultaneous_testing = TRUE, silence = 1L)
actual_5 <- perform_assessment_tests(data = test_data_repaired_5, B = 10, method = method, level = 0.95, include_rejection_rates = TRUE, simultaneous_testing = TRUE, silence = 1L)
actual_6 <- perform_assessment_tests(data = test_data_repaired_6, B = 10, method = method, level = 0.95, include_rejection_rates = TRUE, simultaneous_testing = TRUE, silence = 1L)
actual_7 <- perform_assessment_tests(data = test_data_repaired_7, B = 10, method = method, level = 0.95, include_rejection_rates = TRUE, simultaneous_testing = TRUE, silence = 1L)

expected_number_comparisons_1 <- unique(test_data_repaired_1$comparison) |> length()
expected_number_comparisons_2 <- unique(test_data_repaired_2$comparison) |> length()
expected_number_comparisons_3 <- unique(test_data_repaired_3$comparison) |> length()
expected_number_comparisons_4 <- unique(test_data_repaired_4$comparison) |> length()
expected_number_comparisons_5 <- unique(test_data_repaired_5$comparison) |> length()
expected_number_comparisons_6 <- unique(test_data_repaired_6$comparison) |> length()
expected_number_comparisons_7 <- unique(test_data_repaired_7$comparison) |> length()


test_that(desc = "Testing output types for B >= 1 and fg", code = {

  expect_setequal(names(actual_1), c("comparison", "test_name", "test", "p.value", "conclusion", "rejection_rate"))
  expect_setequal(names(actual_2), c("comparison", "test_name", "test", "p.value", "conclusion", "rejection_rate"))
  expect_setequal(names(actual_3), c("comparison", "test_name", "test", "p.value", "conclusion", "rejection_rate"))
  expect_setequal(names(actual_4), c("comparison", "test_name", "test", "p.value", "conclusion", "rejection_rate"))
  expect_setequal(names(actual_5), c("comparison", "test_name", "test", "p.value", "conclusion", "rejection_rate"))
  expect_setequal(names(actual_6), c("comparison", "test_name", "test", "p.value", "conclusion", "rejection_rate"))
  expect_setequal(names(actual_7), c("comparison", "test_name", "test", "p.value", "conclusion", "rejection_rate"))

  expect_setequal(sapply(actual_1, FUN = class) |> unname(), c(rep("character", 5), "numeric"))
  expect_setequal(sapply(actual_2, FUN = class) |> unname(), c(rep("character", 5), "numeric"))
  expect_setequal(sapply(actual_3, FUN = class) |> unname(), c(rep("character", 5), "numeric"))
  expect_setequal(sapply(actual_4, FUN = class) |> unname(), c(rep("character", 5), "numeric"))
  expect_setequal(sapply(actual_5, FUN = class) |> unname(), c(rep("character", 5), "numeric"))
  expect_setequal(sapply(actual_6, FUN = class) |> unname(), c(rep("character", 5), "numeric"))
  expect_setequal(sapply(actual_7, FUN = class) |> unname(), c(rep("character", 5), "numeric"))

  expect_true(all(actual_1$rejection_rate <= 1 & actual_1$rejection_rate >= 0))
  expect_true(all(actual_2$rejection_rate <= 1 & actual_2$rejection_rate >= 0))
  expect_true(all(actual_3$rejection_rate <= 1 & actual_3$rejection_rate >= 0))
  expect_true(all(actual_4$rejection_rate <= 1 & actual_4$rejection_rate >= 0))
  expect_true(all(actual_5$rejection_rate <= 1 & actual_5$rejection_rate >= 0))
  expect_true(all(actual_6$rejection_rate <= 1 & actual_6$rejection_rate >= 0))
  expect_true(all(actual_7$rejection_rate <= 1 & actual_7$rejection_rate >= 0))

  expect_true((unique(actual_1$conclusion) |> length()) >= 2 &
                (unique(actual_1$conclusion) |> length()) <= 4)
  expect_true((unique(actual_2$conclusion) |> length()) >= 2 &
                (unique(actual_2$conclusion) |> length()) <= 4)
  expect_true((unique(actual_3$conclusion) |> length()) >= 2 &
                (unique(actual_3$conclusion) |> length()) <= 4)
  expect_true((unique(actual_4$conclusion) |> length()) >= 2 &
                (unique(actual_4$conclusion) |> length()) <= 4)
  expect_true((unique(actual_5$conclusion) |> length()) >= 2 &
                (unique(actual_5$conclusion) |> length()) <= 4)
  expect_true((unique(actual_6$conclusion) |> length()) >= 2 &
                (unique(actual_6$conclusion) |> length()) <= 4)
  expect_true((unique(actual_7$conclusion) |> length()) >= 2 &
                (unique(actual_7$conclusion) |> length()) <= 4)

  expect_equal(object = length(unique(actual_1$comparison)), expected_number_comparisons_1)
  expect_equal(object = length(unique(actual_2$comparison)), expected_number_comparisons_2)
  expect_equal(object = length(unique(actual_3$comparison)), expected_number_comparisons_3)
  expect_equal(object = length(unique(actual_4$comparison)), expected_number_comparisons_4)
  expect_equal(object = length(unique(actual_5$comparison)), expected_number_comparisons_5)
  expect_equal(object = length(unique(actual_6$comparison)), expected_number_comparisons_6)
  expect_equal(object = length(unique(actual_7$comparison)), expected_number_comparisons_7)

})



