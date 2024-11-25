library(testthat)
library(commutability)
library(readxl)
suppressWarnings(library(data.table))
library(fasteqa)

T_EPK2020_CS <- setDT(read_excel("~/Packages/datasets to be tested on/W..EPK2020_CS.xlsx"))
T_HB2020_CS <- setDT(read_excel("~/Packages/datasets to be tested on/W..HB2020_CS.xlsx"))
T_LPK2020_CS <- setDT(read_excel("~/Packages/datasets to be tested on/W..LPK2020_CS.xlsx"))
T_MCV2020_CS <- setDT(read_excel("~/Packages/datasets to be tested on/W..MCV2020_CS.xlsx"))
T_TPK2020_CS <- setDT(read_excel("~/Packages/datasets to be tested on/W..TPK2020_CS.xlsx"))
T_HDLCXXX_CS <- setDT(read_excel("~/Packages/datasets to be tested on/HDLC.CS.FILTERED.xlsx"))

check_test_data_1 <- check_data(data = T_EPK2020_CS)
check_test_data_2 <- check_data(data = T_HB2020_CS)
check_test_data_3 <- check_data(data = T_LPK2020_CS)
check_test_data_4 <- check_data(data = T_MCV2020_CS)
check_test_data_5 <- check_data(data = T_HDLCXXX_CS)

T_EPK2020_CS <- MS_wise(data = repair_data(data = T_EPK2020_CS, data_check = check_test_data_1))
T_HB2020_CS <- MS_wise(data = repair_data(data = T_HB2020_CS, data_check = check_test_data_2))
T_LPK2020_CS <- MS_wise(data = repair_data(data = T_LPK2020_CS, data_check = check_test_data_3))
T_MCV2020_CS <- MS_wise(data = repair_data(data = T_MCV2020_CS, data_check = check_test_data_4))
T_HDLCXXX_CS <- MS_wise(data = repair_data(data = T_HDLCXXX_CS, data_check = check_test_data_5))


A_EPK2020_CS <- estimate_zeta_data(data = T_EPK2020_CS, type = "percentile", zeta_critical = 2.22, B = 200)
A_HB2020_CS <- estimate_zeta_data(data = T_HB2020_CS, type = "percentile", zeta_critical = 2.22, B = 200)
A_LPK2020_CS <- estimate_zeta_data(data = T_LPK2020_CS, type = "percentile", zeta_critical = 2.22, B = 200)
A_MCV2020_CS <- estimate_zeta_data(data = T_MCV2020_CS, type = "percentile", zeta_critical = 2.22, B = 200)
A_HDLCXXX_CS <- estimate_zeta_data(data = T_HDLCXXX_CS, type = "percentile", zeta_critical = 2.22, B = 200)

A2_EPK2020_CS <- estimate_zeta_data(data = T_EPK2020_CS, type = "percentile", zeta_critical = 2.22, B = NULL)
A2_HB2020_CS <- estimate_zeta_data(data = T_HB2020_CS, type = "percentile", zeta_critical = 2.22, B = NULL)
A2_LPK2020_CS <- estimate_zeta_data(data = T_LPK2020_CS, type = "percentile", zeta_critical = 2.22, B = NULL)
A2_MCV2020_CS <- estimate_zeta_data(data = T_MCV2020_CS, type = "percentile", zeta_critical = 2.22, B = NULL)
A2_HDLCXXX_CS <- estimate_zeta_data(data = T_HDLCXXX_CS, type = "percentile", zeta_critical = 2.22, B = NULL)

test_that(desc = "Testing for valid names for full output", code = {
  expect_named(object = A_EPK2020_CS, expected = c("comparison", "zeta", "lwr", "upr", "zeta_critical", "zeta_conclusion"), ignore.order = TRUE)
  expect_named(object = A_HB2020_CS, expected = c("comparison", "zeta", "lwr", "upr", "zeta_critical", "zeta_conclusion"), ignore.order = TRUE)
  expect_named(object = A_LPK2020_CS, expected = c("comparison", "zeta", "lwr", "upr", "zeta_critical", "zeta_conclusion"), ignore.order = TRUE)
  expect_named(object = A_MCV2020_CS, expected = c("comparison", "zeta", "lwr", "upr", "zeta_critical", "zeta_conclusion"), ignore.order = TRUE)
  expect_named(object = A_HDLCXXX_CS, expected = c("comparison", "zeta", "lwr", "upr", "zeta_critical", "zeta_conclusion"), ignore.order = TRUE)
})

test_that(desc = "Testing zeta > 0", code = {
  sapply(X = A_EPK2020_CS$zeta, FUN = function(x) expect_gt(object = x, expected = 0))
  sapply(X = A_HB2020_CS$zeta, FUN = function(x) expect_gt(object = x, expected = 0))
  sapply(X = A_LPK2020_CS$zeta, FUN = function(x) expect_gt(object = x, expected = 0))
  sapply(X = A_MCV2020_CS$zeta, FUN = function(x) expect_gt(object = x, expected = 0))
  sapply(X = A_HDLCXXX_CS$zeta, FUN = function(x) expect_gt(object = x, expected = 0))
})

test_that(desc = "upr > lwr", code = {
  expect_true(all(A_EPK2020_CS$lwr <= A_EPK2020_CS$upr))
  expect_true(all(A_HB2020_CS$lwr <= A_HB2020_CS$upr))
  expect_true(all(A_LPK2020_CS$lwr <= A_LPK2020_CS$upr))
  expect_true(all(A_MCV2020_CS$lwr <= A_MCV2020_CS$upr))
  expect_true(all(A_HDLCXXX_CS$lwr <= A_HDLCXXX_CS$upr))
})


test_that(desc = "Testing for valid names for incomplete output", code = {
  expect_named(object = A2_EPK2020_CS, expected = c("comparison", "zeta"), ignore.order = TRUE)
  expect_named(object = A2_HB2020_CS, expected = c("comparison", "zeta"), ignore.order = TRUE)
  expect_named(object = A2_LPK2020_CS, expected = c("comparison", "zeta"), ignore.order = TRUE)
  expect_named(object = A2_MCV2020_CS, expected = c("comparison", "zeta"), ignore.order = TRUE)
  expect_named(object = A2_HDLCXXX_CS, expected = c("comparison", "zeta"), ignore.order = TRUE)
})

test_that(desc = "Testing if the correct errors are thrown for EPK", code = {
  expect_error(object = estimate_zeta_data(data = T_EPK2020_CS, B = -1), regexp = "is a negative value")
  expect_error(object = estimate_zeta_data(data = T_EPK2020_CS, B = "invalid"), regexp = "is a character string")
  expect_error(object = estimate_zeta_data(data = T_EPK2020_CS, B = 51, M = NULL, N = NA, zeta_critical = NA), regexp = "but a 'logical' was provided")
  expect_error(object = estimate_zeta_data(data = T_EPK2020_CS, B = 51, M = NULL, N = NULL, zeta_critical = NA), regexp = "but a 'NULL' was provided")
  expect_error(object = estimate_zeta_data(data = T_EPK2020_CS, B = 51, M = NULL, N = "invalid", zeta_critical = NA), regexp = "is a character string")
  expect_error(object = estimate_zeta_data(data = T_EPK2020_CS, B = 51, M = NULL, N = -1, zeta_critical = NA), regexp = "is a negative value or zero")
  expect_error(object = estimate_zeta_data(data = T_EPK2020_CS, B = 51, M = NULL, N = 0, zeta_critical = NA), regexp = "is a negative value or zero")
  expect_error(object = estimate_zeta_data(data = T_EPK2020_CS, B = 51, M = NULL, N = TRUE, zeta_critical = NA), regexp = "is a logical value")
  expect_error(object = estimate_zeta_data(data = T_EPK2020_CS, B = 51, M = NULL, N = FALSE, zeta_critical = NA), regexp = "is a logical value")
  expect_error(object = estimate_zeta_data(data = T_EPK2020_CS, B = 51, M = NULL, N = 5, zeta_critical = NA), regexp = "'NULL'")
  expect_error(object = estimate_zeta_data(data = T_EPK2020_CS, B = 51, M = NA, N = 5, zeta_critical = NA), regexp = "'logical'")
  expect_error(object = estimate_zeta_data(data = T_EPK2020_CS, B = 51, M = TRUE, N = 5, zeta_critical = NA), regexp = "'logical'")
  expect_error(object = estimate_zeta_data(data = T_EPK2020_CS, B = 51, M = FALSE, N = 5, zeta_critical = NA), regexp = "'logical'")
  expect_error(object = estimate_zeta_data(data = T_EPK2020_CS, B = 51, M = "invalid", N = 5, zeta_critical = NA), regexp = "'character'")
  expect_error(object = estimate_zeta_data(data = T_EPK2020_CS, B = 51, M = -1, N = 5, zeta_critical = NA), regexp = "'numeric'")
})

test_that(desc = "Testing if the correct errors are thrown for LPK", code = {
  expect_error(object = estimate_zeta_data(data = T_LPK2020_CS, B = -1), regexp = "is a negative value")
  expect_error(object = estimate_zeta_data(data = T_LPK2020_CS, B = "invalid"), regexp = "is a character string")
  expect_error(object = estimate_zeta_data(data = T_LPK2020_CS, B = 51, M = NULL, N = NA, zeta_critical = NA), regexp = "but a 'logical' was provided")
  expect_error(object = estimate_zeta_data(data = T_LPK2020_CS, B = 51, M = NULL, N = NULL, zeta_critical = NA), regexp = "but a 'NULL' was provided")
  expect_error(object = estimate_zeta_data(data = T_LPK2020_CS, B = 51, M = NULL, N = "invalid", zeta_critical = NA), regexp = "is a character string")
  expect_error(object = estimate_zeta_data(data = T_LPK2020_CS, B = 51, M = NULL, N = -1, zeta_critical = NA), regexp = "is a negative value or zero")
  expect_error(object = estimate_zeta_data(data = T_LPK2020_CS, B = 51, M = NULL, N = 0, zeta_critical = NA), regexp = "is a negative value or zero")
  expect_error(object = estimate_zeta_data(data = T_LPK2020_CS, B = 51, M = NULL, N = TRUE, zeta_critical = NA), regexp = "is a logical value")
  expect_error(object = estimate_zeta_data(data = T_LPK2020_CS, B = 51, M = NULL, N = FALSE, zeta_critical = NA), regexp = "is a logical value")
  expect_error(object = estimate_zeta_data(data = T_LPK2020_CS, B = 51, M = NULL, N = 5, zeta_critical = NA), regexp = "'NULL'")
  expect_error(object = estimate_zeta_data(data = T_LPK2020_CS, B = 51, M = NA, N = 5, zeta_critical = NA), regexp = "'logical'")
  expect_error(object = estimate_zeta_data(data = T_LPK2020_CS, B = 51, M = TRUE, N = 5, zeta_critical = NA), regexp = "'logical'")
  expect_error(object = estimate_zeta_data(data = T_LPK2020_CS, B = 51, M = FALSE, N = 5, zeta_critical = NA), regexp = "'logical'")
  expect_error(object = estimate_zeta_data(data = T_LPK2020_CS, B = 51, M = "invalid", N = 5, zeta_critical = NA), regexp = "'character'")
  expect_error(object = estimate_zeta_data(data = T_LPK2020_CS, B = 51, M = -1, N = 5, zeta_critical = NA), regexp = "'numeric'")
})

test_that(desc = "Testing if the correct errors are thrown for MCV", code = {
  expect_error(object = estimate_zeta_data(data = T_MCV2020_CS, B = -1), regexp = "is a negative value")
  expect_error(object = estimate_zeta_data(data = T_MCV2020_CS, B = "invalid"), regexp = "is a character string")
  expect_error(object = estimate_zeta_data(data = T_MCV2020_CS, B = 51, M = NULL, N = NA, zeta_critical = NA), regexp = "but a 'logical' was provided")
  expect_error(object = estimate_zeta_data(data = T_MCV2020_CS, B = 51, M = NULL, N = NULL, zeta_critical = NA), regexp = "but a 'NULL' was provided")
  expect_error(object = estimate_zeta_data(data = T_MCV2020_CS, B = 51, M = NULL, N = "invalid", zeta_critical = NA), regexp = "is a character string")
  expect_error(object = estimate_zeta_data(data = T_MCV2020_CS, B = 51, M = NULL, N = -1, zeta_critical = NA), regexp = "is a negative value or zero")
  expect_error(object = estimate_zeta_data(data = T_MCV2020_CS, B = 51, M = NULL, N = 0, zeta_critical = NA), regexp = "is a negative value or zero")
  expect_error(object = estimate_zeta_data(data = T_MCV2020_CS, B = 51, M = NULL, N = TRUE, zeta_critical = NA), regexp = "is a logical value")
  expect_error(object = estimate_zeta_data(data = T_MCV2020_CS, B = 51, M = NULL, N = FALSE, zeta_critical = NA), regexp = "is a logical value")
  expect_error(object = estimate_zeta_data(data = T_MCV2020_CS, B = 51, M = NULL, N = 5, zeta_critical = NA), regexp = "'NULL'")
  expect_error(object = estimate_zeta_data(data = T_MCV2020_CS, B = 51, M = NA, N = 5, zeta_critical = NA), regexp = "'logical'")
  expect_error(object = estimate_zeta_data(data = T_MCV2020_CS, B = 51, M = TRUE, N = 5, zeta_critical = NA), regexp = "'logical'")
  expect_error(object = estimate_zeta_data(data = T_MCV2020_CS, B = 51, M = FALSE, N = 5, zeta_critical = NA), regexp = "'logical'")
  expect_error(object = estimate_zeta_data(data = T_MCV2020_CS, B = 51, M = "invalid", N = 5, zeta_critical = NA), regexp = "'character'")
  expect_error(object = estimate_zeta_data(data = T_MCV2020_CS, B = 51, M = -1, N = 5, zeta_critical = NA), regexp = "'numeric'")
})

test_that(desc = "Testing if the correct errors are thrown for HB", code = {
  expect_error(object = estimate_zeta_data(data = T_HB2020_CS, B = -1), regexp = "is a negative value")
  expect_error(object = estimate_zeta_data(data = T_HB2020_CS, B = "invalid"), regexp = "is a character string")
  expect_error(object = estimate_zeta_data(data = T_HB2020_CS, B = 51, M = NULL, N = NA, zeta_critical = NA), regexp = "but a 'logical' was provided")
  expect_error(object = estimate_zeta_data(data = T_HB2020_CS, B = 51, M = NULL, N = NULL, zeta_critical = NA), regexp = "but a 'NULL' was provided")
  expect_error(object = estimate_zeta_data(data = T_HB2020_CS, B = 51, M = NULL, N = "invalid", zeta_critical = NA), regexp = "is a character string")
  expect_error(object = estimate_zeta_data(data = T_HB2020_CS, B = 51, M = NULL, N = -1, zeta_critical = NA), regexp = "is a negative value or zero")
  expect_error(object = estimate_zeta_data(data = T_HB2020_CS, B = 51, M = NULL, N = 0, zeta_critical = NA), regexp = "is a negative value or zero")
  expect_error(object = estimate_zeta_data(data = T_HB2020_CS, B = 51, M = NULL, N = TRUE, zeta_critical = NA), regexp = "is a logical value")
  expect_error(object = estimate_zeta_data(data = T_HB2020_CS, B = 51, M = NULL, N = FALSE, zeta_critical = NA), regexp = "is a logical value")
  expect_error(object = estimate_zeta_data(data = T_HB2020_CS, B = 51, M = NULL, N = 5, zeta_critical = NA), regexp = "'NULL'")
  expect_error(object = estimate_zeta_data(data = T_HB2020_CS, B = 51, M = NA, N = 5, zeta_critical = NA), regexp = "'logical'")
  expect_error(object = estimate_zeta_data(data = T_HB2020_CS, B = 51, M = TRUE, N = 5, zeta_critical = NA), regexp = "'logical'")
  expect_error(object = estimate_zeta_data(data = T_HB2020_CS, B = 51, M = FALSE, N = 5, zeta_critical = NA), regexp = "'logical'")
  expect_error(object = estimate_zeta_data(data = T_HB2020_CS, B = 51, M = "invalid", N = 5, zeta_critical = NA), regexp = "'character'")
  expect_error(object = estimate_zeta_data(data = T_HB2020_CS, B = 51, M = -1, N = 5, zeta_critical = NA), regexp = "'numeric'")
})

test_that(desc = "Testing if correct warnings are thrown", code = {
  expect_warning(object = estimate_zeta_data(data = T_EPK2020_CS, B = 30L), regexp = "30")
  expect_warning(object = estimate_zeta_data(data = T_MCV2020_CS, B = 40L), regexp = "40")
  expect_warning(object = estimate_zeta_data(data = T_HB2020_CS, B = 45L), regexp = "45")
  expect_warning(object = estimate_zeta_data(data = T_LPK2020_CS, B = 49L), regexp = "49")
  expect_warning(object = estimate_zeta_data(data = T_HDLCXXX_CS, B = 49L), regexp = "49")

  expect_warning(object = estimate_zeta_data(data = T_EPK2020_CS, B = 51L, N = 0.3, M = 0.1, zeta_critical = NULL), regexp = "0.3")
  expect_warning(object = estimate_zeta_data(data = T_MCV2020_CS, B = 51L, N = 0.2, M = 0.1, zeta_critical = NULL), regexp = "0.2")
  expect_warning(object = estimate_zeta_data(data = T_HB2020_CS, B = 51L, N = 0.1, M = 0.1, zeta_critical = NULL), regexp = "0.1")
  expect_warning(object = estimate_zeta_data(data = T_LPK2020_CS, B = 51L, N = 0.4, M = 0.1, zeta_critical = NULL), regexp = "0.4")
  expect_warning(object = estimate_zeta_data(data = T_HDLCXXX_CS, B = 51L, N = 0.39, M = 0.1, zeta_critical = NULL), regexp = "0.39")
})


A3_EPK2020_CS <- estimate_zeta_data(data = T_EPK2020_CS, type = "normal", zeta_critical = 2.22, B = 200, level = 1 - 1e-6)
lwr_larger_than_zero <- A3_EPK2020_CS$lwr >= 0

test_that(desc = "Testing if lower bounds of bootstrap confidence intervals are truncated at zero", code = {
  sapply(lwr_larger_than_zero, expect_gte, expected = 0)
})

random_zeta_critical <- round(runif(5, 2, 6), 2L)

for(i in 1:5){
  A3_HDLCXXX_CS <- estimate_zeta_data(data = T_HDLCXXX_CS, type = "bca", zeta_critical = random_zeta_critical[i], B = 51, level = 0.90)
  A3_zeta_conclusion <- A3_HDLCXXX_CS$zeta_conclusion
  expected_zeta_conclusion <- as.integer(A3_HDLCXXX_CS$zeta >= random_zeta_critical[i])
  test_that(desc = "Testing if observed 'zeta_conclusion' corresponds with the expected conclusions", code = {
    expect_equal(object = A3_zeta_conclusion, expected = expected_zeta_conclusion)
  })
}
