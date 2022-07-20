library(testthat)
library(commutability)
library(readxl)
library(data.table)
library(fasteqa)

T_EPK2020_CS <- setDT(read_excel("~/datasets to be tested on/W..EPK2020_CS.xlsx"))
T_HB2020_CS <- setDT(read_excel("~/datasets to be tested on/W..HB2020_CS.xlsx"))
T_LPK2020_CS <- setDT(read_excel("~/datasets to be tested on/W..LPK2020_CS.xlsx"))
T_MCV2020_CS <- setDT(read_excel("~/datasets to be tested on/W..MCV2020_CS.xlsx"))
T_TPK2020_CS <- setDT(read_excel("~/datasets to be tested on/W..TPK2020_CS.xlsx"))
T_HDLCXXX_CS <- setDT(read_excel("~/datasets to be tested on/HDLC.CS.FILTERED.xlsx"))

T_EPK2020_CS <- MS_wise(data = T_EPK2020_CS)
T_HB2020_CS <- MS_wise(data = T_HB2020_CS)
T_LPK2020_CS <- MS_wise(data = T_LPK2020_CS)
T_MCV2020_CS <- MS_wise(data = T_TPK2020_CS)
T_HDLCXXX_CS <- MS_wise(data = T_HDLCXXX_CS)

A_EPK2020_CS <- estimate_zeta_data(data = T_EPK2020_CS, type = "percentile", zeta_critical = 2.22)
A_HB2020_CS <- estimate_zeta_data(data = T_HB2020_CS, type = "percentile", zeta_critical = 2.22)
A_LPK2020_CS <- estimate_zeta_data(data = T_LPK2020_CS, type = "percentile", zeta_critical = 2.22)
A_MCV2020_CS <- estimate_zeta_data(data = T_MCV2020_CS, type = "percentile", zeta_critical = 2.22)
A_HDLCXXX_CS <- estimate_zeta_data(data = T_HDLCXXX_CS, type = "percentile", zeta_critical = 2.22)

test_that(desc = "Testing for valid names", code = {
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


