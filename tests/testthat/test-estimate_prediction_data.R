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

check_data_1 <- check_data(test_data_1)
check_data_2 <- check_data(test_data_2)
check_data_3 <- check_data(test_data_3)
check_data_4 <- check_data(test_data_4)
check_data_5 <- check_data(test_data_5)
check_data_6 <- check_data(test_data_6)
check_data_7 <- check_data(test_data_7)

test_data_1 <- repair_data(data = test_data_1, check_data_1) |> MS_wise() |> na.omit()
test_data_2 <- repair_data(data = test_data_2, check_data_2) |> MS_wise() |> na.omit()
test_data_3 <- repair_data(data = test_data_3, check_data_3) |> MS_wise() |> na.omit()
test_data_4 <- repair_data(data = test_data_4, check_data_4) |> MS_wise() |> na.omit()
test_data_5 <- repair_data(data = test_data_5, check_data_5) |> MS_wise() |> na.omit()
test_data_6 <- repair_data(data = test_data_6, check_data_6) |> MS_wise() |> na.omit()
test_data_7 <- repair_data(data = test_data_7, check_data_7) |> MS_wise() |> na.omit()

default_pb_data_1 <- estimate_prediction_data(data = test_data_1, new_data = NULL)
default_pb_data_2 <- estimate_prediction_data(data = test_data_2, new_data = NULL)
default_pb_data_3 <- estimate_prediction_data(data = test_data_3, new_data = NULL)
default_pb_data_4 <- estimate_prediction_data(data = test_data_4, new_data = NULL)
default_pb_data_5 <- estimate_prediction_data(data = test_data_5, new_data = NULL)
default_pb_data_6 <- estimate_prediction_data(data = test_data_6, new_data = NULL)
default_pb_data_7 <- estimate_prediction_data(data = test_data_7, new_data = NULL)


test_that(desc = "Testing output names for default pb data", code = {
  expect_named(object = default_pb_data_1, expected = c("comparison", "predictor", "prediction", "lwr", "upr"), ignore.order = FALSE)
  expect_named(object = default_pb_data_2, expected = c("comparison", "predictor", "prediction", "lwr", "upr"), ignore.order = FALSE)
  expect_named(object = default_pb_data_3, expected = c("comparison", "predictor", "prediction", "lwr", "upr"), ignore.order = FALSE)
  expect_named(object = default_pb_data_4, expected = c("comparison", "predictor", "prediction", "lwr", "upr"), ignore.order = FALSE)
  expect_named(object = default_pb_data_5, expected = c("comparison", "predictor", "prediction", "lwr", "upr"), ignore.order = FALSE)
  expect_named(object = default_pb_data_6, expected = c("comparison", "predictor", "prediction", "lwr", "upr"), ignore.order = FALSE)
  expect_named(object = default_pb_data_7, expected = c("comparison", "predictor", "prediction", "lwr", "upr"), ignore.order = FALSE)
})

test_that(desc = "Testing comparison order for output and input", code = {
  expect_true(all(unique(test_data_1$comparison) == unique(default_pb_data_1$comparison)))
  expect_true(all(unique(test_data_2$comparison) == unique(default_pb_data_2$comparison)))
  expect_true(all(unique(test_data_3$comparison) == unique(default_pb_data_3$comparison)))
  expect_true(all(unique(test_data_4$comparison) == unique(default_pb_data_4$comparison)))
  expect_true(all(unique(test_data_5$comparison) == unique(default_pb_data_5$comparison)))
  expect_true(all(unique(test_data_6$comparison) == unique(default_pb_data_6$comparison)))
  expect_true(all(unique(test_data_7$comparison) == unique(default_pb_data_7$comparison)))
})

test_that(desc = "Testing must bes", code = {
  expect_true(all(default_pb_data_1$lwr <= default_pb_data_1$upr))
  expect_true(all(default_pb_data_2$lwr <= default_pb_data_2$upr))
  expect_true(all(default_pb_data_3$lwr <= default_pb_data_3$upr))
  expect_true(all(default_pb_data_4$lwr <= default_pb_data_4$upr))
  expect_true(all(default_pb_data_5$lwr <= default_pb_data_5$upr))
  expect_true(all(default_pb_data_6$lwr <= default_pb_data_6$upr))
  expect_true(all(default_pb_data_7$lwr <= default_pb_data_7$upr))

  expect_true(all(default_pb_data_1$lwr <= default_pb_data_1$prediction) | any(default_pb_data_1$prediction < 0))
  expect_true(all(default_pb_data_2$lwr <= default_pb_data_2$prediction) | any(default_pb_data_2$prediction < 0))
  expect_true(all(default_pb_data_3$lwr <= default_pb_data_3$prediction) | any(default_pb_data_3$prediction < 0))
  expect_true(all(default_pb_data_4$lwr <= default_pb_data_4$prediction) | any(default_pb_data_4$prediction < 0))
  expect_true(all(default_pb_data_5$lwr <= default_pb_data_5$prediction) | any(default_pb_data_5$prediction < 0))
  expect_true(all(default_pb_data_6$lwr <= default_pb_data_6$prediction) | any(default_pb_data_6$prediction < 0))
  expect_true(all(default_pb_data_7$lwr <= default_pb_data_7$prediction) | any(default_pb_data_7$prediction < 0))

  expect_true(all(default_pb_data_1$upr >= default_pb_data_1$prediction))
  expect_true(all(default_pb_data_2$upr >= default_pb_data_2$prediction))
  expect_true(all(default_pb_data_3$upr >= default_pb_data_3$prediction))
  expect_true(all(default_pb_data_4$upr >= default_pb_data_4$prediction))
  expect_true(all(default_pb_data_5$upr >= default_pb_data_5$prediction))
  expect_true(all(default_pb_data_6$upr >= default_pb_data_6$prediction))
  expect_true(all(default_pb_data_7$upr >= default_pb_data_7$prediction))

})

test_that(desc = "Testing whether MP_B data from CS data is used in pb", code = {
  expect_true(all(default_pb_data_1$predictor == test_data_1$MP_B))
  expect_true(all(default_pb_data_2$predictor == test_data_2$MP_B))
  expect_true(all(default_pb_data_3$predictor == test_data_3$MP_B))
  expect_true(all(default_pb_data_4$predictor == test_data_4$MP_B))
  expect_true(all(default_pb_data_5$predictor == test_data_5$MP_B))
  expect_true(all(default_pb_data_6$predictor == test_data_6$MP_B))
  expect_true(all(default_pb_data_7$predictor == test_data_7$MP_B))
})

# EQAM data used for testing
eqam_1 <- test_data_1[,list("MP_A" = range(MP_A), "MP_B" = range(MP_B)), by=list(comparison)] |> split(by="comparison",keep.by=FALSE) |>
  lapply(FUN = function(x) simulate_eqa_data(parameters = list(n = 3, R = 3, cvx = 0.02, cvy = 0.05, cil = x$MP_A[1], ciu = x$MP_A[2])) |> setDT()) |> rbindlist(idcol="comparison")
eqam_2 <- test_data_2[,list("MP_A" = range(MP_A), "MP_B" = range(MP_B)), by=list(comparison)] |> split(by="comparison",keep.by=FALSE) |>
  lapply(FUN = function(x) simulate_eqa_data(parameters = list(n = 2, R = 2, cvx = 0.03, cvy = 0.02, cil = x$MP_A[1], ciu = x$MP_A[2])) |> setDT()) |> rbindlist(idcol="comparison")
eqam_3 <- test_data_3[,list("MP_A" = range(MP_A), "MP_B" = range(MP_B)), by=list(comparison)] |> split(by="comparison",keep.by=FALSE) |>
  lapply(FUN = function(x) simulate_eqa_data(parameters = list(n = 4, R = 4, cvx = 0.05, cvy = 0.05, cil = x$MP_A[1], ciu = x$MP_A[2])) |> setDT()) |> rbindlist(idcol="comparison")
eqam_4 <- test_data_4[,list("MP_A" = range(MP_A), "MP_B" = range(MP_B)), by=list(comparison)] |> split(by="comparison",keep.by=FALSE) |>
  lapply(FUN = function(x) simulate_eqa_data(parameters = list(n = 3, R = 2, cvx = 0.05, cvy = 0.02, cil = x$MP_A[1], ciu = x$MP_A[2])) |> setDT()) |> rbindlist(idcol="comparison")
eqam_5 <- test_data_5[,list("MP_A" = range(MP_A), "MP_B" = range(MP_B)), by=list(comparison)] |> split(by="comparison",keep.by=FALSE) |>
  lapply(FUN = function(x) simulate_eqa_data(parameters = list(n = 3, R = 3, cvx = 0.02, cvy = 0.04, cil = x$MP_A[1], ciu = x$MP_A[2])) |> setDT()) |> rbindlist(idcol="comparison")
eqam_6 <- test_data_6[,list("MP_A" = range(MP_A), "MP_B" = range(MP_B)), by=list(comparison)] |> split(by="comparison",keep.by=FALSE) |>
  lapply(FUN = function(x) simulate_eqa_data(parameters = list(n = 3, R = 3, cvx = 0.04, cvy = 0.01, cil = x$MP_A[1], ciu = x$MP_A[2])) |> setDT()) |> rbindlist(idcol="comparison")
eqam_7 <- test_data_7[,list("MP_A" = range(MP_A), "MP_B" = range(MP_B)), by=list(comparison)] |> split(by="comparison",keep.by=FALSE) |>
  lapply(FUN = function(x) simulate_eqa_data(parameters = list(n = 3, R = 3, cvx = 0.03, cvy = 0.03, cil = x$MP_A[1], ciu = x$MP_A[2])) |> setDT()) |> rbindlist(idcol="comparison")

ce_data_1 <- estimate_prediction_data(data = test_data_1, new_data = eqam_1)
ce_data_2 <- estimate_prediction_data(data = test_data_2, new_data = eqam_2)
ce_data_3 <- estimate_prediction_data(data = test_data_3, new_data = eqam_3)
ce_data_4 <- estimate_prediction_data(data = test_data_4, new_data = eqam_4)
ce_data_5 <- estimate_prediction_data(data = test_data_5, new_data = eqam_5)
ce_data_6 <- estimate_prediction_data(data = test_data_6, new_data = eqam_6)
ce_data_7 <- estimate_prediction_data(data = test_data_7, new_data = eqam_7)

test_that(desc = "Testing output names for default ce data", code = {
  expect_named(object = ce_data_1, expected = c("comparison", "SampleID", "MP_B", "MP_A", "prediction", "lwr", "upr", "inside"), ignore.order = FALSE)
  expect_named(object = ce_data_2, expected = c("comparison", "SampleID", "MP_B", "MP_A", "prediction", "lwr", "upr", "inside"), ignore.order = FALSE)
  expect_named(object = ce_data_3, expected = c("comparison", "SampleID", "MP_B", "MP_A", "prediction", "lwr", "upr", "inside"), ignore.order = FALSE)
  expect_named(object = ce_data_4, expected = c("comparison", "SampleID", "MP_B", "MP_A", "prediction", "lwr", "upr", "inside"), ignore.order = FALSE)
  expect_named(object = ce_data_5, expected = c("comparison", "SampleID", "MP_B", "MP_A", "prediction", "lwr", "upr", "inside"), ignore.order = FALSE)
  expect_named(object = ce_data_6, expected = c("comparison", "SampleID", "MP_B", "MP_A", "prediction", "lwr", "upr", "inside"), ignore.order = FALSE)
  expect_named(object = ce_data_7, expected = c("comparison", "SampleID", "MP_B", "MP_A", "prediction", "lwr", "upr", "inside"), ignore.order = FALSE)
})

test_that(desc = "Testing comparison order for output and cs input (ce data)", code = {
  expect_true(all(unique(test_data_1$comparison) == unique(ce_data_1$comparison)))
  expect_true(all(unique(test_data_2$comparison) == unique(ce_data_2$comparison)))
  expect_true(all(unique(test_data_3$comparison) == unique(ce_data_3$comparison)))
  expect_true(all(unique(test_data_4$comparison) == unique(ce_data_4$comparison)))
  expect_true(all(unique(test_data_5$comparison) == unique(ce_data_5$comparison)))
  expect_true(all(unique(test_data_6$comparison) == unique(ce_data_6$comparison)))
  expect_true(all(unique(test_data_7$comparison) == unique(ce_data_7$comparison)))
})

test_that(desc = "Testing comparison order for output and eqam input (ce data)", code = {
  expect_true(all(unique(eqam_1$comparison) == unique(ce_data_1$comparison)))
  expect_true(all(unique(eqam_2$comparison) == unique(ce_data_2$comparison)))
  expect_true(all(unique(eqam_3$comparison) == unique(ce_data_3$comparison)))
  expect_true(all(unique(eqam_4$comparison) == unique(ce_data_4$comparison)))
  expect_true(all(unique(eqam_5$comparison) == unique(ce_data_5$comparison)))
  expect_true(all(unique(eqam_6$comparison) == unique(ce_data_6$comparison)))
  expect_true(all(unique(eqam_7$comparison) == unique(ce_data_7$comparison)))
})

test_that(desc = "Testing must bes for ce data", code = {
  expect_true(all(ce_data_1$lwr <= ce_data_1$upr))
  expect_true(all(ce_data_2$lwr <= ce_data_2$upr))
  expect_true(all(ce_data_3$lwr <= ce_data_3$upr))
  expect_true(all(ce_data_4$lwr <= ce_data_4$upr))
  expect_true(all(ce_data_5$lwr <= ce_data_5$upr))
  expect_true(all(ce_data_6$lwr <= ce_data_6$upr))
  expect_true(all(ce_data_7$lwr <= ce_data_7$upr))

  expect_true(all(ce_data_1$lwr <= ce_data_1$prediction) | any(ce_data_1$prediction < 0))
  expect_true(all(ce_data_2$lwr <= ce_data_2$prediction) | any(ce_data_2$prediction < 0))
  expect_true(all(ce_data_3$lwr <= ce_data_3$prediction) | any(ce_data_3$prediction < 0))
  expect_true(all(ce_data_4$lwr <= ce_data_4$prediction) | any(ce_data_4$prediction < 0))
  expect_true(all(ce_data_5$lwr <= ce_data_5$prediction) | any(ce_data_5$prediction < 0))
  expect_true(all(ce_data_6$lwr <= ce_data_6$prediction) | any(ce_data_6$prediction < 0))
  expect_true(all(ce_data_7$lwr <= ce_data_7$prediction) | any(ce_data_7$prediction < 0))

  expect_true(all(ce_data_1$upr >= ce_data_1$prediction))
  expect_true(all(ce_data_2$upr >= ce_data_2$prediction))
  expect_true(all(ce_data_3$upr >= ce_data_3$prediction))
  expect_true(all(ce_data_4$upr >= ce_data_4$prediction))
  expect_true(all(ce_data_5$upr >= ce_data_5$prediction))
  expect_true(all(ce_data_6$upr >= ce_data_6$prediction))
  expect_true(all(ce_data_7$upr >= ce_data_7$prediction))
})

test_that(desc = "Testing output types", code = {
  expect_identical(lapply(X = ce_data_1, FUN = class) |> unlist() |> unname(), c(rep("character", 2), rep("numeric", 5), "integer"))
  expect_identical(lapply(X = ce_data_2, FUN = class) |> unlist() |> unname(), c(rep("character", 2), rep("numeric", 5), "integer"))
  expect_identical(lapply(X = ce_data_3, FUN = class) |> unlist() |> unname(), c(rep("character", 2), rep("numeric", 5), "integer"))
  expect_identical(lapply(X = ce_data_4, FUN = class) |> unlist() |> unname(), c(rep("character", 2), rep("numeric", 5), "integer"))
  expect_identical(lapply(X = ce_data_5, FUN = class) |> unlist() |> unname(), c(rep("character", 2), rep("numeric", 5), "integer"))
  expect_identical(lapply(X = ce_data_6, FUN = class) |> unlist() |> unname(), c(rep("character", 2), rep("numeric", 5), "integer"))
  expect_identical(lapply(X = ce_data_7, FUN = class) |> unlist() |> unname(), c(rep("character", 2), rep("numeric", 5), "integer"))

  expect_identical(lapply(X = default_pb_data_1, FUN = class) |> unlist() |> unname(), c(rep("character", 1), rep("numeric", 4)))
  expect_identical(lapply(X = default_pb_data_2, FUN = class) |> unlist() |> unname(), c(rep("character", 1), rep("numeric", 4)))
  expect_identical(lapply(X = default_pb_data_3, FUN = class) |> unlist() |> unname(), c(rep("character", 1), rep("numeric", 4)))
  expect_identical(lapply(X = default_pb_data_4, FUN = class) |> unlist() |> unname(), c(rep("character", 1), rep("numeric", 4)))
  expect_identical(lapply(X = default_pb_data_4, FUN = class) |> unlist() |> unname(), c(rep("character", 1), rep("numeric", 4)))
  expect_identical(lapply(X = default_pb_data_6, FUN = class) |> unlist() |> unname(), c(rep("character", 1), rep("numeric", 4)))
  expect_identical(lapply(X = default_pb_data_7, FUN = class) |> unlist() |> unname(), c(rep("character", 1), rep("numeric", 4)))
})

default_pb_data_1 <- estimate_prediction_data(data = test_data_1, new_data = NULL, method = "clsi")
default_pb_data_2 <- estimate_prediction_data(data = test_data_2, new_data = NULL, method = "clsi")
default_pb_data_3 <- estimate_prediction_data(data = test_data_3, new_data = NULL, method = "clsi")
default_pb_data_4 <- estimate_prediction_data(data = test_data_4, new_data = NULL, method = "clsi")
default_pb_data_5 <- estimate_prediction_data(data = test_data_5, new_data = NULL, method = "clsi")
default_pb_data_6 <- estimate_prediction_data(data = test_data_6, new_data = NULL, method = "clsi")
default_pb_data_7 <- estimate_prediction_data(data = test_data_7, new_data = NULL, method = "clsi")


test_that(desc = "Testing output names for default pb data", code = {
  expect_named(object = default_pb_data_1, expected = c("comparison", "predictor", "prediction", "lwr", "upr"), ignore.order = FALSE)
  expect_named(object = default_pb_data_2, expected = c("comparison", "predictor", "prediction", "lwr", "upr"), ignore.order = FALSE)
  expect_named(object = default_pb_data_3, expected = c("comparison", "predictor", "prediction", "lwr", "upr"), ignore.order = FALSE)
  expect_named(object = default_pb_data_4, expected = c("comparison", "predictor", "prediction", "lwr", "upr"), ignore.order = FALSE)
  expect_named(object = default_pb_data_5, expected = c("comparison", "predictor", "prediction", "lwr", "upr"), ignore.order = FALSE)
  expect_named(object = default_pb_data_6, expected = c("comparison", "predictor", "prediction", "lwr", "upr"), ignore.order = FALSE)
  expect_named(object = default_pb_data_7, expected = c("comparison", "predictor", "prediction", "lwr", "upr"), ignore.order = FALSE)
})

test_that(desc = "Testing comparison order for output and input", code = {
  expect_true(all(unique(test_data_1$comparison) == unique(default_pb_data_1$comparison)))
  expect_true(all(unique(test_data_2$comparison) == unique(default_pb_data_2$comparison)))
  expect_true(all(unique(test_data_3$comparison) == unique(default_pb_data_3$comparison)))
  expect_true(all(unique(test_data_4$comparison) == unique(default_pb_data_4$comparison)))
  expect_true(all(unique(test_data_5$comparison) == unique(default_pb_data_5$comparison)))
  expect_true(all(unique(test_data_6$comparison) == unique(default_pb_data_6$comparison)))
  expect_true(all(unique(test_data_7$comparison) == unique(default_pb_data_7$comparison)))
})

test_that(desc = "Testing must bes", code = {
  expect_true(all(default_pb_data_1$lwr <= default_pb_data_1$upr))
  expect_true(all(default_pb_data_2$lwr <= default_pb_data_2$upr))
  expect_true(all(default_pb_data_3$lwr <= default_pb_data_3$upr))
  expect_true(all(default_pb_data_4$lwr <= default_pb_data_4$upr))
  expect_true(all(default_pb_data_5$lwr <= default_pb_data_5$upr))
  expect_true(all(default_pb_data_6$lwr <= default_pb_data_6$upr))
  expect_true(all(default_pb_data_7$lwr <= default_pb_data_7$upr))

  expect_true(all(default_pb_data_1$lwr <= default_pb_data_1$prediction) | any(default_pb_data_1$prediction < 0))
  expect_true(all(default_pb_data_2$lwr <= default_pb_data_2$prediction) | any(default_pb_data_2$prediction < 0))
  expect_true(all(default_pb_data_3$lwr <= default_pb_data_3$prediction) | any(default_pb_data_3$prediction < 0))
  expect_true(all(default_pb_data_4$lwr <= default_pb_data_4$prediction) | any(default_pb_data_4$prediction < 0))
  expect_true(all(default_pb_data_5$lwr <= default_pb_data_5$prediction) | any(default_pb_data_5$prediction < 0))
  expect_true(all(default_pb_data_6$lwr <= default_pb_data_6$prediction) | any(default_pb_data_6$prediction < 0))
  expect_true(all(default_pb_data_7$lwr <= default_pb_data_7$prediction) | any(default_pb_data_7$prediction < 0))

  expect_true(all(default_pb_data_1$upr >= default_pb_data_1$prediction))
  expect_true(all(default_pb_data_2$upr >= default_pb_data_2$prediction))
  expect_true(all(default_pb_data_3$upr >= default_pb_data_3$prediction))
  expect_true(all(default_pb_data_4$upr >= default_pb_data_4$prediction))
  expect_true(all(default_pb_data_5$upr >= default_pb_data_5$prediction))
  expect_true(all(default_pb_data_6$upr >= default_pb_data_6$prediction))
  expect_true(all(default_pb_data_7$upr >= default_pb_data_7$prediction))

})

test_that(desc = "Testing whether MP_B data from CS data is used in pb", code = {
  expect_true(all(default_pb_data_1$predictor == test_data_1$MP_B))
  expect_true(all(default_pb_data_2$predictor == test_data_2$MP_B))
  expect_true(all(default_pb_data_3$predictor == test_data_3$MP_B))
  expect_true(all(default_pb_data_4$predictor == test_data_4$MP_B))
  expect_true(all(default_pb_data_5$predictor == test_data_5$MP_B))
  expect_true(all(default_pb_data_6$predictor == test_data_6$MP_B))
  expect_true(all(default_pb_data_7$predictor == test_data_7$MP_B))
})

# EQAM data used for testing
eqam_1 <- test_data_1[,list("MP_A" = range(MP_A), "MP_B" = range(MP_B)), by=list(comparison)] |> split(by="comparison",keep.by=FALSE) |>
  lapply(FUN = function(x) simulate_eqa_data(parameters = list(n = 3, R = 3, cvx = 0.02, cvy = 0.05, cil = x$MP_A[1], ciu = x$MP_A[2])) |> setDT()) |> rbindlist(idcol="comparison")
eqam_2 <- test_data_2[,list("MP_A" = range(MP_A), "MP_B" = range(MP_B)), by=list(comparison)] |> split(by="comparison",keep.by=FALSE) |>
  lapply(FUN = function(x) simulate_eqa_data(parameters = list(n = 2, R = 2, cvx = 0.03, cvy = 0.02, cil = x$MP_A[1], ciu = x$MP_A[2])) |> setDT()) |> rbindlist(idcol="comparison")
eqam_3 <- test_data_3[,list("MP_A" = range(MP_A), "MP_B" = range(MP_B)), by=list(comparison)] |> split(by="comparison",keep.by=FALSE) |>
  lapply(FUN = function(x) simulate_eqa_data(parameters = list(n = 4, R = 4, cvx = 0.05, cvy = 0.05, cil = x$MP_A[1], ciu = x$MP_A[2])) |> setDT()) |> rbindlist(idcol="comparison")
eqam_4 <- test_data_4[,list("MP_A" = range(MP_A), "MP_B" = range(MP_B)), by=list(comparison)] |> split(by="comparison",keep.by=FALSE) |>
  lapply(FUN = function(x) simulate_eqa_data(parameters = list(n = 3, R = 2, cvx = 0.05, cvy = 0.02, cil = x$MP_A[1], ciu = x$MP_A[2])) |> setDT()) |> rbindlist(idcol="comparison")
eqam_5 <- test_data_5[,list("MP_A" = range(MP_A), "MP_B" = range(MP_B)), by=list(comparison)] |> split(by="comparison",keep.by=FALSE) |>
  lapply(FUN = function(x) simulate_eqa_data(parameters = list(n = 3, R = 3, cvx = 0.02, cvy = 0.04, cil = x$MP_A[1], ciu = x$MP_A[2])) |> setDT()) |> rbindlist(idcol="comparison")
eqam_6 <- test_data_6[,list("MP_A" = range(MP_A), "MP_B" = range(MP_B)), by=list(comparison)] |> split(by="comparison",keep.by=FALSE) |>
  lapply(FUN = function(x) simulate_eqa_data(parameters = list(n = 3, R = 3, cvx = 0.04, cvy = 0.01, cil = x$MP_A[1], ciu = x$MP_A[2])) |> setDT()) |> rbindlist(idcol="comparison")
eqam_7 <- test_data_7[,list("MP_A" = range(MP_A), "MP_B" = range(MP_B)), by=list(comparison)] |> split(by="comparison",keep.by=FALSE) |>
  lapply(FUN = function(x) simulate_eqa_data(parameters = list(n = 3, R = 3, cvx = 0.03, cvy = 0.03, cil = x$MP_A[1], ciu = x$MP_A[2])) |> setDT()) |> rbindlist(idcol="comparison")

ce_data_1 <- estimate_prediction_data(data = test_data_1, new_data = eqam_1, method = "clsi")
ce_data_2 <- estimate_prediction_data(data = test_data_2, new_data = eqam_2, method = "clsi")
ce_data_3 <- estimate_prediction_data(data = test_data_3, new_data = eqam_3, method = "clsi")
ce_data_4 <- estimate_prediction_data(data = test_data_4, new_data = eqam_4, method = "clsi")
ce_data_5 <- estimate_prediction_data(data = test_data_5, new_data = eqam_5, method = "clsi")
ce_data_6 <- estimate_prediction_data(data = test_data_6, new_data = eqam_6, method = "clsi")
ce_data_7 <- estimate_prediction_data(data = test_data_7, new_data = eqam_7, method = "clsi")

test_that(desc = "Testing output names for default ce data", code = {
  expect_named(object = ce_data_1, expected = c("comparison", "SampleID", "MP_B", "MP_A", "prediction", "lwr", "upr", "inside"), ignore.order = FALSE)
  expect_named(object = ce_data_2, expected = c("comparison", "SampleID", "MP_B", "MP_A", "prediction", "lwr", "upr", "inside"), ignore.order = FALSE)
  expect_named(object = ce_data_3, expected = c("comparison", "SampleID", "MP_B", "MP_A", "prediction", "lwr", "upr", "inside"), ignore.order = FALSE)
  expect_named(object = ce_data_4, expected = c("comparison", "SampleID", "MP_B", "MP_A", "prediction", "lwr", "upr", "inside"), ignore.order = FALSE)
  expect_named(object = ce_data_5, expected = c("comparison", "SampleID", "MP_B", "MP_A", "prediction", "lwr", "upr", "inside"), ignore.order = FALSE)
  expect_named(object = ce_data_6, expected = c("comparison", "SampleID", "MP_B", "MP_A", "prediction", "lwr", "upr", "inside"), ignore.order = FALSE)
  expect_named(object = ce_data_7, expected = c("comparison", "SampleID", "MP_B", "MP_A", "prediction", "lwr", "upr", "inside"), ignore.order = FALSE)
})

test_that(desc = "Testing comparison order for output and cs input (ce data)", code = {
  expect_true(all(unique(test_data_1$comparison) == unique(ce_data_1$comparison)))
  expect_true(all(unique(test_data_2$comparison) == unique(ce_data_2$comparison)))
  expect_true(all(unique(test_data_3$comparison) == unique(ce_data_3$comparison)))
  expect_true(all(unique(test_data_4$comparison) == unique(ce_data_4$comparison)))
  expect_true(all(unique(test_data_5$comparison) == unique(ce_data_5$comparison)))
  expect_true(all(unique(test_data_6$comparison) == unique(ce_data_6$comparison)))
  expect_true(all(unique(test_data_7$comparison) == unique(ce_data_7$comparison)))
})

test_that(desc = "Testing comparison order for output and eqam input (ce data)", code = {
  expect_true(all(unique(eqam_1$comparison) == unique(ce_data_1$comparison)))
  expect_true(all(unique(eqam_2$comparison) == unique(ce_data_2$comparison)))
  expect_true(all(unique(eqam_3$comparison) == unique(ce_data_3$comparison)))
  expect_true(all(unique(eqam_4$comparison) == unique(ce_data_4$comparison)))
  expect_true(all(unique(eqam_5$comparison) == unique(ce_data_5$comparison)))
  expect_true(all(unique(eqam_6$comparison) == unique(ce_data_6$comparison)))
  expect_true(all(unique(eqam_7$comparison) == unique(ce_data_7$comparison)))
})

test_that(desc = "Testing must bes for ce data", code = {
  expect_true(all(ce_data_1$lwr <= ce_data_1$upr))
  expect_true(all(ce_data_2$lwr <= ce_data_2$upr))
  expect_true(all(ce_data_3$lwr <= ce_data_3$upr))
  expect_true(all(ce_data_4$lwr <= ce_data_4$upr))
  expect_true(all(ce_data_5$lwr <= ce_data_5$upr))
  expect_true(all(ce_data_6$lwr <= ce_data_6$upr))
  expect_true(all(ce_data_7$lwr <= ce_data_7$upr))

  expect_true(all(ce_data_1$lwr <= ce_data_1$prediction) | any(ce_data_1$prediction < 0))
  expect_true(all(ce_data_2$lwr <= ce_data_2$prediction) | any(ce_data_2$prediction < 0))
  expect_true(all(ce_data_3$lwr <= ce_data_3$prediction) | any(ce_data_3$prediction < 0))
  expect_true(all(ce_data_4$lwr <= ce_data_4$prediction) | any(ce_data_4$prediction < 0))
  expect_true(all(ce_data_5$lwr <= ce_data_5$prediction) | any(ce_data_5$prediction < 0))
  expect_true(all(ce_data_6$lwr <= ce_data_6$prediction) | any(ce_data_6$prediction < 0))
  expect_true(all(ce_data_7$lwr <= ce_data_7$prediction) | any(ce_data_7$prediction < 0))

  expect_true(all(ce_data_1$upr >= ce_data_1$prediction))
  expect_true(all(ce_data_2$upr >= ce_data_2$prediction))
  expect_true(all(ce_data_3$upr >= ce_data_3$prediction))
  expect_true(all(ce_data_4$upr >= ce_data_4$prediction))
  expect_true(all(ce_data_5$upr >= ce_data_5$prediction))
  expect_true(all(ce_data_6$upr >= ce_data_6$prediction))
  expect_true(all(ce_data_7$upr >= ce_data_7$prediction))
})

test_that(desc = "Testing output types", code = {
  expect_identical(lapply(X = ce_data_1, FUN = class) |> unlist() |> unname(), c(rep("character", 2), rep("numeric", 5), "integer"))
  expect_identical(lapply(X = ce_data_2, FUN = class) |> unlist() |> unname(), c(rep("character", 2), rep("numeric", 5), "integer"))
  expect_identical(lapply(X = ce_data_3, FUN = class) |> unlist() |> unname(), c(rep("character", 2), rep("numeric", 5), "integer"))
  expect_identical(lapply(X = ce_data_4, FUN = class) |> unlist() |> unname(), c(rep("character", 2), rep("numeric", 5), "integer"))
  expect_identical(lapply(X = ce_data_5, FUN = class) |> unlist() |> unname(), c(rep("character", 2), rep("numeric", 5), "integer"))
  expect_identical(lapply(X = ce_data_6, FUN = class) |> unlist() |> unname(), c(rep("character", 2), rep("numeric", 5), "integer"))
  expect_identical(lapply(X = ce_data_7, FUN = class) |> unlist() |> unname(), c(rep("character", 2), rep("numeric", 5), "integer"))

  expect_identical(lapply(X = default_pb_data_1, FUN = class) |> unlist() |> unname(), c(rep("character", 1), rep("numeric", 4)))
  expect_identical(lapply(X = default_pb_data_2, FUN = class) |> unlist() |> unname(), c(rep("character", 1), rep("numeric", 4)))
  expect_identical(lapply(X = default_pb_data_3, FUN = class) |> unlist() |> unname(), c(rep("character", 1), rep("numeric", 4)))
  expect_identical(lapply(X = default_pb_data_4, FUN = class) |> unlist() |> unname(), c(rep("character", 1), rep("numeric", 4)))
  expect_identical(lapply(X = default_pb_data_4, FUN = class) |> unlist() |> unname(), c(rep("character", 1), rep("numeric", 4)))
  expect_identical(lapply(X = default_pb_data_6, FUN = class) |> unlist() |> unname(), c(rep("character", 1), rep("numeric", 4)))
  expect_identical(lapply(X = default_pb_data_7, FUN = class) |> unlist() |> unname(), c(rep("character", 1), rep("numeric", 4)))
})

set.seed(1)
particular_new_data_1 <- lapply(X = split(x = test_data_1, by = "comparison", keep.by = FALSE),
                                FUN = function(x) list("MP_B" = round(runif(1, 4, 7), 2)) |> setDT()) |> rbindlist(idcol = "comparison")
particular_new_data_2 <- lapply(X = split(x = test_data_2, by = "comparison", keep.by = FALSE),
                                FUN = function(x) list("MP_B" = round(runif(1, 4, 8), 2)) |> setDT()) |> rbindlist(idcol = "comparison")
particular_new_data_3 <- lapply(X = split(x = test_data_3, by = "comparison", keep.by = FALSE),
                                FUN = function(x) list("MP_B" = round(runif(1, 34, 42), 2)) |> setDT()) |> rbindlist(idcol = "comparison")
particular_new_data_4 <- lapply(X = split(x = test_data_4, by = "comparison", keep.by = FALSE),
                                FUN = function(x) list("MP_B" = round(runif(1, 34, 41), 2)) |> setDT()) |> rbindlist(idcol = "comparison")
particular_new_data_5 <- lapply(X = split(x = test_data_5, by = "comparison", keep.by = FALSE),
                                FUN = function(x) list("MP_B" = round(runif(1, 11, 64), 2)) |> setDT()) |> rbindlist(idcol = "comparison")
particular_new_data_6 <- lapply(X = split(x = test_data_6, by = "comparison", keep.by = FALSE),
                                FUN = function(x) list("MP_B" = round(runif(1, 33, 64), 2)) |> setDT()) |> rbindlist(idcol = "comparison")
particular_new_data_7 <- lapply(X = split(x = test_data_7, by = "comparison", keep.by = FALSE),
                                FUN = function(x) list("MP_B" = round(runif(1, 4, 5), 2)) |> setDT()) |> rbindlist(idcol = "comparison")

particular_pb_data_fg_1 <- estimate_prediction_data(data = test_data_1, new_data = particular_new_data_1, method = "fg")
particular_pb_data_fg_2 <- estimate_prediction_data(data = test_data_2, new_data = particular_new_data_2, method = "fg")
particular_pb_data_fg_3 <- estimate_prediction_data(data = test_data_3, new_data = particular_new_data_3, method = "fg")
particular_pb_data_fg_4 <- estimate_prediction_data(data = test_data_4, new_data = particular_new_data_4, method = "fg")
particular_pb_data_fg_5 <- estimate_prediction_data(data = test_data_5, new_data = particular_new_data_5, method = "fg")
particular_pb_data_fg_6 <- estimate_prediction_data(data = test_data_6, new_data = particular_new_data_6, method = "fg")
particular_pb_data_fg_7 <- estimate_prediction_data(data = test_data_7, new_data = particular_new_data_7, method = "fg")

particular_pb_data_clsi_1 <- estimate_prediction_data(data = test_data_1, new_data = particular_new_data_1, method = "clsi")
particular_pb_data_clsi_2 <- estimate_prediction_data(data = test_data_2, new_data = particular_new_data_2, method = "clsi")
particular_pb_data_clsi_3 <- estimate_prediction_data(data = test_data_3, new_data = particular_new_data_3, method = "clsi")
particular_pb_data_clsi_4 <- estimate_prediction_data(data = test_data_4, new_data = particular_new_data_4, method = "clsi")
particular_pb_data_clsi_5 <- estimate_prediction_data(data = test_data_5, new_data = particular_new_data_5, method = "clsi")
particular_pb_data_clsi_6 <- estimate_prediction_data(data = test_data_6, new_data = particular_new_data_6, method = "clsi")
particular_pb_data_clsi_7 <- estimate_prediction_data(data = test_data_7, new_data = particular_new_data_7, method = "clsi")

test_that(desc = "Testing whether CLSI and F-G methods yield same results where they should", code = {
  expect_true(all(particular_pb_data_clsi_1$prediction == particular_pb_data_fg_1$prediction))
  expect_true(all(particular_pb_data_clsi_2$prediction == particular_pb_data_fg_2$prediction))
  expect_true(all(particular_pb_data_clsi_3$prediction == particular_pb_data_fg_3$prediction))
  expect_true(all(particular_pb_data_clsi_4$prediction == particular_pb_data_fg_4$prediction))
  expect_true(all(particular_pb_data_clsi_5$prediction == particular_pb_data_fg_5$prediction))
  expect_true(all(particular_pb_data_clsi_6$prediction == particular_pb_data_fg_6$prediction))
  expect_true(all(particular_pb_data_clsi_7$prediction == particular_pb_data_fg_7$prediction))
})

ce_data_1 <- estimate_prediction_data(data = test_data_1, new_data = eqam_1, method = "clsi", B = 100)
ce_data_2 <- estimate_prediction_data(data = test_data_2, new_data = eqam_2, method = "fg", B = 100)
ce_data_3 <- estimate_prediction_data(data = test_data_3, new_data = eqam_3, method = "clsi", B = 100)
ce_data_4 <- estimate_prediction_data(data = test_data_4, new_data = eqam_4, method = "fg", B = 100)
ce_data_5 <- estimate_prediction_data(data = test_data_5, new_data = eqam_5, method = "clsi", B = 100)
ce_data_6 <- estimate_prediction_data(data = test_data_6, new_data = eqam_6, method = "fg", B = 100)
ce_data_7 <- estimate_prediction_data(data = test_data_7, new_data = eqam_7, method = "clsi", B = 100)

test_that(desc = "Testing correctness for calculation of inside rates", code = {
  expect_true(object = all(ce_data_1$inside_rate <= 1 | ce_data_1$inside_rate >= 0))
  expect_true(object = all(ce_data_2$inside_rate <= 1 | ce_data_2$inside_rate >= 0))
  expect_true(object = all(ce_data_3$inside_rate <= 1 | ce_data_3$inside_rate >= 0))
  expect_true(object = all(ce_data_4$inside_rate <= 1 | ce_data_4$inside_rate >= 0))
  expect_true(object = all(ce_data_5$inside_rate <= 1 | ce_data_5$inside_rate >= 0))
  expect_true(object = all(ce_data_6$inside_rate <= 1 | ce_data_6$inside_rate >= 0))
  expect_true(object = all(ce_data_7$inside_rate <= 1 | ce_data_7$inside_rate >= 0))
})

test_that(desc = "Testing output names for ce_data with inside rates", code = {
  expect_named(object = ce_data_1, expected = c("comparison", "SampleID", "MP_B", "MP_A", "prediction", "lwr", "upr", "inside",  "inside_rate"), ignore.order = FALSE)
  expect_named(object = ce_data_2, expected = c("comparison", "SampleID", "MP_B", "MP_A", "prediction", "lwr", "upr", "inside",  "inside_rate"), ignore.order = FALSE)
  expect_named(object = ce_data_3, expected = c("comparison", "SampleID", "MP_B", "MP_A", "prediction", "lwr", "upr", "inside",  "inside_rate"), ignore.order = FALSE)
  expect_named(object = ce_data_4, expected = c("comparison", "SampleID", "MP_B", "MP_A", "prediction", "lwr", "upr", "inside",  "inside_rate"), ignore.order = FALSE)
  expect_named(object = ce_data_5, expected = c("comparison", "SampleID", "MP_B", "MP_A", "prediction", "lwr", "upr", "inside",  "inside_rate"), ignore.order = FALSE)
  expect_named(object = ce_data_6, expected = c("comparison", "SampleID", "MP_B", "MP_A", "prediction", "lwr", "upr", "inside",  "inside_rate"), ignore.order = FALSE)
  expect_named(object = ce_data_7, expected = c("comparison", "SampleID", "MP_B", "MP_A", "prediction", "lwr", "upr", "inside",  "inside_rate"), ignore.order = FALSE)
  expect_true(object = TRUE)
})

test_that(desc = "Testing some expected errors and warnings", code = {
  expect_error(object = estimate_prediction_data(data = "yes"), regexp = "character")
  expect_error(object = estimate_prediction_data(data = 1L), regexp = "integer")
  expect_error(object = estimate_prediction_data(data = test_data_1[,-c("comparison")]), regexp = "comparison")
  expect_error(object = estimate_prediction_data(data = test_data_1[,-c("comparison", "MP_A")]), regexp = "comparison, MP_A")
  expect_error(object = estimate_prediction_data(data = test_data_1, new_data = "gen_1!00"), regexp = "3 non-empty")
  expect_warning(object = estimate_prediction_data(data = test_data_1, new_data = "gen!1A00999B9"), regexp = "Defaulting to 100")
  expect_warning(object = estimate_prediction_data(data = test_data_1, new_data = "AB1CltR#gen"), regexp = "Defaulting to 100")
})

