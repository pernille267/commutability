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

test_data_1 <- repair_data(data = test_data_1, data_check = check_data_1) |> MS_wise() |> na.omit()
test_data_2 <- repair_data(data = test_data_2, data_check = check_data_2) |> MS_wise() |> na.omit()
test_data_3 <- repair_data(data = test_data_3, data_check = check_data_3) |> MS_wise() |> na.omit()
test_data_4 <- repair_data(data = test_data_4, data_check = check_data_4) |> MS_wise() |> na.omit()
test_data_5 <- repair_data(data = test_data_5, data_check = check_data_5) |> MS_wise() |> na.omit()
test_data_6 <- repair_data(data = test_data_6, data_check = check_data_6) |> MS_wise()
test_data_7 <- repair_data(data = test_data_7, data_check = check_data_7) |> MS_wise()

test_that(desc = "Testing warnings", code = {
  expect_warning(object = transform_data(test_data_1, transformation = "l_5"))
  expect_warning(object = transform_data(test_data_1, transformation = "loga_1.8"))
  expect_warning(object = transform_data(test_data_1, transformation = "sqr_3_2"))
  expect_warning(object = transform_data(test_data_1, transformation = "5"))

  expect_warning(object = transform_data(test_data_2, transformation = "somethingcompletelywrong"))
  expect_warning(object = transform_data(test_data_2, transformation = "powr_2"))
  expect_warning(object = transform_data(test_data_2, transformation = "3GB_3"))
  expect_warning(object = transform_data(test_data_2, transformation = "3-3"))

  expect_warning(object = transform_data(test_data_3, transformation = "loggy_19199"))
  expect_warning(object = transform_data(test_data_3, transformation = "inv_#Cake"))
  expect_warning(object = transform_data(test_data_3, transformation = "#12x#"))
  expect_warning(object = transform_data(test_data_3, transformation = "3-3"))
})

actual_4_1 <- transform_data(test_data_4, transformation = "lg")
actual_4_2 <- transform_data(test_data_4, transformation = "identity")
actual_4_3 <- transform_data(test_data_4, transformation = "sqrt")
actual_4_4 <- transform_data(test_data_4, transformation = "root>2")

actual_5_1 <- transform_data(test_data_5, transformation = "log_2")
actual_5_2 <- transform_data(test_data_5, transformation = "nroot/3")
actual_5_3 <- transform_data(test_data_5, transformation = "root_3")
actual_5_4 <- transform_data(test_data_5, transformation = "log_2LX")

expected_4_1 <- copy(test_data_4); expected_4_1$MP_A <- log(expected_4_1$MP_A); expected_4_1$MP_B <- log(expected_4_1$MP_B)
expected_4_2 <- copy(test_data_4)
expected_4_3 <- copy(test_data_4); expected_4_3$MP_A <- sqrt(expected_4_3$MP_A); expected_4_3$MP_B <- sqrt(expected_4_3$MP_B)
expected_4_4 <- copy(test_data_4); expected_4_4$MP_A <- sqrt(expected_4_4$MP_A); expected_4_4$MP_B <- sqrt(expected_4_4$MP_B)

expected_5_1 <- copy(test_data_5); expected_5_1$MP_A <- log2(expected_5_1$MP_A); expected_5_1$MP_B <- log2(expected_5_1$MP_B)
expected_5_2 <- copy(test_data_5); expected_5_2$MP_A <- (expected_5_2$MP_A)**(1/3); expected_5_2$MP_B <- (expected_5_2$MP_B)**(1/3)
expected_5_3 <- expected_5_2
expected_5_4 <- copy(test_data_5); expected_5_4$MP_A <- log(expected_5_4$MP_A, 2L); expected_5_4$MP_B <- log(expected_5_4$MP_B, 2L)


test_that(desc = "Correctness of output", code = {
  expect_identical(object = actual_4_1, expected = expected_4_1)
  expect_identical(object = actual_4_2, expected = expected_4_2)
  expect_identical(object = actual_4_3, expected = expected_4_3)
  expect_identical(object = actual_4_4, expected = expected_4_4)

  expect_identical(object = actual_5_1, expected = expected_5_1)
  expect_identical(object = actual_5_2, expected = expected_5_2)
  expect_identical(object = actual_5_3, expected = expected_5_3)
  expect_identical(object = actual_5_4, expected = expected_5_4)

  expect_identical(object = actual_5_1, expected = actual_5_4)
  expect_identical(object = actual_4_3, expected = actual_4_3)
})

actual_6_1 <- transform_data(test_data_6, transformation = "inv#1")
actual_6_2 <- transform_data(test_data_6, transformation = "SQuARE")
actual_6_3 <- transform_data(test_data_6, transformation = "cuBEroot")
actual_6_4 <- transform_data(test_data_6, transformation = "power^to be:3.14")

actual_7_1 <- transform_data(test_data_7, transformation = "inv#____1.5")
actual_7_2 <- transform_data(test_data_7, transformation = "2pOw")
actual_7_3 <- transform_data(test_data_7, transformation = "log_(2)")
actual_7_4 <- transform_data(test_data_7, transformation = "ln  <BASE>  3")

expected_6_1 <- copy(test_data_6); expected_6_1$MP_A <- 1/(expected_6_1$MP_A); expected_6_1$MP_B <- 1/(expected_6_1$MP_B)
expected_6_2 <- copy(test_data_6); expected_6_2$MP_A <- (expected_6_2$MP_A)**2; expected_6_2$MP_B <- (expected_6_2$MP_B)**2
expected_6_3 <- copy(test_data_6); expected_6_3$MP_A <- (expected_6_3$MP_A)**(1/3); expected_6_3$MP_B <- (expected_6_3$MP_B)**(1/3)
expected_6_4 <- copy(test_data_6); expected_6_4$MP_A <- (expected_6_4$MP_A)**3.14; expected_6_4$MP_B <- (expected_6_4$MP_B)**3.14

expected_7_1 <- copy(test_data_7); expected_7_1$MP_A <- 1/((expected_7_1$MP_A)**1.5); expected_7_1$MP_B <- 1/((expected_7_1$MP_B)**1.5)
expected_7_2 <- copy(test_data_7); expected_7_2$MP_A <- (expected_7_2$MP_A)**2; expected_7_2$MP_B <- (expected_7_2$MP_B)**2
expected_7_3 <- copy(test_data_7); expected_7_3$MP_A <- log2(expected_7_3$MP_A); expected_7_3$MP_B <- log2(expected_7_3$MP_B)
expected_7_4 <- copy(test_data_7); expected_7_4$MP_A <- log(expected_7_4$MP_A, 3L); expected_7_4$MP_B <- log(expected_7_4$MP_B, 3L)

test_that(desc = "Correctness of output for extremely weird transformation input", code = {
  expect_identical(object = actual_6_1, expected = expected_6_1)
  expect_identical(object = actual_6_2, expected = expected_6_2)
  expect_identical(object = actual_6_3, expected = expected_6_3)
  expect_identical(object = actual_6_4, expected = expected_6_4)

  expect_identical(object = actual_7_1, expected = expected_7_1)
  expect_identical(object = actual_7_2, expected = expected_7_2)
  expect_identical(object = actual_7_3, expected = expected_7_3)
  expect_identical(object = actual_7_4, expected = expected_7_4)
})

