# Required packages
library(testthat)
library(readxl)
library(data.table)

# Read data from folder
T_EPK2020_CS <- setDT(read_excel("~/Packages/datasets to be tested on/W..EPK2020_CS.xlsx"))
T_HDLCXXX_CS <- setDT(read_excel("~/Packages/datasets to be tested on/HDLC.CS.FILTERED.xlsx"))

# Repair data and convert to long-format
T_EPK2020_CS <- MS_wise(data = repair_data(data = T_EPK2020_CS, remove_invalid_methods = TRUE))
T_HDLCXXX_CS <- MS_wise(data = repair_data(data = T_HDLCXXX_CS, remove_invalid_methods = TRUE))

# Outputs: Type 1 (full)
A_EPK2020_CS <- estimate_zeta_data(data = T_EPK2020_CS, type = "percentile", zeta_critical = 2.22, B = 200)
A_HDLCXXX_CS <- estimate_zeta_data(data = T_HDLCXXX_CS, type = "BCa", zeta_critical = 2.22, B = 200)

# Type 2 (only point estimates)
A2_EPK2020_CS <- estimate_zeta_data(data = T_EPK2020_CS, type = "basic", zeta_critical = 2.22, B = NULL)
A2_HDLCXXX_CS <- estimate_zeta_data(data = T_HDLCXXX_CS, type = "normal", zeta_critical = 2.22, B = NULL)

# Check names of output
test_that(desc = "Checking if output have expected names", code = {

  # These names are expected
  expected_names_full <- c("comparison",
                           "zeta",
                           "lwr",
                           "upr",
                           "zeta_critical",
                           "zeta_conclusion")
  expected_names_orig <- expected_names_full[1:2]

  # Check if output have names according to expected (Full & Orig)

  # (1)
  expect_named(object = A_EPK2020_CS,
               expected = expected_names_full,
               ignore.order = FALSE,
               ignore.case = FALSE)

  # (2)
  expect_named(object = A_HDLCXXX_CS,
               expected = expected_names_full,
               ignore.order = FALSE,
               ignore.case = FALSE)

  # (3)
  expect_named(object = A2_EPK2020_CS,
               expected = expected_names_orig,
               ignore.order = FALSE,
               ignore.case = FALSE)

  # (4)
  expect_named(object = A2_HDLCXXX_CS,
               expected = expected_names_orig,
               ignore.order = FALSE,
               ignore.case = FALSE)
})

# Check domains (only very serious errors are detected here ...)
test_that(desc = "Checking if zeta, lwr and upr are at all plausible", code = {
  expect_true(object = all(A_EPK2020_CS$zeta >= 0))
  expect_true(object = all(A_HDLCXXX_CS$zeta >= 0))
  expect_true(object = all(0 <= A_EPK2020_CS$lwr))
  expect_true(object = all(0 <= A_HDLCXXX_CS$lwr))
  expect_true(object = all(A_EPK2020_CS$upr >= A_EPK2020_CS$lwr))
  expect_true(object = all(A_HDLCXXX_CS$upr >= A_HDLCXXX_CS$lwr))
})

# Check all bootstrapped intervals
test_that(desc = "Checking validity of bootstrap CIs", code = {

  expect_no_error(object = estimate_zeta_data(data = T_EPK2020_CS,
                                              type = "normal",
                                              B = 200,
                                              level = 0.95,
                                              M = NULL,
                                              N = NULL,
                                              zeta_critical = 2.25,
                                              method = "ols",
                                              invalid_NA = TRUE))

  expect_no_error(object = estimate_zeta_data(data = T_HDLCXXX_CS,
                                              type = "basic",
                                              B = 200,
                                              level = 0.95,
                                              M = NULL,
                                              N = NULL,
                                              zeta_critical = 2.25,
                                              method = "ols",
                                              invalid_NA = TRUE))

  expect_no_error(object = estimate_zeta_data(data = T_EPK2020_CS,
                                              type = "percentile",
                                              B = 200,
                                              level = 0.95,
                                              M = NULL,
                                              N = NULL,
                                              zeta_critical = 2.25,
                                              method = "ss",
                                              invalid_NA = TRUE))

  expect_no_error(object = estimate_zeta_data(data = T_EPK2020_CS,
                                              type = "BCa",
                                              B = 200,
                                              level = 0.95,
                                              M = NULL,
                                              N = NULL,
                                              zeta_critical = 2.25,
                                              method = "ssw",
                                              invalid_NA = TRUE))

})

# Check approximations of zeta_critical
test_that(desc = "Checking validity of approximated zeta_critical", code = {

  # Two takes

  # First
  zeta_critical_approximated <- estimate_zeta_data(data = T_EPK2020_CS,
                                                   B = 51,
                                                   zeta_critical = NULL,
                                                   M = 1,
                                                   N = 1e4)$zeta_critical

  average_zeta_critical_approximated <- mean(x = zeta_critical_approximated,
                                             na.rm = TRUE)

  zeta_critical_from_look_up_table <- look_up_table[n == 25 & R == 3 & M == 1]$zeta

  expect_true(object = average_zeta_critical_approximated > zeta_critical_from_look_up_table * 0.95)
  expect_true(object = average_zeta_critical_approximated < zeta_critical_from_look_up_table * 1.05)

  # Second
  zeta_critical_approximated <- estimate_zeta_data(data = T_EPK2020_CS,
                                                   B = 51,
                                                   zeta_critical = NULL,
                                                   M = 0.50,
                                                   N = 1e4)$zeta_critical

  average_zeta_critical_approximated <- mean(x = zeta_critical_approximated,
                                             na.rm = TRUE)

  zeta_critical_from_look_up_table <- look_up_table[n == 25 & R == 3 & M == 0.5]$zeta

  expect_true(object = average_zeta_critical_approximated > zeta_critical_from_look_up_table * 0.95)
  expect_true(object = average_zeta_critical_approximated < zeta_critical_from_look_up_table * 1.05)


})

# Check if correct error messages are delivered
test_that(desc = "Testing if the correct errors", code = {
  expect_error(object = estimate_zeta_data(data = T_EPK2020_CS, B = -1), regexp = "is a negative value")
  expect_error(object = estimate_zeta_data(data = T_EPK2020_CS, B = "invalid"), regexp = "is a character string")
  expect_error(object = estimate_zeta_data(data = T_EPK2020_CS, B = 51, zeta_critical = NA, M = 0.5, N = 0), regexp = "is a negative value or zero")
  expect_error(object = estimate_zeta_data(data = T_EPK2020_CS, B = 51, zeta_critical = NA, M = 0.5, N = "what?"), regexp = "is a character string")
  expect_error(object = estimate_zeta_data(data = T_EPK2020_CS, B = 51, zeta_critical = NA, M = 0.5, N = -1), regexp = "is a negative value or zero")
  expect_error(object = estimate_zeta_data(data = T_EPK2020_CS, B = 51, zeta_critical = NA, M = 0.5, N = NULL), regexp = "NULL")
  expect_error(object = estimate_zeta_data(data = T_EPK2020_CS, B = 51, zeta_critical = NA, M = 0.5, N = NA), regexp = "logical")
  expect_error(object = estimate_zeta_data(data = T_EPK2020_CS, B = 51, zeta_critical = NA, M = 0), regexp = "equal to or larger than 0")
  expect_error(object = estimate_zeta_data(data = T_EPK2020_CS, B = 51, zeta_critical = NA, M = "what?"), regexp = "character")
  expect_error(object = estimate_zeta_data(data = T_EPK2020_CS, B = 51, zeta_critical = NA, M = -1), regexp = "Please provide a")
  expect_error(object = estimate_zeta_data(data = T_EPK2020_CS, B = 51, zeta_critical = NA, M = NULL), regexp = "NULL")
  expect_error(object = estimate_zeta_data(data = T_EPK2020_CS, B = 51, zeta_critical = NA, M = NA), regexp = "logical")
})











