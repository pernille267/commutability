library(testthat)
library(readxl)
library(data.table)
library(fasteqa)
library(smooth.commutability)

# Testing data
test_cs_data <- copy(crp_cs_data)

# Check if no error or warning is thrown for expected valid input
test_that(desc = "Check if functions works with valid input", code = {

  # Some arbitrary run tests

  # Run without error (1)
  expect_no_condition(
    object = perform_assessment_tests(
      data = test_cs_data,
      B = NULL,
      method = "fg",
      robust = TRUE,
      level = 0.90,
      adjust_p_value = TRUE,
      holm = TRUE,
      output = "visual"
    )
  )

  # Run without error (2)
  expect_no_condition(
    object = perform_assessment_tests(
      data = test_cs_data,
      B = NULL,
      method = "ssw",
      robust = TRUE,
      level = 0.62,
      adjust_p_value = FALSE,
      holm = FALSE,
      output = "visual"
    )
  )

  # Run without error (3)
  expect_no_condition(
    object = perform_assessment_tests(
      data = test_cs_data,
      B = NULL,
      method = "ss",
      robust = TRUE,
      level = 0.76,
      adjust_p_value = FALSE,
      holm = FALSE,
      output = "exact"
    )
  )

  # Run without error (4)
  expect_no_condition(
    object = perform_assessment_tests(
      data = test_cs_data,
      B = 115,
      method = "clsi",
      robust = TRUE,
      level = 0.9123,
      adjust_p_value = FALSE,
      holm = TRUE,
      output = "visual"
    )
  )

  # Run without error (5)
  expect_no_condition(
    object = perform_assessment_tests(
      data = test_cs_data,
      B = 20,
      method = "ssw",
      robust = TRUE,
      level = 0.92111,
      adjust_p_value = FALSE,
      holm = TRUE,
      output = "exact"
    )
  )

  # Run without error (3)
  expect_no_condition(
    object = perform_assessment_tests(
      data = test_cs_data,
      B = 29,
      method = "ols",
      robust = TRUE,
      level = 0.96,
      adjust_p_value = TRUE,
      holm = FALSE,
      output = "visual"
    )
  )
})

# Check output structure depending outout
test_that(desc = "Check if output structure is as expected", code = {

  # Expect names
  visual_names_1 <- c("IVD-MD Comparison",
                      "Testing",
                      "Test Name",
                      "p-value",
                      "Conclusion")
  visual_names_2 <- c(visual_names_1, "Rejection Rate")

  raw_names_1 <- c("comparison",
                   "testing",
                   "test_name",
                   "p.value",
                   "conclusion")
  raw_names_2 <- c(raw_names_1, "rejection_rate")

  # Expected classes
  visual_classes_1 <- rep("character", 5L)
  visual_classes_2 <- rep("character", 6L)
  raw_classes_1 <- c(rep("character", 3),
                     "numeric",
                     "character")
  raw_classes_2 <- c(raw_classes_1, "numeric")


  # Actual output
  actual_1 <- perform_assessment_tests(
    data = test_cs_data,
    B = NULL,
    method = sample(x = c("ols", "clsi", "fg", "ss", "ssw"), size = 1),
    robust = sample(x = c(FALSE, TRUE), size = 1),
    level = runif(n = 1, min = 0.75, max = 0.99),
    adjust_p_value = sample(x = c(FALSE, TRUE), size = 1),
    holm = sample(x = c(FALSE, TRUE), size = 1),
    output = "visual"
  )

  # Check structure of actual_1

  # Check names
  expect_named(
    object = actual_1,
    expected = visual_names_1,
    ignore.order = FALSE,
    ignore.case = FALSE
  )

  # Check classes
  expect_equal(
    object = unname(sapply(actual_1, class, simplify = TRUE, USE.NAMES = FALSE)),
    expected = visual_classes_1
  )

  # Actual output
  actual_2 <- perform_assessment_tests(
    data = test_cs_data,
    B = NULL,
    method = sample(x = c("ols", "clsi", "fg", "ss", "ssw"), size = 1),
    robust = sample(x = c(FALSE, TRUE), size = 1),
    level = runif(n = 1, min = 0.75, max = 0.99),
    adjust_p_value = sample(x = c(FALSE, TRUE), size = 1),
    holm = sample(x = c(FALSE, TRUE), size = 1),
    output = "raw"
  )

  # Check structure of actual_2

  # Check names
  expect_named(
    object = actual_2,
    expected = raw_names_1,
    ignore.order = FALSE,
    ignore.case = FALSE
  )

  # Check classes
  expect_equal(
    object = unname(sapply(actual_2, class, simplify = TRUE, USE.NAMES = FALSE)),
    expected = raw_classes_1
  )

  # Actual output
  actual_3 <- perform_assessment_tests(
    data = test_cs_data,
    B = 19,
    method = sample(x = c("ols", "clsi", "fg", "ss", "ssw"), size = 1),
    robust = sample(x = c(FALSE, TRUE), size = 1),
    level = runif(n = 1, min = 0.75, max = 0.99),
    adjust_p_value = sample(x = c(FALSE, TRUE), size = 1),
    holm = sample(x = c(FALSE, TRUE), size = 1),
    output = "visual"
  )

  # Check structure of actual_3

  # Check names
  expect_named(
    object = actual_3,
    expected = visual_names_2,
    ignore.order = FALSE,
    ignore.case = FALSE
  )

  # Check classes
  expect_equal(
    object = unname(sapply(actual_3, class, simplify = TRUE, USE.NAMES = FALSE)),
    expected = visual_classes_2
  )

  # Actual output
  actual_4 <- perform_assessment_tests(
    data = test_cs_data,
    B = 21,
    method = sample(x = c("ols", "clsi", "fg", "ss", "ssw"), size = 1),
    robust = sample(x = c(FALSE, TRUE), size = 1),
    level = runif(n = 1, min = 0.75, max = 0.99),
    adjust_p_value = sample(x = c(FALSE, TRUE), size = 1),
    holm = sample(x = c(FALSE, TRUE), size = 1),
    output = "raw"
  )

  # Check structure of actual_4

  # Check names
  expect_named(
    object = actual_4,
    expected = raw_names_2,
    ignore.order = FALSE,
    ignore.case = FALSE
  )

  # Check classes
  expect_equal(
    object = unname(sapply(actual_4, class, simplify = TRUE, USE.NAMES = FALSE)),
    expected = raw_classes_2
  )

})



