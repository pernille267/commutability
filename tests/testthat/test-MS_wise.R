library(testthat)
library(readxl)
library(data.table)
library(fasteqa)

test_data_1 <- setDT(read_excel("~/Packages/datasets to be tested on/W..EPK2020_CS.xlsx"))
test_data_2 <- setDT(read_excel("~/Packages/datasets to be tested on/HDLC.CS.FILTERED.xlsx"))
test_data_3 <- setDT(read_excel("~/Packages/datasets to be tested on/test_data_7.xlsx"))
test_data_4 <- setDT(read_excel("~/Packages/datasets to be tested on/test_data_6.xlsx"))

test_data_1 <- repair_data(test_data_1)
test_data_2 <- repair_data(test_data_2)
test_data_3 <- repair_data(test_data_3)
test_data_4 <- repair_data(test_data_4)

# Testing names of output
test_that(desc = "Checking output for valid names", code = {

  # The expected names
  expected_names <- c("comparison", "SampleID", "ReplicateID", "MP_A", "MP_B")

  # Actual output
  actual_1 <- get_comparison_data(data = test_data_1)
  actual_2 <- get_comparison_data(data = test_data_3)

  # Tests
  expect_named(object = actual_1,
               expected = expected_names,
               ignore.order = FALSE,
               ignore.case = FALSE)

  expect_named(object = actual_2,
               expected = expected_names,
               ignore.order = FALSE,
               ignore.case = FALSE)

})

# Testing types of output
test_that(desc = "Checking output types", code = {

  # The expected types
  expected_types <- c("character", "character", "character", "numeric", "numeric")
  names(expected_types) <- c("comparison", "SampleID", "ReplicateID", "MP_A", "MP_B")

  # Actual output
  actual_1 <- sapply(X = get_comparison_data(data = test_data_3),
                     FUN = function(out_column) {
                       class(out_column)
                     },
                     simplify = TRUE,
                     USE.NAMES = FALSE)
  actual_2 <- sapply(X = get_comparison_data(data = test_data_3),
                     FUN = function(out_column) {
                       class(out_column)
                     },
                     simplify = TRUE,
                     USE.NAMES = FALSE)

  # Tests
  expect_equal(object = actual_1, expected = expected_types)
  expect_equal(object = actual_2, expected = expected_types)

})

# Testing errors
test_that(desc = "Checking errors / warnings", code = {

  # An example of very wrong data
  unsuitable_data <- data.table("Cat ID" = c(1, 2, 3),
                                "Breed" = c("naked", "not naked", "forest cat"),
                                "Tail length" = c(13.22, 11.92, 9.94),
                                "Has head" = c(TRUE, TRUE, FALSE))

  # An example of data that is already on IVD-MD comparison format
  already_transformed <- get_comparison_data(test_data_1)

  # Check if errors are thrown if data is not data.table, list or data.frame
  expect_error(object = get_comparison_data(data = 1),
               regexp = "Your input class: 'numeric'")
  expect_error(object = get_comparison_data(data = c("blabla", "blablabla")),
               regexp = "Your input class: 'character'")

  # Check if error is thrown if data is far off
  expect_error(object = get_comparison_data(data = unsuitable_data),
               regexp = "Only these columns were found in your 'data' input:")

  # Check if warning is thrown if data already have a column named comparison
  expect_warning(object = get_comparison_data(already_transformed),
                 regexp = "The input 'data' appears to already be in long format.")


})

