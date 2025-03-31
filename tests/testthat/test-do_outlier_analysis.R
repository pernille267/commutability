library(testthat)
library(readxl)
library(data.table)
library(fasteqa)
library(smooth.commutability)

# Get testing data
test_cs_data <- copy(smooth.commutability::crp_cs_data)
test_cs_data$MP_A[1:3] <- test_cs_data$MP_A[1:3] + 26

# Test output structure for output = 'visual'
test_that(desc = "Check output structure for output = 'visual'", code = {

  random_variable <- sample(
    x = c("influence",
          "absolute_difference",
          "relative_difference",
          "log_difference"),
    size = 4,
    replace = FALSE
  )

  random_level <- runif(n = 4, min = 0.90, max = 0.99)

  # Expect to run without condition
  expect_no_condition(
    object = do_outlier_analysis(
      data = test_cs_data,
      method = "iqr",
      level = random_level[1],
      variable = random_variable[1]
    )
  )
  expect_no_condition(
    object = do_outlier_analysis(
      data = test_cs_data,
      method = "chauvenet",
      level = random_level[2],
      variable = random_variable[2]
    )
  )
  expect_no_condition(
    object = do_outlier_analysis(
      data = test_cs_data,
      method = "burnett",
      level = random_level[3],
      variable = random_variable[3]
    )
  )
  expect_no_condition(
    object = do_outlier_analysis(
      data = test_cs_data,
      method = "qrange",
      level = random_level[4],
      variable = random_variable[4]
    )
  )

  # Expected to have particular names
  expected_names_SampleID_wise <- c("IVD-MD Comparison",
                                    "Outlier Detection Method",
                                    "Outliers")

  expected_names_ReplicateID_wise <- c("SampleID",
                                       "AQT90",
                                       "Chroma",
                                       "Eutech",
                                       "Luminex",
                                       "QuickRead")

  # Actual outputs
  actual_1 <- do_outlier_analysis(data = test_cs_data,
                                  method = "iqr",
                                  level = random_level[4],
                                  variable = random_variable[4])
  actual_2 <- do_outlier_analysis(data = test_cs_data,
                                  method = "chauvenet",
                                  level = random_level[3],
                                  variable = random_variable[3])
  actual_3 <- do_outlier_analysis(data = test_cs_data,
                                  method = "burnett",
                                  level = random_level[2],
                                  variable = random_variable[2])
  actual_4 <- do_outlier_analysis(test_cs_data,
                                  method = "qrange",
                                  level = random_level[1],
                                  variable = random_variable[1])

  # Check names
  expect_named(
    object = actual_1,
    expected = expected_names_SampleID_wise,
    ignore.order = FALSE,
    ignore.case = FALSE
  )
  expect_named(
    object = actual_2,
    expected = expected_names_SampleID_wise,
    ignore.order = FALSE,
    ignore.case = FALSE
  )
  expect_named(
    object = actual_3,
    expected = expected_names_SampleID_wise,
    ignore.order = FALSE,
    ignore.case = FALSE
  )
  expect_named(
    object = actual_4,
    expected = expected_names_ReplicateID_wise,
    ignore.order = FALSE,
    ignore.case = FALSE
  )

  # Check if is data.table
  expect_true(
    object = is.data.table(actual_1)
  )
  expect_true(
    object = is.data.table(actual_2)
  )
  expect_true(
    object = is.data.table(actual_3)
  )
  expect_true(
    object = is.data.table(actual_4)
  )

  # Check number of rows
  expect_equal(
    object = nrow(actual_1),
    expected = length(unique(test_cs_data$comparison))
  )
  expect_equal(
    object = nrow(actual_2),
    expected = length(unique(test_cs_data$comparison))
  )
  expect_equal(
    object = nrow(actual_3),
    expected = length(unique(test_cs_data$comparison))
  )
  expect_equal(
    object = nrow(actual_4),
    expected = length(unique(reverse_comparison_data(test_cs_data)$SampleID))
  )

})

# Test output structure for output = 'raw'
test_that(desc = "Check output structure for output = 'raw'", code = {

  # Get random parameters
  random_variable <- sample(
    x = c("influence",
          "absolute_difference",
          "relative_difference",
          "log_difference"),
    size = 4,
    replace = FALSE
  )

  # Get random level
  random_level <- runif(n = 4, min = 0.90, max = 0.99)

  # Expect to run without condition
  expect_no_condition(
    object = do_outlier_analysis(
      data = test_cs_data,
      method = "iqr",
      level = random_level[1],
      variable = random_variable[1],
      output = "raw"
    )
  )
  expect_no_condition(
    object = do_outlier_analysis(
      data = test_cs_data,
      method = "chauvenet",
      level = random_level[2],
      variable = random_variable[2],
      output = "raw"
    )
  )
  expect_no_condition(
    object = do_outlier_analysis(
      data = test_cs_data,
      method = "burnett",
      level = random_level[3],
      variable = random_variable[3],
      output = "raw"
    )
  )
  expect_no_condition(
    object = do_outlier_analysis(
      data = test_cs_data,
      method = "qrange",
      level = random_level[4],
      variable = random_variable[4],
      output = "raw"
    )
  )

  # Expected to have particular names
  expected_names_SampleID_wise <- c("comparison",
                                    "SampleID",
                                    "variable",
                                    "outlier")
  expected_names_ReplicateID_wise <- c("comparison",
                                       "SampleID",
                                       "outlier_A",
                                       "outlier_B")

  # Actual outputs
  actual_1 <- do_outlier_analysis(data = test_cs_data,
                                  method = "iqr",
                                  level = random_level[4],
                                  variable = random_variable[4],
                                  output = "raw")
  actual_2 <- do_outlier_analysis(data = test_cs_data,
                                  method = "chauvenet",
                                  level = random_level[3],
                                  variable = random_variable[3],
                                  output = "raw")
  actual_3 <- do_outlier_analysis(data = test_cs_data,
                                  method = "burnett",
                                  level = random_level[2],
                                  variable = random_variable[2],
                                  output = "raw")
  actual_4 <- do_outlier_analysis(test_cs_data,
                                  method = "qrange",
                                  level = random_level[1],
                                  variable = random_variable[1],
                                  output = "raw")


  # Check names
  expect_named(
    object = actual_1,
    expected = expected_names_SampleID_wise,
    ignore.order = FALSE,
    ignore.case = FALSE
  )
  expect_named(
    object = actual_2,
    expected = expected_names_SampleID_wise,
    ignore.order = FALSE,
    ignore.case = FALSE
  )
  expect_named(
    object = actual_3,
    expected = expected_names_SampleID_wise,
    ignore.order = FALSE,
    ignore.case = FALSE
  )
  expect_named(
    object = actual_4,
    expected = expected_names_ReplicateID_wise,
    ignore.order = FALSE,
    ignore.case = FALSE
  )

  # Check if is data.table
  expect_true(
    object = is.data.table(actual_1)
  )
  expect_true(
    object = is.data.table(actual_2)
  )
  expect_true(
    object = is.data.table(actual_3)
  )
  expect_true(
    object = is.data.table(actual_4)
  )

  # Check number of rows
  expect_equal(
    object = nrow(actual_1),
    expected = sum(
      unname(
        tapply(
          X = test_cs_data$SampleID,
          INDEX = test_cs_data$comparison,
          FUN = function(comp) {
            length(unique(comp))
          },
          simplify = TRUE
        )
      )
    )
  )
  expect_equal(
    object = nrow(actual_2),
    expected = sum(
      unname(
        tapply(
          X = test_cs_data$SampleID,
          INDEX = test_cs_data$comparison,
          FUN = function(comp) {
            length(unique(comp))
          },
          simplify = TRUE
        )
      )
    )
  )
  expect_equal(
    object = nrow(actual_3),
    expected = sum(
      unname(
        tapply(
          X = test_cs_data$SampleID,
          INDEX = test_cs_data$comparison,
          FUN = function(comp) {
            length(unique(comp))
          },
          simplify = TRUE
        )
      )
    )
  )
  # Check number of rows
  expect_equal(
    object = nrow(actual_4),
    expected = sum(
      unname(
        tapply(
          X = test_cs_data$SampleID,
          INDEX = test_cs_data$comparison,
          FUN = function(comp) {
            length(unique(comp))
          },
          simplify = TRUE
        )
      )
    )
  )
})




