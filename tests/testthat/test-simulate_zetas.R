# random draws
rd <- 5L

# Various study designs
small_study_design <- list(n = 20, R = 2)
medium_study_design <- list(n = 25, R = 3)
large_study_design <- list(n = 40, R = 4)
study_designs <- list(small_study_design, medium_study_design, large_study_design)

# Various auxiliary parameters for respective simulation scenarios
simulation_scenario_2_1 <- list(eta = 5)
simulation_scenario_2_2 <- list(eta = 2)
simulation_scenario_3_1 <- list(prop = 0.05, mmax = 5)
simulation_scenario_3_2 <- list(prop = 0.05, mmax = 10)
simulation_scenario_3_3 <- list(prop = 0.30, mmax = 5)
simulation_scenario_3_4 <- list(prop = 0.30, mmax = 10)
simulation_scenario_4_1 <- list(qran = 0.05, mmax = 5, qpos = sample(x = c(0, 1), size = 1))
simulation_scenario_4_2 <- list(qran = 0.05, mmax = 10, qpos = sample(x = c(0, 1), size = 1))
simulation_scenario_4_3 <- list(qran = 0.30, mmax = 5, qpos = sample(x = c(0, 1), size = 1))
simulation_scenario_4_4 <- list(qran = 0.30, mmax = 10, qpos = sample(x = c(0, 1), size = 1))
simulation_scenario_5_1 <- list(M = 0.25)
simulation_scenario_5_2 <- list(M = 0.50)
simulation_scenario_5_3 <- list(M = 0.75)
simulation_scenario_5_4 <- list(M = 1.00)
simulation_scenarios <- list(simulation_scenario_2_1, simulation_scenario_2_2,
                             simulation_scenario_3_1, simulation_scenario_3_2,
                             simulation_scenario_3_3, simulation_scenario_3_4,
                             simulation_scenario_4_1, simulation_scenario_4_2,
                             simulation_scenario_4_3, simulation_scenario_4_4,
                             simulation_scenario_5_1, simulation_scenario_5_2,
                             simulation_scenario_5_3, simulation_scenario_5_4)


random_main_parameters <- sample.int(n = length(study_designs), size = rd, replace = TRUE, prob = c(0.25, 0.50, 0.25))
random_secondary_parameters <- sample.int(n = length(simulation_scenarios), size = rd, replace = TRUE)

test_that(desc = "Testing 'attach' functionality", code = {
  for(i in 1:rd){

    expected_parameter_names_1 <- names(c(study_designs[[random_main_parameters[i]]], simulation_scenarios[[random_secondary_parameters[i]]]))
    expected_parameter_names_1 <- c(expected_parameter_names_1, c("replicate_id", "statistic"))
    parameters_i <- c(study_designs[[random_main_parameters[i]]], simulation_scenarios[[random_secondary_parameters[i]]])

    observed_output_1 <- simulate_zetas(n = 100, parameters = parameters_i, statistic = mad, m = 5, attach = TRUE, simplify = TRUE)
    observed_output_2 <- simulate_zetas(n = 100, parameters = parameters_i, statistic = median, m = 5, attach = TRUE, simplify = FALSE)
    observed_output_3 <- simulate_zetas(n = 100, parameters = parameters_i, statistic = quantile, m = 5, attach = TRUE, simplify = TRUE)
    observed_output_4 <- simulate_zetas(n = 100, parameters = parameters_i, statistic = sd, m = 1, attach = TRUE, simplify = TRUE)

    expect_named(object = observed_output_1, expected = expected_parameter_names_1, ignore.order = FALSE)
    expect_named(object = observed_output_2, expected = expected_parameter_names_1, ignore.order = FALSE)
    expect_named(object = observed_output_3, expected = expected_parameter_names_1, ignore.order = FALSE)
    expect_named(object = observed_output_4, expected = expected_parameter_names_1, ignore.order = FALSE)
  }
})

test_that(desc = "Testing 'simplify' functionality", code = {
  for(i in 1:rd){


    parameters_i <- c(study_designs[[random_main_parameters[i]]], simulation_scenarios[[random_secondary_parameters[i]]])

    # Expects a data.table object
    observed_output_1 <- simulate_zetas(n = 100, parameters = parameters_i, statistic = sd, m = 1, attach = TRUE, simplify = TRUE)
    # Expects a numeric value
    observed_output_2 <- simulate_zetas(n = 100, parameters = parameters_i, statistic = mean, m = 1, attach = FALSE, simplify = TRUE)
    # Expects a list object
    observed_output_3 <- simulate_zetas(n = 100, parameters = parameters_i, statistic = var, m = 1, attach = TRUE, simplify = FALSE)
    # Expects a list object, but with greater length than the previous because simulation parameters are attached to output
    observed_output_4 <- simulate_zetas(n = 100, parameters = parameters_i, statistic = median, m = 1, attach = FALSE, simplify = FALSE)

    # Expects a data.table object
    observed_output_5 <- simulate_zetas(n = 100, parameters = parameters_i, statistic = var, m = 2, attach = TRUE, simplify = TRUE)
    # Expects a vector with two elements
    observed_output_6 <- simulate_zetas(n = 100, parameters = parameters_i, statistic = sd, m = 2, attach = FALSE, simplify = TRUE)
    # Expects a list object
    observed_output_7 <- simulate_zetas(n = 100, parameters = parameters_i, statistic = median, m = 2, attach = TRUE, simplify = FALSE)
    # Expects a list object, but with greater length than the previous because simulation parameters are attached to output
    observed_output_8 <- simulate_zetas(n = 100, parameters = parameters_i, statistic = mean, m = 2, attach = FALSE, simplify = FALSE)

    expect_true(object = is.data.table(observed_output_1))
    expect_true(object = !is.data.table(observed_output_2) && !is.list(observed_output_2) && length(observed_output_2) == 1 && is.numeric(observed_output_2))
    expect_true(object = !is.data.table(observed_output_3) && is.list(observed_output_3))
    expect_true(object = !is.data.table(observed_output_4) && is.list(observed_output_4) && length(observed_output_3) > is.list(observed_output_4))

    expect_true(object = is.data.table(observed_output_5))
    expect_true(object = !is.data.table(observed_output_6) && !is.list(observed_output_6) && length(observed_output_6) == 2 && is.numeric(observed_output_6))
    expect_true(object = !is.data.table(observed_output_7) && is.list(observed_output_7))
    expect_true(object = !is.data.table(observed_output_8) && is.list(observed_output_8) && length(observed_output_7) > is.list(observed_output_8))

  }
})

test_that(desc = "Testing the valid character options for 'statistic'", code = {
  for(i in 1:rd){
    expected_parameter_names <- names(c(study_designs[[random_main_parameters[i]]], simulation_scenarios[[random_secondary_parameters[i]]]))
    all_statistics_names <- c("replicate_id", "mean", "sd", "skewness", "kurtosis", "1%", "2.5%", "5%", "10%", "25%", "50%", "75%", "90%", "95%", "97.5%", "99%")
    all2_statistics_names <- c("replicate_id", "mean", "sd", "skewness", "kurtosis", "1%", "99%")
    moments_statistics_names <- c("replicate_id", "mean", "sd", "skewness", "kurtosis")
    moments12_statistics_names <- c("replicate_id", "mean", "sd")
    moments34_statistics_names <- c("replicate_id", "skewness", "kurtosis")
    quantiles_statistics_names <- c("replicate_id", "1%", "2.5%", "5%", "10%", "25%", "50%", "75%", "90%", "95%", "97.5%", "99%")
    quantiles2_statistics_names <- c("replicate_id", "1%", "25%", "50%", "75%", "99%")
    quartiles_statistics_names <- c("replicate_id", "25%", "50%", "75%")
    parameters_i <- c(study_designs[[random_main_parameters[i]]], simulation_scenarios[[random_secondary_parameters[i]]])

    observed_output_1 <- simulate_zetas(n = 100, parameters = parameters_i, statistic = "all", m = sample.int(2, 1), attach = TRUE, simplify = TRUE)
    observed_output_2 <- simulate_zetas(n = 100, parameters = parameters_i, statistic = "all2", m = sample.int(2, 1), attach = TRUE, simplify = TRUE)
    observed_output_3 <- simulate_zetas(n = 100, parameters = parameters_i, statistic = "moments", m = sample.int(2, 1), attach = TRUE, simplify = TRUE)
    observed_output_4 <- simulate_zetas(n = 100, parameters = parameters_i, statistic = "moments12", m = sample.int(2, 1), attach = TRUE, simplify = TRUE)
    observed_output_5 <- simulate_zetas(n = 100, parameters = parameters_i, statistic = "moments34", m = sample.int(2, 1), attach = TRUE, simplify = TRUE)
    observed_output_6 <- simulate_zetas(n = 100, parameters = parameters_i, statistic = "quantiles", m = sample.int(2, 1), attach = TRUE, simplify = TRUE)
    observed_output_7 <- simulate_zetas(n = 100, parameters = parameters_i, statistic = "quantiles2", m = sample.int(2, 1), attach = TRUE, simplify = TRUE)
    observed_output_8 <- simulate_zetas(n = 100, parameters = parameters_i, statistic = "quartiles", m = sample.int(2, 1), attach = TRUE, simplify = TRUE)

    expect_named(object = observed_output_1, expected = c(expected_parameter_names, all_statistics_names), ignore.order = FALSE)
    expect_named(object = observed_output_2, expected = c(expected_parameter_names, all2_statistics_names), ignore.order = FALSE)
    expect_named(object = observed_output_3, expected = c(expected_parameter_names, moments_statistics_names), ignore.order = FALSE)
    expect_named(object = observed_output_4, expected = c(expected_parameter_names, moments12_statistics_names), ignore.order = FALSE)
    expect_named(object = observed_output_5, expected = c(expected_parameter_names, moments34_statistics_names), ignore.order = FALSE)
    expect_named(object = observed_output_6, expected = c(expected_parameter_names, quantiles_statistics_names), ignore.order = FALSE)
    expect_named(object = observed_output_7, expected = c(expected_parameter_names, quantiles2_statistics_names), ignore.order = FALSE)
    expect_named(object = observed_output_8, expected = c(expected_parameter_names, quartiles_statistics_names), ignore.order = FALSE)

  }
})

test_that(desc = "Testing functionality of 'm'", code = {
  for(i in 1:rd){
    parameters_i <- c(study_designs[[random_main_parameters[i]]], simulation_scenarios[[random_secondary_parameters[i]]])
    observed_output_1 <- simulate_zetas(n = 100, parameters = parameters_i, statistic = median, m = 2, simplify = TRUE)
    observed_output_2 <- simulate_zetas(n = 100, parameters = parameters_i, statistic = median, m = 3, simplify = TRUE)
    observed_output_3 <- simulate_zetas(n = 100, parameters = parameters_i, statistic = median, m = 5, simplify = TRUE)
    observed_output_4 <- simulate_zetas(n = 100, parameters = parameters_i, statistic = median, m = 10, simplify = TRUE)

    expect_equal(object = nrow(observed_output_1), expected = 2)
    expect_equal(object = nrow(observed_output_2), expected = 3)
    expect_equal(object = nrow(observed_output_3), expected = 5)
    expect_equal(object = nrow(observed_output_4), expected = 10)
  }

})

test_that(desc = "Testing errors", code = {
  expect_error(object = simulate_zetas(n = 100, parameters = list(n = 25, R = 3), m = -2L), regexp = "is negative")
  expect_error(object = simulate_zetas(n = 100, parameters = list(n = 25, R = 3), m = 1.2), regexp = "is a decimal number")
  expect_error(object = simulate_zetas(n = 100, parameters = list(n = 25, R = 3), m = "m is a string"), regexp = "is of invalid class")
  expect_error(object = simulate_zetas(n = 100, parameters = c(1, 2), m = 1), regexp = "The input 'parameters' is neither a list, data.table, nor data.frame.")
  expect_error(object = simulate_zetas(n = 100, parameters = list(n = 25, R = 3), m = 2, statistic = "none"))
  expect_error(object = simulate_zetas(n = 100, parameters = list(n = 25, R = 3), m = 1, statistic = "wrong character string"), regexp = "did not match with one of the valid options")
  expect_error(object = simulate_zetas(n = 100, parameters = list(n = 25, R = 3), m = 2, statistic = "wrong character string"), regexp = "did not match with one of the valid options")
  expect_error(object = simulate_zetas(n = 100, parameters = list(n = 25, R = 3), m = 2, statistic = mean, attach = 2), regexp = "Expected a logical value, but a numeric value was provided instead")
  expect_error(object = simulate_zetas(n = 100, parameters = list(n = 25, R = 3), m = 2, statistic = mean, attach = "Why is this a character?"), regexp = "Expected a logical value, but a character string was provided instead")
  expect_error(object = simulate_zetas(n = 100, parameters = list(n = 25, R = 3), m = 2, statistic = mean, attach = 5L), regexp = "Expected a logical value, but a integer value was provided instead")
  expect_error(object = simulate_zetas(n = 100, parameters = list(n = 25, R = 3), m = 2, statistic = mean, attach = NA), regexp = "Expected a non-missing logical value, but a missing value was provided instead")
  expect_error(object = simulate_zetas(n = 100, parameters = list(n = 25, R = 3), m = 2, statistic = mean, attach = NULL), regexp = "Expected a non-missing logical value, but a NULL was provided instead")
  expect_error(object = simulate_zetas(n = 100, parameters = list(n = 25, R = 3), m = 2, statistic = mean, attach = list("a" = 1)), regexp = "Expected a non-missing logical value, but a list was provided instead")
  expect_error(object = simulate_zetas(n = 100, parameters = list(n = 25, R = 3), m = 2, statistic = mean, attach = data.table("a" = 1)), regexp = "Expected a non-missing logical value, but a data.table was provided instead")
  expect_error(object = suppressWarnings(simulate_zetas(n = 100, parameters = list(n = 25, R = 3), m = 2, statistic = mean, attach = c("incorrect", "TRUE"))) , regexp = "Expected a non-missing logical value, but a character string was provided instead")
  expect_error(object = simulate_zetas(n = 100, parameters = list(n = 25, R = 3), m = 2, statistic = mean, simplify = 2), regexp = "Expected a logical value, but a numeric value was provided instead")
  expect_error(object = simulate_zetas(n = 100, parameters = list(n = 25, R = 3), m = 2, statistic = mean, simplify = "Why is this a character?"), regexp = "Expected a logical value, but a character string was provided instead")
  expect_error(object = simulate_zetas(n = 100, parameters = list(n = 25, R = 3), m = 2, statistic = mean, simplify = 5L), regexp = "Expected a logical value, but a integer value was provided instead")
  expect_error(object = simulate_zetas(n = 100, parameters = list(n = 25, R = 3), m = 2, statistic = mean, simplify = NA), regexp = "Expected a non-missing logical value, but a missing value was provided instead")
  expect_error(object = simulate_zetas(n = 100, parameters = list(n = 25, R = 3), m = 2, statistic = mean, simplify = NULL), regexp = "Expected a non-missing logical value, but a NULL was provided instead")
  expect_error(object = simulate_zetas(n = 100, parameters = list(n = 25, R = 3), m = 2, statistic = mean, simplify = list("a" = 1)), regexp = "Expected a non-missing logical value, but a list was provided instead")
  expect_error(object = simulate_zetas(n = 100, parameters = list(n = 25, R = 3), m = 2, statistic = mean, simplify = data.table("a" = 1)), regexp = "Expected a non-missing logical value, but a data.table was provided instead")
  expect_error(object = suppressWarnings(simulate_zetas(n = 100, parameters = list(n = 25, R = 3), m = 2, statistic = mean, simplify = c("incorrect", "TRUE"))) , regexp = "Expected a non-missing logical value, but a character string was provided instead")
  expect_error(object = simulate_zetas(n = -1.1, parameters = list(n = 20, R = 5), m = 3, statistic = quantile, simplify = 1, attach = 1))
  expect_error(object = simulate_zetas(n = 0, parameters = list(n = 20, R = 5), m = 3, statistic = quantile, simplify = 1, attach = 1))
  expect_error(object = simulate_zetas(n = 1, parameters = list(n = 20, R = 5), m = 3, statistic = quantile, simplify = 1, attach = 1))
  expect_error(object = simulate_zetas(n = 1.99, parameters = list(n = 20, R = 5), m = 3, statistic = quantile, simplify = 1, attach = 1))
  expect_error(object = simulate_zetas(n = 2.77, parameters = list(n = 20, R = 5), m = 3, statistic = quantile, simplify = 1, attach = 1))
  expect_error(object = simulate_zetas(n = "str", parameters = list(n = 20, R = 5), m = 3, statistic = quantile, simplify = 1, attach = 1))
  expect_error(object = simulate_zetas(n = TRUE, parameters = list(n = 20, R = 5), m = 3, statistic = quantile, simplify = 1, attach = 1))
  expect_error(object = simulate_zetas(n = NA_real_, parameters = list(n = 20, R = 5), m = 3, statistic = quantile, simplify = 1, attach = 1))
  expect_error(object = simulate_zetas(n = NULL, parameters = list(n = 20, R = 5), m = 3, statistic = quantile, simplify = 1, attach = 1))
  expect_error(object = simulate_zetas(n = 1e3L, parameters = list(n = 20, R = 5), m = 3, statistic = "raw", simplify = TRUE, attach = 0))
})

test_that(desc = "Testing warnings", code = {
  expect_warning(object = simulate_zetas(n = c(3, 1), parameters = list(n = 24, R = 4, eta = 5, eta0 = 0.5)))
  expect_warning(object = simulate_zetas(n = 10, parameters = list(n = 24, R = 4, eta = 5, eta0 = 0.5), simplify = "TRUE"))
  expect_warning(object = simulate_zetas(n = 10, parameters = list(n = 24, R = 4, eta = 5, eta0 = 0.9), simplify = TRUE, attach = "FALSE"))

  expect_warning(object = simulate_zetas(n = 10, parameters = list(n = 24, R = 4, eta = 5, eta0 = 0.5), simplify = "FALSE"))
  expect_warning(object = simulate_zetas(n = 10, parameters = list(n = 24, R = 4, eta = 5, eta0 = 0.5), simplify = c(1, 0)))
  expect_warning(object = simulate_zetas(n = 10, parameters = list(n = 24, R = 4, eta = 5, eta0 = 0.9), simplify = TRUE, attach = c(TRUE, FALSE)))

  expect_warning(object = simulate_zetas(n = c(12, 13, 15), parameters = list(n = 24, R = 4, eta = 5, eta0 = 0.5)))
})




