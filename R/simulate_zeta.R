#' Simulate an Estimated zeta Value
#'
#' This function simulates a zeta value based on provided simulation parameters. It is mainly used internally
#' within the \code{simulate_zetas()} function due to the requirements of the \code{boot} package.
#'
#' @param parameters A \code{list}, \code{data.table}, or \code{data.frame} containing the simulation
#' parameters for \code{simulate_eqa_data()}. Refer to \code{?simulate_eqa_data} for available options
#' and valid syntax.
#'
#' @param simulation_indices A numeric \code{vector} used by the \code{boot} function from the \code{boot}
#' package. While it is not mandatory to specify this parameter, it becomes relevant when using the
#' \code{boot} function, which is why the parameter exists.
#'
#' @details
#' For more comprehensive functionality when simulating zeta values, consider using the
#' \code{simulate_zetas()} function. This function is primarily designed for internal use in conjunction
#' with the \code{boot} function from the \code{boot} package.
#'
#' @return Returns the estimated zeta value, a \code{float}, derived from the given simulation parameters.
#'
#' @export
#'
#' @examples
#' # Simulate a zeta value for a pair of IVD-MDs susceptible to random DINS
#' simulate_zeta(parameters = list(n = 25, R = 3, prop = 0.05, mmax = 5))
#'
#' # Simulate a zeta value for a pair of IVD-MDs susceptible to upper systematic DINS
#' simulate_zeta(parameters = list(n = 20, R = 4, qpos = 1, qran = 0.2, mmax = 5))


simulate_zeta <- function(parameters, simulation_indices){
  return(estimate_zeta(data = simulate_eqa_data(parameters = parameters))$zeta)
}
