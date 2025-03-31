#' Look-up table for upper \eqn{\hat{\zeta}} values
#'
#' A dataset containing critical values of the differences in non-selectivity
#' estimator, \eqn{\hat{\zeta}}, based on 2604 unqiue combinations of study
#' designs and maximum tolerable differences in non-selectivity magnitudes.
#' That is, all combinations of \eqn{n = 20, 21, \ldots, 50},
#' \eqn{R = 2, 3, 4, 5}, and \eqn{M = 0, 0.05, 0.10, \ldots, 0.95, 1}.
#'
#' @format A data.table with 2604 rows and 4 variables:
#' \describe{
#'   \item{n}{The number of clinical samples in the study design}
#'   \item{R}{The number of replicated measurements performed on each clinical sample in the study design}
#'   \item{M}{Average relative increase of point-wise prediction intervals attributed to differences in non-selectivity}
#'   \item{zeta}{The associated lower limit of the rejection region of \eqn{\hat{\zeta}} for n, R and M}
#' }
"look_up_table"

#' Example Clinical Sample Data
#'
#' An example dataset containing clinical sample measurements for ten unique
#' IVD-MDs. \code{Thermo} is considered a reference IVD-MD.
#'
#' @format A \code{data.table} with 123 rows and 12 variables:
#' \describe{
#'   \item{SampleID}{An \code{integer} vector. The clinical sample identifiers}
#'   \item{ReplicateID}{A \code{character} vector. The replicate measurement identifiers}
#'   \item{other}{IVD-MD measurements}
#' }
"commutability_cs_data"

#' Example Evaluated Material Sample Data
#'
#' An example dataset containing evaluated material sample measurements for
#' ten unique IVD-MDs. This dataset contains data for \eqn{61} evaluated
#' materials. \code{Thermo} is considered a reference IVD-MD.
#'
#' @format A \code{data.table} with 183 rows and 12 variables:
#' \describe{
#'   \item{SampleID}{An \code{character} vector. The evaluated material sample identifiers}
#'   \item{ReplicateID}{A \code{character} vector. The replicate measurement identifiers}
#'   \item{other}{IVD-MD measurements}
#' }
"commutability_eq_data"
