#' Look-up table for critcal zeta values based on study design
#'
#' A dataset containing rejection values for \code{zeta} based on 432 different combinations of study designs and
#' acceptable differences in non-selectivity
#'
#' @format A data frame with 75 rows and 7 variables:
#' \describe{
#'   \item{id}{Row ID}
#'   \item{n}{Number of clinical samples}
#'   \item{R}{Number of replicated measurements on each clinical sample}
#'   \item{M}{Average relative increase of prediction intervals caused by differences in non-selectivity}
#'   \item{zeta}{Rejection value for zeta. Any computed zeta value above this value will result in conclusion of differences in non-selectivity}
#' }
"look_up_table"