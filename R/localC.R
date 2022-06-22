#' Compute Local Geary statistic
#'
#' @description
#'
#' The Local Geary is a local adaptation of Geary's C statistic of spatial autocorrelation. The Local Geary uses squared differences to measure dissimilarity unlike the Local Moran. Low values of the Local Geary indicate positive spatial autocorrelation and large refers to negative spatial autocorrelation.

#' Inference for the Local Geary is based on a permutation approach which compares the observed value to the reference distribution under spatial randomness. The Local Geary creates a pseudo p-value. This is not an analytical p-value and is based on the number of permutations and as such should be used with care.
#'
#' @details
#'
#' ## Overview
#'
#' The Local Geary can be extended to a multivariate context. When \code{x} is a numeric vector, the univariate Local Geary will be calculated. To calculate the multivariate Local Moran provide either a list or a matrix. When \code{x} is a list, each element must be a numeric vector of the same length and of the same length as the neighbours in \code{listw}. In the case that \code{x} is a matrix the number of rows must be the same as the length of the neighbours in \code{listw}.
#'
#' While not required in the univariate context, the standardized Local Geary is calculated. The multivariate Local Geary is \emph{always} standardized.
#'
#' The univariate Local Geary is calculated as \eqn{c_i = \sum_j w_{ij}(x_i - x_j)^2} and the multivariate Local Geary is calculated as \eqn{c_{k,i} = \sum_{v=1}^{k} c_{v,i}} as described in Anselin (2019).
#'
#' ## Implementation
#'
#' These functions are based on the implementations of the local Geary statistic in the development version of spdep. They are based on [spdep::localC] and [spdep::localC_perm].
#'
#' [spdep::localC_perm] and thus [local_c_perm] utilize a conditional permutation approach to approximate a reference distribution where each observation `i` is held fixed, randomly samples neighbors, and calculated the local C statistic for that tuple (`ci`). This is repeated `nsim` times. From the simulations 3 different types of p-values are calculated—all of which have their potential flaws. So be _extra judicious_ with using p-values to make conclusions.
#'
#' - `p_ci`: utilizes the sample mean and standard deviation. The p-value is then calculated using `pnorm()`--assuming a normal distribution which isn't always true.
#' - `p_ci_sim`: uses the rank of the observed statistic.
#' - `p_folded_sim`: follows the pysal implementation where p-values are in the range of \[0, 0.5\]. This excludes 1/2 of all p-values and should be used with caution.
#'
#' @author Josiah Parry, \email{josiah.parry@gmail.com}
#' @references {Anselin, L. (1995), Local Indicators of Spatial Association—LISA. Geographical Analysis, 27: 93-115. \doi{10.1111/j.1538-4632.1995.tb00338.x}}
#'
#' {Anselin, L. (2019), A Local Indicator of Multivariate Spatial Association: Extending Geary's c. Geogr Anal, 51: 133-150. \doi{10.1111/gean.12164}}
#' @export
#' @param x a numeric vector, or list of numeric vectors of equal length.
#' @param nb a neighbor list
#' @param wt a weights list
#' @param ... other arguments passed to \code{spdep::localC}
local_c <- function(x, nb, wt, ...) {
  listw <- recreate_listw(nb, wt)
  spdep::localC(x, listw, ...)
}

#' Compute Local Geary Statistic
#' @param nsim The number of simulations used to generate reference distribution.
#' @param alternative A character defining the alternative hypothesis. Must be one of "two.sided", "less" or "greater".
#' @param ... other arguments passed to [spdep::localC_perm()], e.g. `zero.policy = TRUE` to allow for zones without neighbors.
#' @rdname local_c
#' @export
#' @examples
#' g <- dplyr::transmute(guerry,
#'                       nb = st_contiguity(geometry),
#'                       wt = st_weights(nb),
#'                       geary = local_c_perm(
#'                         x = list(crime_pers, literacy), nb, wt)
#' )
#'
#' tidyr::unnest(g, geary)
#'
#' @returns a `data.frame` with columns
#'
#' - `ci`: Local Geary statistic
#' - `e_ci`: expected value of the Local Geary based on permutations
#' - `z_ci`: standard deviation based on permutations
#' - `var_ci`: variance based on permutations
#' - `p_ci`: p-value based on permutation sample standard deviation and means
#' - `p_ci_sim`: p-value based on rank of observed statistic
#' - `p_folded_sim`: p-value based on the implementation of Pysal which always assumes a two-sided test taking the minimum possible p-value
#' - `skewness`: sample skewness
#' - `kurtosis`: sample kurtosis
local_c_perm <- function(x, nb, wt, nsim = 499, alternative = "two.sided", ...) {

  lc_cols <- c("ci", "cluster", "e_ci", "var_ci", "z_ci", "p_ci",
               "p_ci_sim", "p_folded_sim", "skewness", "kurtosis")
  listw <- recreate_listw(nb, wt)
  lcp <- spdep::localC_perm(x, listw, nsim = nsim, alternative = alternative, ...)
  res <- cbind(ci = as.numeric(lcp),
               cluster = attr(lcp, "cluster"),
               as.data.frame(attr(lcp, "pseudo-p")))

  stats::setNames(res, lc_cols)
}
