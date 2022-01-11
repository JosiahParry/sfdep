#' Compute Local Geary statistic
#'
#' @description
#'
#' The Local Geary is a local adaptation of Geary's C statistic of spatial autocorrelation. The Local Geary uses squared differences to measure dissimilarity unlike the Local Moran. Low values of the Local Geary indicate positive spatial autocorrelation and large refers to negative spatial autocorrelation.

#' Inference for the Local Geary is based on a permutation approach which compares the observed value to the reference distribution under spatial randomness. The Local Geary creates a pseudo p-value. This is not an analytical p-value and is based on the number of permutations and as such should be used with care.
#'
#' @details
#' The Local Geary can be extended to a multivariate context. When \code{x} is a numeric vector, the univariate Local Geary will be calculated. To calculate the multivariate Local Moran provide either a list or a matrix. When \code{x} is a list, each element must be a numeric vector of the same length and of the same length as the neighbours in \code{listw}. In the case that \code{x} is a matrix the number of rows must be the same as the length of the neighbours in \code{listw}.
#'
#' While not required in the univariate context, the standardized Local Geary is calculated. The multivariate Local Geary is \emph{always} standardized.
#'
#' The univariate Local Geary is calculated as \eqn{c_i = \sum_j w_{ij}(x_i - x_j)^2} and the multivariate Local Geary is calculated as \eqn{c_{k,i} = \sum_{v=1}^{k} c_{v,i}} as described in Anselin (2019).
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
