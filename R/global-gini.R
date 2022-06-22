#' Spatial Gini Index
#'
#' Calculates the spatial Gini index for a given numeric vector and neighbor list.
#' Based on the formula provided Rey and Smith (2013).
#'
#' The Gini index is a global measure of inequality based on the Lorenz curve. It ranges between 0 and 1 where 0 is perfect equality and 1 is perfect inequality.
#'
#' The spatial Gini index decomposes the Gini coefficient based on spatial neighbors.
#'
#' @references
#' \doi{http://dx.doi.org/10.1007/s12076-012-0086-z}
#'
#' @param x a numeric vector without missing values
#' @param nb a neighbor list, for example created with [`st_contiguity()`]
#'
#' @examples
#' nb <- st_contiguity(guerry)
#' x <- guerry$wealth
#' spatial_gini(x, nb)
#'
#' @returns
#' A scalar numeric.
#' @export
spatial_gini <- function(x, nb) {

  # cast as a matrix
  w <- nb_as_matrix(nb)

  # constants
  xbar <- mean(x, na.rm = TRUE)
  n <- length(x)

  # instantiate vectors to fill
  lhs_num <- numeric(n)
  rhs_num <- numeric(n)

  for (i in 1:n) {
    xi <- x[i]
    wij <- w[i,]
    xdiff <- abs(xi - x)
    lhs_num[i] <- sum(xdiff * wij)
    rhs_num[i] <- sum(xdiff * (1 - wij))
  }

  # calculate denominator
  denom <- 2 * (n^2) * xbar

  # result
  (sum(lhs_num) / denom) + (sum(rhs_num) / denom)
}
