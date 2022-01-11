#' Compute Local Geary statistic
#'
#' @param x a numeric vector, or list of numeric vectors of equal length.
#' @param nb a neighbor list
#' @param wt a weights list
#' @param ... other arguments passed to \code{spdep::localC}
local_c <- function(x, nb, wt, ...) {
  listw <- recreate_listw(nb, wt)
  spdep::localC(x, listw, ...)
}
