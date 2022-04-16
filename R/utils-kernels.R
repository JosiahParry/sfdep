#' Kernel functions
#'
#' Kernel functions for creating kernel based weights with [`st_kernel_weights()`]
#'
#' @details
#'
#' Supported kernels are below.
#'
#' Formulas come from Anselin & Morrison's [notes](https://spatialanalysis.github.io/lab_tutorials/Spatial_Weights_as_Distance_Functions.html#kernal-weights).
#'
#' - `uniform`:  K(z) = 1/2 for ∣z∣<1
#' - `triangular`: K(z) = (1−∣z∣) for ∣z∣ < 1
#' - `epanechnikov`: K(z) = (3/4)(1−z^2) for ∣z∣ < 1
#' - `quartic`: K(z) = (15/16)*(1−(z/threshold)^2^)2 for ∣z∣ < 1
#' - `guassian`: K(z) = (2pi)^{1/2} * exp(−z^2/2)
#'
#' @keywords internal
#' @returns a list of length 5 where each element is a kernel function.
kernels <- list(
  uniform = function(x, ...) x * 0 + .5,
  triangular = function(x, thresh) 1 - abs(x / thresh),
  epanechnikov = function(x, thresh) .75 * (1- (x / thresh)^2),
  quartic = function(x, thresh) (15/16)*(1-(x / thresh)^2)^2,
  gaussian =  function(x, thresh) sqrt(2 * pi)*exp((-(x / thresh)^2)/2)
)


#' Identify critical threshold
#'
#' Identifies the minimum distance in which each observation will have at least one neighbor.
#'
#' @param geometry an sf geometry column
#' @export
#' @returns a numeric scalar value.
#' @examples
#' critical_threshold(sf::st_geometry(guerry))
critical_threshold <- function(geometry) {
  pnts <- check_polygon(geometry)
  knb <- spdep::knn2nb(spdep::knearneigh(pnts, 1))
  max(unlist(spdep::nbdists(knb, pnts)))
}

