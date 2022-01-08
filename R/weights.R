#' Calculate spatial weights
#' @param nb A neighbor list object as created by `st_neighbors()`.
#' @param style Default `"W"` for row standardized weights.
#' @importFrom spdep nb2listw
#' @family weights
#' @export
st_weights <- function(nb, style = "W", allow_zero = NULL, ...) {

  listw <- nb2listw(nb, style = style, zero.policy = allow_zero, ...)

  listw[["weights"]]
}


#' Calculate Inverse Distance Bands
#'
#' @param x Spatial points. Typically the `geometry` column of an sf object.
#' @param nb A nb neighbor list such as created from `st_knn(x, k = 1)`.
#'
#' @details See implementation details [here](https://spatialanalysis.github.io/lab_tutorials/Spatial_Weights_as_Distance_Functions.html#kernal-weights). For more on distance band based weights see [here](https://spatialanalysis.github.io/lab_tutorials/Distance_Based_Spatial_Weights.html#creating-distance-band-weights).
#'
#' @importFrom spdep dnearneigh nbdists
#' @family weights
#' @export
st_inverse_weights <- function(x, nb, scale = 100) {
  # As implemented by Luc Anselin
  # https://spatialanalysis.github.io/lab_tutorials/Spatial_Weights_as_Distance_Functions.html#inverse-distance-weights

  threshold <- max(unlist(nbdists(nb, x)))
  dist_band <- dnearneigh(x, 0, threshold)
  distances <- nbdists(dist_band, x)
  lapply(distances, function(x) (1/(x/scale)))

}


#' Calculate Kernel Weights
#'
#' @inheritParams st_inverse_weights
#' @param kernel One of "uniform", "gaussian",  "triangular", "epanechnikov", or "quartic".
#' @importFrom spdep nbdists dnearneigh include.self
#' @family weights
#' @export
st_kernel_weight <- function(x, nb, kernel = "uniform") {

  match.arg(kernel, names(kernels))

  threshold <- max(unlist(nbdists(nb, x)))
  kernal_nb <- dnearneigh(x, 0, threshold)
  kernal_nb <- include.self(kernal_nb)
  kernal_dists <- nbdists(kernal_nb, x)
  lapply(kernal_dists, kernels[[kernel]], threshold)

}
