#' Calculate spatial weights
#'
#' Calculate polygon spatial weights from a `nb` list. See [spdep::nb2listw()] for further details.
#'
#' @details
#'
#' Under the hood, [st_weights()] creates a `listw` object and then extracts the weights elements from it as the `neighbours` element is already--presumably--already existent in the neighbors list you've already created. `listw` objects are recreated using [recreate_listw()] when calculating other statistics.
#'
#' @param nb A neighbor list object as created by `st_neighbors()`.
#' @param style Default `"W"` for row standardized weights. This value can also be "B", "C", "U", "minmax", and "S". See [spdep::nb2listw()] for details.
#' @importFrom spdep nb2listw
#' @family weights
#' @export
#' @examples
#' guerry %>%
#'  dplyr::mutate(nb = st_contiguity(geometry),
#'                wt = st_weights(nb))
#'
#' # using geometry column directly
#' nb <- st_contiguity(guerry$geometry)
#' wt <- st_weights(nb)
st_weights <- function(nb, style = "W", allow_zero = NULL, ...) {

  listw <- nb2listw(nb, style = style, zero.policy = allow_zero, ...)

  listw[["weights"]]
}


#' Calculate Inverse Distance Bands
#'
#' @param x Spatial points. Typically the `geometry` column of an sf object.
#' @param nb A nb neighbor list such as created from `st_knn(x, k = 1)`.
#' @param scale The scaling factor to use when calculating the inverse distance band.
#' @param threshold The critical threshold to use for distance band.
#' @details See implementation details [here](https://spatialanalysis.github.io/lab_tutorials/Spatial_Weights_as_Distance_Functions.html#kernal-weights). For more on distance band based weights see [here](https://spatialanalysis.github.io/lab_tutorials/Distance_Based_Spatial_Weights.html#creating-distance-band-weights).
#'
#' @importFrom spdep dnearneigh nbdists
#' @family weights
#' @export
st_inverse_weights <- function(x, nb, scale = 100, threshold = NULL) {
  # As implemented by Luc Anselin
  # https://spatialanalysis.github.io/lab_tutorials/Spatial_Weights_as_Distance_Functions.html#inverse-distance-weights
  if (is.null(threshold)) threshold <- max(unlist(nbdists(nb, x)))
  dist_band <- dnearneigh(x, 0, threshold)
  distances <- nbdists(dist_band, x)
  lapply(distances, function(x) (1/(x/scale)))

}


#' Calculate Kernel Weights
#'
#' @details
#'
#' By default [st_kernel_weight()] utilizes a critical threshold of the maximum neighbor distance. If desired, the critical threshold can be specified manually. The `threshold` will be passed to the underlying kernel.
#'
#' See [kernels] for more.
#'
#' @inheritParams st_inverse_weights
#' @param kernel One of "uniform", "gaussian",  "triangular", "epanechnikov", or "quartic".
#' @importFrom spdep nbdists dnearneigh include.self
#' @family weights
#' @export
st_kernel_weights <- function(x, nb, kernel = "uniform", threshold = NULL) {

  match.arg(kernel, names(kernels))

  # set threshold if not set
  if (is.null(threshold)) threshold <- max(unlist(nbdists(nb, x)))

  kernal_nb <- dnearneigh(x, 0, threshold)
  kernal_nb <- include.self(kernal_nb)
  kernal_dists <- nbdists(kernal_nb, x)
  lapply(kernal_dists, kernels[[kernel]], threshold)

}
