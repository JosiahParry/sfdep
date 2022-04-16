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
#' @param allow_zero If `TRUE`, assigns zero as lagged value to zone without neighbors.
#' @param ... additional arguments passed to [spdep::nb2listw()].
#' @importFrom spdep nb2listw
#' @family weights
#' @export
#' @examples
#' guerry %>%
#'  dplyr::mutate(nb = st_contiguity(geometry),
#'                wt = st_weights(nb),
#'                .before = 1)
#'
#' # using geometry column directly
#' nb <- st_contiguity(guerry$geometry)
#' wt <- st_weights(nb)
#' wt[1:3]
#' @returns a list where each element is a numeric vector
st_weights <- function(nb, style = "W", allow_zero = NULL, ...) {

  listw <- nb2listw(nb, style = style, zero.policy = allow_zero, ...)

  listw[["weights"]]
}

# TODO i dont think inverse distance band is correct.
# returning too many observations

#' Calculate Inverse Distance Bands
#'
#' @param x Spatial points. Typically the `geometry` column of an sf object.
#' @param nb A nb neighbor list such as created from `st_knn(x, k = 1)`.
#' @param scale The scaling factor to use when calculating the inverse distance band.
#' @param threshold The critical threshold to use for distance band.
#' @details See implementation details [here](https://spatialanalysis.github.io/lab_tutorials/Spatial_Weights_as_Distance_Functions.html#kernal-weights). For more on distance band based weights see [here](https://spatialanalysis.github.io/lab_tutorials/Distance_Based_Spatial_Weights.html#creating-distance-band-weights).
#'
# @importFrom spdep dnearneigh nbdists
#' @family weights
#st_inverse_weights <- function(x, nb, scale = 100, threshold = NULL) {
#  # As implemented by Luc Anselin
#  # https://spatialanalysis.github.io/lab_tutorials#/Spatial_Weights_as_Distance_Functions.html#inverse-distance-weights
#  x <- check_polygon(x)
#  if (is.null(threshold)) threshold <- max(unlist(spdep::nbdists(nb, x)))
#  dist_band <- spdep::dnearneigh(x, 0, threshold)
#  distances <- spdep::nbdists(dist_band, x)
#  lapply(distances, function(x) (1/(x/scale)))
#
#}
