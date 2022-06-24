# need Szero inspired by
# https://geographicdata.science/book/notebooks/04_spatial_weights.html

#' Global sum of weights
#'
#' Calculate the global sum of weights
#' @param wt a weights list—i.e. created by `st_weights()`
#' @returns a scalar double
#' @export
#' @examples
#' nb <- st_contiguity(guerry)
#' wt <- st_weights(nb)
#' szero(wt)
szero <- function(wt) {
  sum(unlist(wt))
}

#' Percent Non-zero Neighbors
#'
#' Calculate the percentage of non-zero neighbors in a neighbor list.
#'
#' @param nb a neighbor list object
#' @export
#' @returns a scalar double
#' @examples
#' nb <- st_contiguity(guerry)
#' pct_nonzero(nb)
pct_nonzero <- function(nb) {
  (sum(lengths(nb)) / length(nb)^2) * 100
}
