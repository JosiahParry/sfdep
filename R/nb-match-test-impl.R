#' @title Local Neighbor Match Test
#'
#' @description Implements the Local Neighbor Match Test as described in _Tobler's Law in a Multivariate World_ (Anselin and Li, 2020).
#'
#' @inheritParams recreate_listw
#' @inheritParams nmt_impl
#' @examples
#'
#' if (requireNamespace("dplyr", quietly = TRUE)) {
#' library(magrittr)
#' guerry %>%
#'   dplyr::transmute(nb = st_knn(geometry, k = 10),
#'          nmt = nb_match_test(list(crime_pers, literacy, suicides),
#'                              nb, nsim = 999)) %>%
#'   tidyr::unnest(nmt)
#'  }
#' @export
#' @returns a `data.frame` with columns
#'
#' - `n_shared` (integer): the number of shared neighbors between geographic and attribute space
#' - `nb_matches` (list): matched neighbor indexes. Each element is an integer vector of same length as the ith observation of `n_shared`
#' - `knn_nb` (list): the neighbors in attribute space
#' - `probability` (numeric): the geometric probability of observing the number of matches
#' - `p_sim` (numeric): a folded simulated p-value
nb_match_test <- function(x, nb, wt = st_weights(nb),
                          k = 10, nsim = 499,
                          scale = TRUE, .method = "euclidian",
                          .p = 2) {

  if (inherits(x, "numeric")) x <- list(x)

  listw <- recreate_listw(nb, wt)

  nmt_impl(x, k, listw, nsim, scale, .method, .p)
}


#' Identify matches between two neighbor lists
#'
#' @param knn_nb a list with the same length as number of observations in the used dataset
#' @param nb a list the same length as knn_nb.
#'
#' @keywords internal
#' @return an integer vector
nmt_calc <- function(knn_nb, nb) {
  # identify matches
  mapply(base::intersect, knn_nb, nb)
}



#' Find conditionally permuted neighbor matches

#' Given a kNN attribute neighbor list and a listw object, find the number of matches given a conditional permutation.
#'
#' @param knn_nb a list with numeric elements. For example as made by [`dbscan::adjacencylist()`]
#' @param listw a listw object likely created by [recreate_listw].
#' @keywords internal
#' @return an integer vector
nmt_perm_impl <- function(knn_nb, listw) {
  p_listw <- permute_listw(listw)
  p_nb <- p_listw[["neighbours"]]
  nmt_calc(knn_nb, p_nb)
}


#' Implementation of Neighbor Match Test
#'
#' @param x a numeric vector or a list of numeric vectors of equal length.
#' @param k the number of neighbors to identify in attribute space. Should be the same as number of neighbors provided in [st_knn].
#' @param nsim the number of simulations to run for calculating the simulated p-value.
#' @param scale default `TRUE`. Whether `x` should be scaled or not. Note that measures should be standardized.
#' @param .method default `"euclidian"`. The distance measure passed to [stats::dist()].
#' @param .p default 2. The power of Minkowski distance passed to the `p` argument in [stats::dist()].
#'
#' @keywords internal
#' @returns a data frame containing columns:
#'   - n_shared
#'   - nb_matches
#'   - knn_nb
#'   - probability
#'   - p_sim
nmt_impl <- function(x, k, listw, nsim = 199,
                     scale = TRUE, .method = "euclidian",
                     .p = 2) {

  if (!requireNamespace("dbscan")) rlang::abort("Package `dbscan` must installed.")
  nb <- listw[["neighbours"]]

  m <- Reduce(cbind.data.frame, x)
  if (scale) m <- scale(m)

  # kNN in attribute space
  d <- stats::dist(m, .method, p = .p)
  kd <- dbscan::kNN(d, k = k)
  # find neighbor ids
  knn_nb <- dbscan::adjacencylist(kd)

  # find observed matches
  obs <- nmt_calc(knn_nb, nb)

  # create replicates
  reps <- replicate(nsim, lengths(nmt_perm_impl(knn_nb, listw)))

  g <- (rowSums(lengths(obs) >= reps) + 1) / (nsim + 1)
  l <- (rowSums(lengths(obs) <= reps) + 1) / (nsim + 1)

  # find simulated p value
  p_sim <- pmin(g, l)

  # find probability
  N <- attr(d, "Size") - 1
  v <- lengths(obs)
  probability <- choose(k, v) * choose(N - k, k - v) / choose(N, k)

  res <- list(
    n_shared = lengths(obs),
    nb_matches = obs,
    knn_nb = knn_nb,
    probability = probability,
    p_sim = p_sim
  )

  list2DF(res)

}


# TODO create this function
# Folded Simulated P-value
#
# Given observed statistics and replicates, calculated a simulated folded p value.
# @param obs observed values
# @param reps a matrix
# folded_p_sim <- function(obs, reps) {
# }

