#' Calculate the Local Gi* for a spacetime cube
#'
#' An alternative implementation to [spdep::localG] intended for use with
#' a spacetime cube.
#'
#' @param x a numeric vector
#' @param times a vector determining time order
#' @param nb a spacetime neighbors list
#' @param wt a weights list associated with `nb`
#' @param n_locs the number of unique locations
#' @param n_sim the number of simulations to run for calculating `p_sim`
#'
#' @keywords internal
local_g_spt <- function(x, times, nb, wt, n_locs, nsim) {
  # identify observed local g values
  obs <- local_g_spt_impl(x, times, nb, wt, n_locs)
  # calculate replicates
  reps <- matrix(ncol = nsim, nrow = length(x))
  for (i in 1:nsim) {
    reps[,i] <- local_g_spt_impl(x, times, cond_permute_nb(nb), wt, n_locs)
  }

  # identify simulated p-values for both tails
  l <- (rowSums(obs >= reps)  + 1)/ (nsim + 1)
  g <- (rowSums(obs <= reps) + 1) / (nsim + 1)

  data.frame(
    gi_star = obs,
    p_sim = pmin(l, g)
  )

}

local_g_spt_impl <- function(x, times, nb, wt, n_locs) {
  xj <-find_xj(x, nb)
  starts = seq(1, length(x), by = n_locs)
  ends = starts + (n_locs - 1)
  all_gis <- numeric(length(x))
  for (i in 1:length(starts)) {
    ind <- starts[i]:ends[i]
    all_gis[ind] <- local_g_spt_calc(x[ind], xj[ind], wt[ind])
  }

  all_gis
}

local_g_spt_calc <- function(x, xj, wj) {
  # This comes from the body of spdep::localG()
  n <- length(wj)
  xibar <- rep(mean(x), n)
  lx <- mapply(xj, wj, FUN = function(x, y) sum(x * y))
  si2 <- rep(sum(scale(x, scale = FALSE)^2)/n, n)
  Wi <- sapply(wj, sum)
  S1i <- sapply(wj, function(x) sum(x^2))
  EG <- Wi * xibar
  res <- (lx - EG)
  VG <- si2 * ((n * S1i - Wi^2)/(n - 1))
  res <- res/sqrt(VG)
  res
}
