#' Bivariate local join count
#'
#' @param x a binary variable either numeric or logical
#' @param z a binary variable either numeric or logical
#' @inheritParams local_jc_uni
#' @export
#' @examples
#' x <- as.integer(guerry$infants > 23574)
#' z <- as.integer(guerry$donations > 10973)
#' nb <- st_contiguity(guerry)
#' wt <- st_weights(nb, style = "B")
#' local_jc_bv(x, z, nb, wt)
#' @returns a `data.frame` with two columns `join_count` and `p_sim` and number of rows equal to the length of arguments `x`, `z`, `nb`, and `wt`.
local_jc_bv <- function(x, z, nb, wt, nsim = 499) {


  case <- ifelse(any(x + z > 1), "CLC", "BJC")


  xj <- find_xj(x, nb)
  zj <- find_xj(z, nb)

  if (case == "BJC") {
    obs <- jc_bjc_calc(x, xj, z, zj, wt)
    index <- which(x == 1L & obs != 0)
    reps <- replicate(nsim, jc_bjc_perm_impl(x, z, recreate_listw(nb, wt), index))
  } else if (case == "CLC") {
    # matches Pysal Join_Counts_Local_BV
    obs <- jc_clc_calc(x, xj, z, zj, wt)
    index <- which(obs > 0)
    reps <- replicate(nsim, jc_clc_perm_impl(x, z, recreate_listw(nb, wt), index))
  }

  l <- (rowSums(obs[index] <= reps) + 1)/ (nsim + 1)
  ps <- pmin(l, 1 -l ) # p-values match pysal

  p_vals <- rep(NA_real_, length(x))
  p_vals[index] <- ps

  data.frame(
    join_count = obs,
    p_sim = p_vals
  )
}


# BJC  --------------------------------------------------------------------

#' Calculate BJC Bivariate Case
#'
#' Assumes no colocation
#' @keywords internal
jc_bjc_calc <- function(x, xj, z, zj, wt) {
  (x * (1 - z)) * mapply(function(wij, zj, xj) sum(wij*zj * (1-xj)),
                         wt, zj, xj)
}

#' Calculate BJC BV for conditional permutations
#' @keywords internal
jc_bjc_perm_impl <- function(x, z, listw, index) {
  p_listw <- permute_listw(listw)
  wt_p <- p_listw[["weights"]][index]
  nb_p <- p_listw[["neighbours"]][index]
  xj_p <- find_xj(x, nb_p)
  zj_p <- find_xj(z, nb_p)
  x_p <- x[index]
  z_p <- z[index]

  jc_bjc_calc(x_p, xj_p, z_p, zj_p, wt_p)

}


# CLC ---------------------------------------------------------------------
jc_clc_calc <- function(x, xj, z, zj, wt) {
  (x * z) * mapply(function(wij, xj, zj) sum(wij * xj * zj), wt, xj, zj)
}

#' Calculate CLC BV for conditional permutations
#'
#' @param x  a binary variable consisting of 1 and 0, or `TRUE` and `FALSE`.
#' @param z  a binary variable consisting of 1 and 0, or `TRUE` and `FALSE`.
#' @param listw a `listw` object
#' @param index an integer vector identifying positions to subset.
#' @keywords internal
jc_clc_perm_impl <- function(x, z, listw, index) {
  p_listw <- permute_listw(listw)
  wt_p <- p_listw[["weights"]][index]
  nb_p <- p_listw[["neighbours"]][index]
  xj_p <- find_xj(x, nb_p)
  zj_p <- find_xj(z, nb_p)
  x_p <- x[index]
  z_p <- z[index]

  jc_clc_calc(x_p, xj_p, z_p, zj_p, wt_p)

}

# reps <- replicate(nsim, jc_clc_perm_impl(x, z,recreate_listw(nb, wt), index))
# l <- (rowSums(obs[index] <= reps) + 1)/ (nsim + 1)
# pmin(l, 1 -l ) # p-values approximately match pysal. Nice.


# https://pysal.org/esda/_modules/esda/join_counts_local_bv.html#Join_Counts_Local_BV

# https://geodacenter.github.io/workbook/6d_local_discrete/lab6d.html#co-location-join-count-statistic



