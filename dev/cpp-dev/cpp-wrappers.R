perm_lw_cpp <- function(listw) {
  nb <- listw[["neighbours"]]
  n <- length(nb)
  perm_nb <- cond_permute(1:n, n, cards = lengths(nb))
  class(perm_nb) <- c("nb", "list")
  listw[["neighbours"]] <- perm_nb
  listw
}
# local_moran_bv_calc(scale(x), find_xj(y, nb), wt)

#' Local Bivariate Moran's I conditional permutation implementation
#' @inheritParams moran_bv_perm_impl
#' @keywords internal
lm_bv_perm_impl <- function(x, y, listw) {
  p_listw <- permute_listw(listw)

  nb <- p_listw[["neighbours"]]
  wt <- p_listw[["weights"]]

  p_yj <- find_xj(y, nb)

  lm_bv_calc(x, p_yj, wt)
}

# local_moran_bv_perm_impl(x, y, listw)
#' Local Bivariate Moran's I spdep implementation
#' @inheritParams moran_bv_impl
#' @keywords internal
lm_bv_impl <- function(x, y, listw, nsim) {

  x <- as.numeric(scale(x))
  y <- as.numeric(scale(y))
  nb <- listw[["neighbours"]]
  wt <- listw[["weights"]]

  obs <- lm_bv_calc(x, find_xj(y, nb),  wt)
  reps <- replicate(nsim, lm_bv_perm_impl(x, y, listw))
  p_sim <- (rowSums(obs <= reps) + 1 )/ (nsim + 1)

  data.frame("Ib" = obs,
             p_sim = pmin(p_sim, 1 - p_sim)
  )
}
