# nb <- guerry_nb$nb
# wt <- guerry_nb$wt
# x <- guerry_nb$crime_pers

# Global Measures

# Morans I
#' Calculate Global Moran's I
#'
#'
#' @inheritParams local_moran
#' @param na_ok default `FALSE`. If `FALSE` presence or `NA` or `Inf` results in an error.
#' @param ... additional arguments passed to [spdep::moran()].
#' @export
#' @family global_moran
#' @examples
#' nb <- guerry_nb$nb
#' wt <- guerry_nb$wt
#' x <- guerry_nb$crime_pers
#' moran <- global_moran(x, nb, wt)
#' @returns an `htest` object
global_moran <- function(x, nb, wt, na_ok = FALSE, ...) {
  listw <- recreate_listw(nb, wt)
  spdep::moran(x, listw = listw,
               n = length(nb),
               S0 = spdep::Szero(listw),
               NAOK = na_ok, ...)

}

#' Global Moran Permutation Test
#' @inheritParams local_moran
#' @param nsim number of simulations to run.
#' @param ... additional arguments passed to [spdep::moran.mc()]
#' @export
#' @family global_moran
#' @examples
#' nb <- guerry_nb$nb
#' wt <- guerry_nb$wt
#' x <- guerry_nb$crime_pers
#' moran <- global_moran_perm(x, nb, wt)
#' broom::tidy(moran)
#' @returns an object of classes `htest`, and `mc.sim`.
global_moran_perm <- function(x, nb, wt, alternative = "two.sided",
                              nsim = 499, ...) {
  listw <- recreate_listw(nb, wt)
  res <- spdep::moran.mc(x, listw = listw,
           nsim = nsim,
           alternative = alternative,
           ...)

  class_modify(res)
}

#' Global Moran Test
#' @inheritParams global_moran_perm
#' @param randomization default `TRUE`. Calculate variance based on randomization. If `FALSE`, under the assumption of normality.
#' @family global_moran
#' @export
#' @examples
#' nb <- guerry_nb$nb
#' wt <- guerry_nb$wt
#' x <- guerry_nb$crime_pers
#' global_moran_test(x, nb, wt)
#' @returns an object of class `htest`
global_moran_test <- function(x, nb, wt, alternative = "greater",
                              randomization = TRUE,
                              ...) {

  listw <- recreate_listw(nb, wt)
  res <- spdep::moran.test(x, listw, randomisation = randomization,
                    alternative = alternative, ...)

  class_modify(res)
}


## Bivariate

#' Compute Geary's C
#'
#' @inheritParams global_moran
#' @param allow_zero If `TRUE`, assigns zero as lagged value to zone without neighbors.
#' @export
#' @family global_c
#' @examples
#' nb <- guerry_nb$nb
#' wt <- guerry_nb$wt
#' x <- guerry_nb$crime_pers
#' global_c(x, nb, wt)
#' @returns a list with two names elements `C` and `K` returning the value of Geary's C and sample kurtosis respectively.
global_c <- function(x, nb, wt, allow_zero = NULL) {
  listw <- recreate_listw(nb, wt)
  spdep::geary(x, listw,
               length(nb), length(nb) - 1,
               spdep::Szero(listw),
               zero.policy = allow_zero)
}

#' Global C Permutation Test
#'
#' @inheritParams global_moran_perm
#' @param allow_zero If `TRUE`, assigns zero as lagged value to zone without neighbors.
#' @param ... additional arguments passed to [spdep::geary.mc()].
#' @family global_c
#' @export
#' @examples
#' geo <- sf::st_geometry(guerry)
#' nb <- st_contiguity(geo)
#' wt <- st_weights(nb)
#' x <- guerry$crime_pers
#' global_c_perm(x, nb, wt)
#' @returns an object of classes `htest` and `mc.sim`
global_c_perm <- function(x, nb, wt, nsim = 499, alternative = "greater",
                          allow_zero = NULL, ...) {
  listw <- recreate_listw(nb, wt)
  res <- spdep::geary.mc(x, listw, nsim = nsim,
                 zero.policy = allow_zero,
                 alternative = alternative,
                 ...)
  class_modify(res)
}

#' Global C Test
#' @inheritParams global_moran_test
#' @param allow_zero If `TRUE`, assigns zero as lagged value to zone without neighbors.
#' @export
#' @family global_c
#' @examples
#' geo <- sf::st_geometry(guerry)
#' nb <- st_contiguity(geo)
#' wt <- st_weights(nb)
#' x <- guerry$crime_pers
#' global_c_test(x, nb, wt)
#' @returns an `htest` object
global_c_test <- function(x, nb, wt, randomization = TRUE, allow_zero = NULL, ...) {
  listw <- recreate_listw(nb, wt)
  res <- spdep::geary.test(x, listw, randomisation = randomization,
                    zero.policy = allow_zero, ...)
  class_modify(res)
}


#' Getis-Ord Global G
#' @inheritParams global_moran_test
#' @inheritParams recreate_listw
#' @param allow_zero If `TRUE`, assigns zero as lagged value to zone without neighbors.
#' @param ... additional methods passed to [spdep::globalG.test()].
#' @export
#' @examples
#' geo <- sf::st_geometry(guerry)
#' nb <- st_contiguity(geo)
#' wt <- st_weights(nb, style = "B")
#' x <- guerry$crime_pers
#' global_g_test(x, nb, wt)
#' @returns an `htest` object
global_g_test <- function(x, nb, wt, alternative = "greater",
                          allow_zero = NULL, ...) {
  # TODO make friendlier warnings for weights type.
  listw <- recreate_listw(nb, wt)
  res <- spdep::globalG.test(x, listw,
                      zero.policy = allow_zero,
                      alternative = alternative,
                      ...)

  class_modify(res)
}



# Join Counts
#' Global Join Count Permutation Test
#'
#' @param fx a factor or character vector of the same length as nb.
#' @inheritParams global_moran_perm
#' @param allow_zero If `TRUE`, assigns zero as lagged value to zone without neighbors.
#' @param ... additional arguments passed to [spdep::joincount.mc()].
#' @export
#' @examples
#' geo <- sf::st_geometry(guerry)
#' nb <- st_contiguity(geo)
#' wt <- st_weights(nb, style = "B")
#' fx <- guerry$region
#' global_jc_perm(fx, nb, wt)
#' @returns an object of class `jclist` which is a list where each element is of class `htest` and `mc.sim`.
global_jc_perm <- function(fx, nb, wt, alternative = "greater", nsim = 499, allow_zero = FALSE, ...) {
  fx <- as.factor(fx)
  # TODO create broom tidy method
  listw <- recreate_listw(nb, wt)
  res <- spdep::joincount.mc(fx, listw, nsim = nsim,
                      zero.policy = allow_zero, ...)
  class_modify(res)
}

#' Global Join Count Test
#' @rdname global_jc_perm
#' @inheritParams global_jc_perm
#' @param allow_zero If `TRUE`, assigns zero as lagged value to zone without neighbors.
#' @param ... additional arguments passed to [spdep::joincount.test()]
#' @export
#' @examples
#' geo <- sf::st_geometry(guerry)
#' nb <- st_contiguity(geo)
#' wt <- st_weights(nb, style = "B")
#' fx <- guerry$region
#' global_jc_test(fx, nb, wt)
#' global_jc_perm(fx, nb, wt)
#' @returns an object of class `jclist` which is a list where each element is of class `htest` and `mc.sim`.
global_jc_test <- function(fx, nb, wt, alternative = "greater", allow_zero = NULL, ...) {
  fx <- as.factor(fx)
  # TODO create broom tidy method
  listw <- recreate_listw(nb, wt)
  res <- spdep::joincount.test(fx, listw,
                        alternative = alternative,
                        zero.policy = allow_zero, ...)
  class_modify(res)
}
