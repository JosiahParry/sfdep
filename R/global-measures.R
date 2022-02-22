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
global_moran <- function(x, nb, wt, na_ok = FALSE, ...) {
  listw <- recreate_listw(nb, wt)
  spdep::moran(x, listw = listw,
               n = length(nb),
               S0 = spdep::Szero(listw),
               NAOK = na_ok, ...)

}

#' Global Moran Permutation Test
#' @inheritParams global_moran
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
global_moran_perm <- function(x, nb, wt, alternative = "two.sided",
                              nsim = 499, ...) {
  listw <- recreate_listw(nb, wt)
  spdep::moran.mc(x, listw = listw,
           nsim = nsim,
           alternative = alternative,
           ...)
}

#' Global Moran Test
#' @inheritParams global_moran
#' @param randomization default `TRUE`. Calculate variance based on randomization. If `FALSE`, under the assumption of normality.
#' @family global_moran
#' @export
#' @examples
#' nb <- guerry_nb$nb
#' wt <- guerry_nb$wt
#' x <- guerry_nb$crime_pers
#' global_moran_test(x, nb, wt)
global_moran_test <- function(x, nb, wt, alternative = "greater",
                              randomization = TRUE,
                              ...) {

  listw <- recreate_listw(nb, wt)
  spdep::moran.test(x, listw, randomisation = randomization,
                    alternative = alternative, ...)

}


## Bivariate

#' Compute Geary's C
#'
#' @inheritParams global_moran
#' @export
#' @family global_c
#' @examples
#' nb <- guerry_nb$nb
#' wt <- guerry_nb$wt
#' x <- guerry_nb$crime_pers
#' global_c(x, nb, wt)
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
#' @param ... additional arguments passed to [spdep::geary.mc()].
#' @family global_c
#' @export
#' @examples
#' global_c_perm(x, nb, wt)
global_c_perm <- function(x, nb, wt, nsim = 499, alternative = "greater",
                          allow_zero = NULL, ...) {
  listw <- recreate_listw(nb, wt)
  spdep::geary.mc(x, listw, nsim = nsim,
                 zero.policy = allow_zero,
                 alternative = alternative,
                 ...)
}

#' Global C Test
#' @inheritParams global_moran_test
#' @export
#' @family global_c
#' @examples
#' global_c_test(x, nb, wt)
global_c_test <- function(x, nb, wt, randomization = TRUE, allow_zero = NULL, ...) {
  listw <- recreate_listw(nb, wt)
  spdep::geary.test(x, listw, randomisation = randomization,
                    zero.policy = allow_zero, ...)
}

# Gettis-ord G
#' Getis-Ord Global G
#'
#' @export
#' @examples
#' global_g_test(x, nb, wt)
global_g_test <- function(x, nb, wt, alternative = "greater",
                          allow_zero = NULL, ...) {
  # TODO make friendlier warnings for weights type.
  listw <- recreate_listw(nb, wt)
  spdep::globalG.test(x, listw,
                      zero.policy = allow_zero,
                      alternative = alternative,
                      ...)
}



# Join Counts
#' Global Join Count Permutation Test
#'
#' @param fx a factor or character vector of the same length as nb.
#' @inheritParams global_moran_perm
#' @param ... additional arguments passed to [spdep::joincount.mc()].
#' @export
#' @examples
#' fx <- guerry$region
#' global_jc_perm(fx, nb, wt)
global_jc_perm <- function(fx, nb, wt, alternative = "greater", nsim = 499, allow_zero = FALSE, ...) {
  fx <- as.factor(fx)
  # TODO create broom tidy method
  listw <- recreate_listw(nb, wt)
  spdep::joincount.mc(fx, listw, nsim = nsim,
                      zero.policy = allow_zero, ...)
}

#' Global Join Count Test
#' @rdname global_jc_perm
#' @inheritParams global_jc_perm
#' @param ... additional arguments passed to [spdep::joincount.test()]
#' @export
#' @examples
#' global_jc_test(fx, nb, wt)
global_jc_test <- function(fx, nb, wt, alternative = "greater", allow_zero = NULL, ...) {
  fx <- as.factor(fx)
  # TODO create broom tidy method
  listw <- recreate_listw(nb, wt)
  spdep::joincount.test(fx, listw,
                        alternative = alternative,
                        zero.policy = allow_zero, ...)
}