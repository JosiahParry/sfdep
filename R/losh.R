#' Local spatial heteroscedacity
#'
#' @param x a numeric vector.
#' @param nb a neighbor list for example created by [st_contiguity()]
#' @param wt a weights list for example created by [st_weights()]
#' @param ... methods passed to [spdep::LOSH]
#' @export
#' @examples
#' nb <- st_contiguity(guerry)
#' wt <- st_weights(nb)
#' x <- guerry$crime_pers
#' losh(x, nb, wt)
#' losh(x, nb, wt, var_hi = FALSE)

losh <- function(x, nb, wt, a = 2, ...) {
  listw <- recreate_listw(nb, wt)
  # capture dots
  dots <- rlang::list2(...)
  # if var_hi set fewer columns are provided
  if (!is.null(dots[["var_hi"]]) && dots[["var_hi"]] == FALSE) {
    losh_names <- c("hi", "x_bar_i", "ei")
  } else {
    losh_names <- c("hi", "e_hi", "var_hi", "z_hi", "x_bar_i", "ei")
  }
  res <- as.data.frame(spdep::LOSH(x, listw, a = a, ...))
  setNames(res, losh_names)
}


#' @inheritParams losh
#' @param nsim number of simulations
#' @rdname losh
#' @export
#' @examples
#' losh_perm(x, nb, wt, nsim = 49)
losh_perm <- function(x, nb, wt, a = 2, nsim = 499, ...) {
  listw <- recreate_listw(nb, wt)
  res <- as.data.frame(spdep::LOSH.mc(x, listw, a = a, nsim = nsim, ...))
  setNames(res, c("hi", "x_bar_i", "ei", "p_sim"))
}


# TODO implement LOSH Chi-Sq