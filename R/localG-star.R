#' Local G*
#'
#' @inheritParams local_g
#' @export
#' @examples
#' nb <- st_contiguity(guerry)
#' wt <- st_weights(nb)
#' x <- guerry$crime_pers
#'
#' res <- local_gstar_perm(x, nb, wt)
#' head(res)
#'
#' res <- local_gstar(x, nb, wt)
#' head(res)
local_gstar <- function(x, nb, wt, alternative = "two.sided", ...) {
  if (is.null(attr(nb, "self.included"))) {
    nb <- spdep::include.self(nb)
    wt <- st_weights(nb)
  }
  suppressWarnings({
    local_g(x, nb, wt, alternative = alternative, ...)
  })

}

#' @inheritParams local_g_perm
#' @export
#' @rdname local_gstar
local_gstar_perm <- function(x, nb, wt, nsim = 499, alternative = "two.sided", ...) {
  if (is.null(attr(nb, "self.included"))) {
    nb <- spdep::include.self(nb)
    wt <- st_weights(nb)
  }

  suppressWarnings({
    local_g_perm(x, nb, wt, nsim = nsim, alternative = alternative, ...)
  })

}
