#' Local G
#'
#' @export
#' @inheritParams local_moran
#' @inheritParams recreate_listw
#' @param ... methods passed to [spdep::localG()] or [spdep::localG_perm()]
#' x <- guerry$crime_pers
#' nb <- st_contiguity(guerry)
#' wt <- st_weights(nb)
local_g <- function(x, nb, wt, alternative = "two.sided", ...) {
  if (!is.null(attr(nb, "self.included"))) {
    cli::cli_alert_warning("attr `self.include` is `TRUE`. Reporting Gi*.")
  }
  listw <- recreate_listw(nb, wt)
  spdep::localG(x, listw, alternative = alternative, ...)
}


#' @export
#' @rdname local_g
local_g_perm <- function(x, nb, wt, nsim = 499, alternative = "two.sided", ...) {
  if (!is.null(attr(nb, "self.included"))) {
    cli::cli_alert_warning("attr `self.include` is `TRUE`. Reporting Gi*.")
  }
  listw <- recreate_listw(nb, wt)
  res <- spdep::localG_perm(x, listw, nsim = nsim, alternative = alternative, ...)
  localg_names <- c("gi", "e_gi", "var_gi", "p_value",
                    "p_sim", "p_folded_sim", "skewness", "kurtosis")

  setNames(as.data.frame(attr(res, "internals")), localg_names)
}


