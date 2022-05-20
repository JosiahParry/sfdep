#' Local G
#'
#' Calculate the local Geary statistic for a given variable.
#'
#' @export
#' @inheritParams local_moran
#' @inheritParams recreate_listw
#' @param ... methods passed to [spdep::localG()] or [spdep::localG_perm()]
#' @examples
#' x <- guerry$crime_pers
#' nb <- st_contiguity(guerry)
#' wt <- st_weights(nb)
#'
#' res <- local_g_perm(x, nb, wt)
#'
#' head(res)
#' @returns a `data.frame` with columns:
#'
#' - `gi`: the observed statistic
#' - `e_gi`: the permutation sample mean
#' - `var_gi`: the permutation sample variance
#' - `p_value`: the p-value using sample mean and standard deviation
#' - `p_folded_sim`: p-value based on the implementation of Pysal which always assumes a two-sided test taking the minimum possible p-value
#' - `skewness`: sample skewness
#' - `kurtosis`: sample kurtosis
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

  gi <- as.numeric(res)
  stats::setNames(
    cbind("gi" = gi, as.data.frame(attr(res, "internals")[, 2:8])),
    localg_names
    )
}


