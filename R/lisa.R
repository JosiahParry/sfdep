#' Calculate the Local Moran's I Statistic
#'
#' Moran's I is calculated for each polygon based on the neighbor and weight lists.
#'
#' @details
#'
#' [local_moran()] calls [spdep::localmoran_perm()] and calculates the Moran I for each polygon. As well as provide simulated p-values.
#'
#' @inheritParams recreate_listw
#' @param x A numeric vector.
#' @param nsim The number of simulations to run.
#' @param ... See `?spdep::localmoran_perm()` for more options.
#' @importFrom spdep localmoran_perm
#' @family stats
#' @return
#' @export
#' @examples
#' library(tidyverse)
#'
#' lisa <- guerry %>%
#'   mutate(nb = st_contiguity(geometry),
#'          wt = st_weights(nb),
#'          moran = local_moran(crime_pers, nb, wt))
#'
#' # unnest the dataframe column
#' tidyr::unnest(lisa, moran)
local_moran <- function(x, nb, wt, alternative = "two.sided",
                        nsim = 499, ...) {

  listw <- recreate_listw(nb, wt)

  lmp <- localmoran_perm(x,
                  listw,
                  nsim = nsim,
                  alternative = alternative,
                  ...)


  lm_cols <- c("ii", "eii", "var_ii", "z_ii", "p_ii",
               "p_ii_sim", "p_folded_sim", "skewness", "kurtosis")

  lm_res <- setNames(data.frame(lmp), lm_cols)
  lm_cats <- attr(lmp, "quadr")

  cbind(lm_res, lm_cats)

}
