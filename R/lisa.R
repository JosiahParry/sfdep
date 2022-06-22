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
#' @param alternative default `"two.sided"`. Should be one of `"greater"`, `"less"`, or `"two.sided"` to specify the alternative hypothesis.
#' @param nsim The number of simulations to run.
#' @param ... See `?spdep::localmoran_perm()` for more options.
#' @importFrom spdep localmoran_perm
#' @family stats
#' @export
#' @examples
#' library(magrittr)
#' lisa <- guerry %>%
#'   dplyr::mutate(nb = st_contiguity(geometry),
#'          wt = st_weights(nb),
#'          moran = local_moran(crime_pers, nb, wt))
#'
#' # unnest the dataframe column
#' tidyr::unnest(lisa, moran)
#' @returns a `data.frame` containing the columns `ii`, `eii`, `var_ii`, `z_ii`, `p_ii`, `p_ii_sim`, and `p_folded_sim`. For more details please see [spdep::localmoran_perm()].
#'
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

  lm_res <- stats::setNames(data.frame(lmp), lm_cols)
  lm_cats <- attr(lmp, "quadr")

  cbind(lm_res, lm_cats)

}
