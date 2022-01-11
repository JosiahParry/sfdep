#' Calculate the Local Moran's I Statistic

#' @param x A numeric vector.
#' @inheritParams recreate_listw
#' @param ... See `?spdep::localmoran()` for more options.
#' @importFrom spdep localmoran_perm
#' @family stats
#' @return
#' @export
#' @examples
#' library(tidyverse)
#'
#' lisa <- sfweight::acs %>%
#'   mutate(nb = st_contiguity(geometry),
#'          wt = st_weights(nb),
#'          moran = local_moran(med_house_income, nb, wt))
#'
#' pluck(lisa, "moran")
local_moran <- function(x, nb, wt, alternative = "two.sided",
                        nsim = 499,
                        conditional = TRUE, ...) {

  listw <- recreate_listw(nb, wt)

  lmp <- localmoran_perm(x,
                  listw,
                  nsim = nsim,
                  alternative = alternative,
                  conditional = conditional,
                  ...)


  lm_cols <- c("ii", "eii", "var_ii", "z_ii", "p_ii",
               "p_ii_sim", "p_folded_sim", "skewness", "kurtosis")

  lm_res <- setNames(data.frame(lmp), lm_cols)
  lm_cats <- attr(lmp, "quadr")

  cbind(lm_res, lm_cats)

}

