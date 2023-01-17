#' Compute local univariate join count
#'
#' The univariate local join count statistic is used to identify clusters of rarely occurring binary variables. The binary variable of interest should occur less
#' than half of the time.
#' @details
#'
#' The local join count statistic requires a binary weights list which can be generated with `st_weights(nb, style = "B")`. Additionally, ensure that the binary variable of interest is rarely occurring in no more than half of observations.
#'
#' P-values are estimated using a conditional permutation approach. This creates a reference distribution from which the observed statistic is compared. For more see [Geoda Glossary](https://geodacenter.github.io/glossary.html#ppvalue).

#' Calls `spdep::local_joincount_uni()`.
#'
#' @inheritParams spdep::local_joincount_uni
#' @param nb a neighbors list object.
#' @param wt default `st_weights(nb, style = "B")`. A binary weights list as created by `st_weights(nb, style = "B")`.
#' @export
#' @examples
#'
#' if (requireNamespace("dplyr", quietly = TRUE)) {
#'
#' res <- dplyr::transmute(
#'   guerry,
#'   top_crime = as.factor(crime_prop > 9000),
#'   nb = st_contiguity(geometry),
#'   wt = st_weights(nb, style = "B"),
#'   jc = local_jc_uni(top_crime, "TRUE", nb, wt))
#' tidyr::unnest(res, jc)
#'
#' }
#' @returns a `data.frame` with two columns `join_count` and `p_sim` and number of rows equal to the length of arguments `x`, `nb`, and `wt`.
local_jc_uni <- function(fx, chosen, nb, wt = st_weights(nb, style = "B"),
                         nsim = 499, alternative = "two.sided", iseed = NULL) {
  listw <- recreate_listw(nb ,wt)
  spdep::local_joincount_uni(fx, chosen, listw, alternative, nsim, iseed)
}
