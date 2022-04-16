#' Compute local univariate join count
#'
#' The univariate local join count statistic is used to identify clusters of rarely occurring binary variables. The binary variable of interest should occur less
#' than half of the time.
#' @details
#'
#' The local join count statistic requires a binary weights list which can be generated with `st_weights(nb, style = "B")`. Additionally, ensure that the binary variable of interest is rarely occurring in no more than half of observations.
#'
#' P-values are estimated using a conditional permutation approach. This creates a reference distribution from which the observed statistic is compared. For more see [Geoda Glossary](https://geodacenter.github.io/glossary.html#ppvalue).
#'
#' @inheritParams jc_uni_impl
#' @param nb a neighbors list object.
#' @param wt default `st_weights(nb, style = "B")`. A binary weights list as created by `st_weights(nb, style = "B")`.
#' @export
#' @examples
#' guerry %>%
#'   dplyr::transmute(top_crime = crime_prop > 9000,
#'                    nb = st_contiguity(geometry),
#'                    wt = st_weights(nb, style = "B"),
#'                    jc = local_jc_uni(top_crime, nb, wt)) %>%
#'   tidyr::unnest(jc)
#' @returns a `data.frame` with two columns `join_count` and `p_sim` and number of rows equal to the length of arguments `x`, `nb`, and `wt`.
local_jc_uni <- function(x, nb, wt = st_weights(nb, style = "B"),
                         nsim = 499, alternative = "two.sided") {
  listw <- recreate_listw(nb ,wt)
  jc_uni_impl(x, listw, nsim, alternative)
}

#' Implementation of the univariate join count on listw objects
#'
#' The univariate local join count statistic is used to identify clusters of rarely occurring binary variables.
#'
#' @param x a binary variable either numeric or logical
#' @param listw a listw object where `attr(, "mode")` is `"binary"`
#' @param nsim the number of conditional permutation simulations
#' @param alternative default `"greater"`. One of `"less"` or `"greater"`.
#' @keywords internal
#' @references https://geodacenter.github.io/workbook/6d_local_discrete/lab6d.html#univariate-local-join-count-statistic
jc_uni_impl <- function(x, listw, nsim, alternative) {

  # TODO Check neighbor attribute and cast to Binary if need be.
  # Check for x input type  and proportion of 1s vs TRUES
  nb <- listw[["neighbours"]]
  wt <- listw[["weights"]]
  xj <- find_xj(x, nb) # find xj values
  obs <- mapply(jc_uni_calc, x, xj, wt) # observed join count

  x_index <- which(x == 1L)
  xj_index <- which(unlist(lapply(xj, function(x) any(x == 1L))) == TRUE)
  index <- intersect(xj_index, x_index)

  reps <- replicate(nsim, jc_uni_perm_impl(x, listw, index))

  # TODO check order for greater and lesser
  # This might be incorrect

  g <- (rowSums(reps <= obs[index]) + 1) / (nsim + 1)
  l <- (rowSums(reps >= obs[index]) + 1)/ (nsim + 1)
  p_value <- switch(
    alternative,
    less = l,
    greater = g,
    two.sided = pmin(l, 1 -l)
  )

  p_values <- rep(NA_real_, length(x))
  p_values[index] <- p_value

  data.frame(
    join_count = obs,
    p_sim = p_values
  )
}



# Steps for calculating local join count:
#
# 1. Find xj
# 2. Find valid p-value indexes
# 3. Calculate observed jc
# 4. Create replications for p-value
# 5. Calculate p-values from reps
# 6. Put table together
# p-values are only reported for those where xi = 1L and have at least 1 neighbor with one xj == 1L value

#' Conditional permutation of univariate local join count
#'
#' Creates conditional permutations on an index and calculates the local univariate
#' join count for that index.
#'
#' TODO this conditional permutation indexing can be useful elsewhere if subsetting
#' is further required for other functions
#'
#' @param x binary variable
#' @param listw must be binary
#' @param index to subset
#' @keywords internal
jc_uni_perm_impl <- function (x, listw, index) {
  p_listw <- permute_listw(listw)
  wt <- p_listw[["weights"]][index]
  nb <- p_listw[["neighbours"]][index]
  xj <- find_xj(x, nb)
  x_p <- x[index]

  mapply(jc_uni_calc, x_p, xj, wt)
}


#' Calculate univariate join count at row level
#'
#' Calculates the local join count statistic for a given xi, xj, and binary weights.
#'
#' Formula is
#'
#' \eqn{x_i \times \Sigma{w_i * x_j}}
#'
#' @keywords internal
jc_uni_calc <- function(x, xj, wt) {
  x * sum(wt * xj)
}

# Example data to match geoda if for debugging
# library(spdep)
# library(sf)
#
# chi <- sf::read_sf("/Users/josiahparry/Downloads/chicago_commpop/chicago_commpop.geojson")
# nbsp <- poly2nb(chi)
# listw <- nb2listw(nbsp, style = "B")
# x <- chi$popplus
