#' Implementation of the univariate join count on listw objects
#'
#' The univariate local join count statistic is used to identify clusters of rarely occurring binary variables.
#' Compute the univariate local join count statistic for a binary variable.
#'
#' @param x a binary variable either numeric or logical
#' @param listw a listw object where `attr(, "mode")` is `"binary"`
#' @param nsim the number of conditional permutation simulations
#' @param alternative default `"greater"`. One of `"less"` or `"greater"`.
#' @export
#' @references https://geodacenter.github.io/workbook/6d_local_discrete/lab6d.html#univariate-local-join-count-statistic
jc_uni_impl <- function(x, listw, nsim = 499) {

  # TODO Check neighbor attribute and cast to Binary if need be.
  # Check for x input type  and proportion of 1s vs TRUES
  nb <- listw[["neighbours"]]
  wt <- listw[["weights"]]

  obs <- mapply(jc_uni_calc, x, xj, wt) # observed join count

  xj <- find_xj(x, nb)

  x_index <- which(x == 1L)
  xj_index <- which(unlist(lapply(xj, function(x) any(x == 1L))) == TRUE)
  index <- intersect(xj_index, x_index)

  reps <- replicate(nsim, jc_uni_perm_impl(x, listw, index))

  p_value <- switch(
    alternative,
    less = (rowSums(reps = obs[index]) + 1) / (nsim + 1),
    greater = (rowSums(reps >= obs[index]) + 1)/ (nsim + 1)
  )

  p_values <- rep(NA_real_, length(x))
  p_values[index] <- p_value

  data.frame(
    join_count = obs,
    p_value = p_values
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
#' TODO this conditional permutation indexing can be useful eslewhere if subsetting
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
#' \deqn{x_i \times \Sigma{w_i * x_j}}
#'
#' @keywords internal
jc_uni_calc <- function(x, xj, wt) {
  x * sum(wt * xj)
}


#' Idenitfy xj values
#'
#' Find `xj` values given an x and neighbors list.
#'
#' @keywords internal
find_xj <- function(x, nb) {
  lapply(nb, FUN = function(nbs_i) x[nbs_i])
}


#' Conditionally permutes a listw object
#'
#' @param a listw object.
#'
#' @keywords internal
permute_listw <- function(listw) {
  n <- length(listw$neighbours)

  cards <- lengths(listw$neighbours)
  # Shuffle the neighbors by randomly sampling from all possible neighbors
  # except where x exists
  perm_nb <- mapply(shuffle_nbs, 1:n, n, cards)
  class(perm_nb) <- c("nb", "list")
  listw$neighbours <- perm_nb

  listw
}

#' Internal function to shuffle neighbors
#'
#' Used in conditional permutation and the function `permute_listw()`.
#'
#' @param i the index position of observation $i$
shuffle_nbs <- function(i, n, card) {
  x <- 1:n
  sample(x[-i], size = card)
}



# Example data to match geoda if for debugging
# library(spdep)
# library(sf)
#
# chi <- sf::read_sf("/Users/josiahparry/Downloads/chicago_commpop/chicago_commpop.geojson")
# nbsp <- poly2nb(chi)
# listw <- nb2listw(nbsp, style = "B")
# x <- chi$popplus


