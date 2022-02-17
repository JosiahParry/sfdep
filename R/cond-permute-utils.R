#' Conditional permutation of neighbors
#'
#' Creates a conditional permutation of neighbors list holding i fixed and shuffling it's neighbors.
#'
#' @param nb a neighbor list.
#' @param seed default null. A value to pass to `set.seed()` for reproducibily.
#' @export
#' @examples
#' nb <- st_contiguity(guerry)
#'
#' # conditionally permute neighbors
#' perm_nb <- cond_permute_nb(nb)
#'
#' # get permuted neighbor weight
#' st_weights(perm_nb)
cond_permute_nb <- function(nb, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  n <- length(nb)
  cards <- lengths(nb)
  res <- mapply(shuffle_nbs, 1:n, n, cards)
  class_modify(res, "nb")
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
#' @param i the index position of observation `i`
#' @keywords internal
shuffle_nbs <- function(i, n, card) {
  x <- 1:n
  sample(x[-i], size = card)
}
