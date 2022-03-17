#' @keywords internal
`%||%` <- function (x, y) {
  if (rlang::is_null(x))
    y
  else x
}

#' Modify object classes
#'
#' Appends classes to exist object classes. This is utilized to aid in adding a list class to objects created by spdep. This enables to use of the returned objects within data frames and tibbles.
#'
#' @param x an object to modify
#' @param class a character vector of classes to append to an object
#' @keywords internal
class_modify <- function(x, class = "list") {
  current <- class(x)
  class(x) <- c(current, class)
  x
}

#' Create a listw object from a neighbors and weight list
#'
#' @param nb a neighbor list object for example as created by `st_contiguity()`.
#' @param wt a weights list as created by `st_weights()`.
#' @keywords internal
recreate_listw <- function(nb, wt) {
  which_style <- c(attr(wt, "W") %||% NA,
                   attr(wt, "B") %||% NA,
                   attr(wt, "C") %||% NA,
                   attr(wt, "U") %||% NA,
                   attr(wt, "minmax") %||% NA,
                   attr(wt, "S") %||% NA)

  possible_styles <- c("W", "B", "C", "U", "minmax", "S")

  listw <- list(style = possible_styles[!is.na(which_style)],
                neighbours = nb,
                weights = wt)

  class(listw) <- c("listw", "nb", "list")

  listw
}

##' Match kernel functions
##'
##' Kernel functions for creating kernel based weights with [`t_kernel_weight()`.
##'
##' @details
##'
##' Supported kernels are below.
##'
##' Formulas come from Anselin & Morrison's [notes](https://spatialanalysis.github.io/lab_tutorials/atial_Weights_as_Distance_Functions.html#kernal-weights).
##'
##' - `uniform`:  K(z) = 1/2 for ∣z∣<1
##' - `triangular`: K(z) = (1−∣z∣) for ∣z∣ < 1
##' - `epanechnikov`: K(z) = (3/4)(1−z^2) for ∣z∣ < 1
##' - `quartic`: K(z) = (15/16)*(1−(z/threshold)^2^)2 for ∣z∣ < 1
##' - `guassian`: K(z) = (2pi)^{1/2} * exp(−z^2/2)
##'
##' @param x kernel distances
##' @param thresh critical threshold
##' @param ... unused
#kernels <- list(
#  uniform = function(x, ...) x * 0 + .5,
#  triangular = function(x, thresh) 1 - abs(x / thresh),
#  epanechnikov = function(x, thresh) .75 * (1- (x / thresh)^2),
#  quartic = function(x, thresh) (15/16)*(1-(x/thresh)^2)^2,
#  gaussian =  function(x, thresh) sqrt(2*pi)*exp((-(x/thresh)^2)/2)
#)


#' Idenitfy xj values
#'
#' Find `xj` values given an x and neighbors list.
#'
#' @keywords internal
find_xj <- function(x, nb) {
  lapply(nb, FUN = function(nbs_i) x[nbs_i])
}


#' Conditional permutation of neighbors
#'
#' Creates a conditional permutation of neighbors list holding i fixed and shuffling it's neighbors.
#'
#' @param nb a neighbor list.
#' @param seed default null. A value to pass to `set.seed()` for reproducibily.
#' @export
#' @examples
#' nb <- st_contiguity(guerry)
#' nb[1:5]
#' # conditionally permute neighbors
#' perm_nb <- cond_permute_nb(nb)
#' perm_nb[1:5]
#' # get permuted neighbor weight
cond_permute_nb <- function(nb, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  n <- length(nb)
  cards <- lengths(nb)
  res <- mapply(shuffle_nbs, 1:n, n, cards)
  class_modify(res, "nb")
}



# Conditional Permutation -------------------------------------------------

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
  perm_nb <- mapply(shuffle_nbs, 1:n, n, cards, SIMPLIFY = FALSE)
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


