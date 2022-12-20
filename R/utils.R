#' @keywords internal
`%||%` <- function (x, y) {
  if (rlang::is_null(x))
    y
  else x
}

#' Check if a vector of packages are available
#'
#' @param x a character vector of package names
#' @keywords internal
check_pkg_suggests <- function(x) {
  missing_pkgs <- !vapply(x, requireNamespace, FUN.VALUE = logical(1), quietly = TRUE)

  if (any(missing_pkgs))
    cli::cli_abort('Missing packages: {paste("`", x[missing_pkgs], "`", sep = "", collapse = ", ")}')
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
#' Given a neighbor and weight list, create a `listw` object.
#'
#' @param nb a neighbor list object for example as created by `st_contiguity()`.
#' @param wt a weights list as created by `st_weights()`.
#' @export
#' @examples
#' recreate_listw(guerry_nb$nb, guerry_nb$wt)
#' @returns a `listw` object
recreate_listw <- function(nb, wt) {
  which_style <- c(attr(wt, "W") %||% NA,
                   attr(wt, "B") %||% NA,
                   attr(wt, "C") %||% NA,
                   attr(wt, "U") %||% NA,
                   attr(wt, "minmax") %||% NA,
                   attr(wt, "S") %||% NA)

  possible_styles <- c("W", "B", "C", "U", "minmax", "S")

  if (!inherits(nb, "nb")) nb <- class_modify(nb, "nb")

  listw <- list(style = possible_styles[!is.na(which_style)],
                neighbours = nb,
                weights = wt)

  class(listw) <- c("listw", "nb", "list")

  listw
}

#' Identify xj values
#'
#' Find `xj` values given a numeric vector, `x`, and neighbors list, `nb`.
#'
#' @param x a vector of any class
#' @param nb a `nb` object e.g. created by [`st_contiguity()`] or [`st_knn()`]
#' @export
#' @returns
#' A list of length `x` where each element is a numeric vector with the same length as the corresponding element in `nb`.
#' @examples
#' nb <- st_contiguity(sf::st_geometry(guerry))
#' xj <- find_xj(guerry$crime_prop, nb)
#' xj[1:3]
find_xj <- function(x, nb) {
  lapply(nb, FUN = function(nbs_i) x[nbs_i])
}


#' Conditional permutation of neighbors
#'
#' Creates a conditional permutation of neighbors list holding i fixed and shuffling it's neighbors.
#'
#' @param nb a neighbor list.
#' @param seed default null. A value to pass to `set.seed()` for reproducibility.
#' @export
#' @examples
#' nb <- st_contiguity(guerry)
#' nb[1:5]
#' # conditionally permute neighbors
#' perm_nb <- cond_permute_nb(nb)
#' perm_nb[1:5]
#' @returns A list of class `nb` where each element contains a random sample of neighbors excluding the observed region.
cond_permute_nb <- function(nb, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  n <- length(nb)
  cards <- lengths(nb)
  res <- mapply(shuffle_nbs, 1:n, n, cards, SIMPLIFY = FALSE)
  class_modify(res, "nb")
}



# Conditional Permutation -------------------------------------------------

#' Conditionally permutes a listw object
#'
#' @param a listw object.
#'
#' @keywords internal
#' @returns a `listw` object
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
#' @returns a `nb` class object
shuffle_nbs <- function(i, n, card) {
  x <- 1:n
  sample(x[-i], size = card)
}


