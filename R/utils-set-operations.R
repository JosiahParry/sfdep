#' Set Operations
#'
#' Perform set operations element-wise on two lists of equal length.
#'
#' @details
#'
#' - `nb_union()` returns the union of elements in each element of x and y
#' - `nb_intersect()` returns the intersection of elements in each element of x and y
#' - `nb_setdiff()` returns the difference of elements in each element of x and y
#'
#' @examples
#' nb <- st_contiguity(guerry$geometry)
#' nb_knn <- st_knn(guerry$geometry, k = 3)
#' nb_setdiff(nb, nb_knn)
#' nb_union(nb, nb_knn)
#' nb_intersect(nb, nb_knn)
#'
#' @return
#' A list of class `nb`
#'
#' @param x list of class `nb`
#' @param y list of class `nb`
#' @export
nb_union <- function(x, y) {
  res <- mapply(union, x, y, SIMPLIFY = FALSE)
  res <- lapply(res, sort)
  res <- fill_missing_nb(res)
  class_modify(res, "nb")
}


#' @rdname nb_union
#' @export
nb_intersect <- function(x, y) {
  res <- mapply(intersect, x, y, SIMPLIFY = FALSE)
  res <- lapply(res, sort)
  res <- fill_missing_nb(res)
  class_modify(res, "nb")
}

#' @rdname nb_union
#' @export
nb_setdiff <- function(x, y) {
  res <- mapply(setdiff, x, y, SIMPLIFY = FALSE)
  res <- lapply(res, sort)
  res <- fill_missing_nb(res)
  class_modify(res, "nb")
}


#' @keywords internal
#' @returns
#' A list of class `nb` ensuring that any list elements with length 0 contain the value `0`.
fill_missing_nb <- function(nb) {
  missing_index <- which(lengths(nb) == 0)
  for (miss in missing_index) {
    nb[[miss]] <- 0L
  }
  nb
}
