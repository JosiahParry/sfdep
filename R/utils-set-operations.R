nb <- st_contiguity(guerry$geometry)
nb_knn <- st_knn(guerry$geometry, k = 3)
nb_setdiff(nb, nb_knn)
nb_union(nb, nb_knn)
nb_intersect(nb, nb_knn)


nb_union <- function(x, y) {
  res <- mapply(union, x, y, SIMPLIFY = FALSE)
  res <- fill_missing_nb(res)
  class_modify(res, "nb")
}


nb_intersect <- function(x, y) {
  res <- mapply(intersect, x, y, SIMPLIFY = FALSE)
  res <- fill_missing_nb(res)
  class_modify(res, "nb")
}

nb_setdiff <- function(x, y) {
  res <- mapply(setdiff, x, y, SIMPLIFY = FALSE)
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
