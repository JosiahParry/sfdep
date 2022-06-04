#' Convert neighbor or weights list to matrix
#'
#' Given a `nb` list or weights list, convert them to a matrix.
#'
#' @param nb a neighbor list—i.e. as created by `st_contiguity()`.
#' @param wt a weights list—i.e. as created by `st_weights()`
#' @export
#'
#' @examples
#'
#' # make a grid
#' g <- sf::st_make_grid(
#'   cellsize = c(10, 10),
#'   offset = c(0, 0),
#'   n = c(2, 2)
#' )
#'
#' # create neighbors
#' nb <- st_contiguity(g)
#'
#' # cast to matrix
#' nb_as_matrix(nb)
#'
#' # create weights
#' wt <- st_weights(nb)
#'
#' # cast as matrix
#' wt_as_matrix(nb, wt)
#'
#' @returns
#' Returns a n x n matrix
wt_as_matrix <- function(nb, wt) {
  listw <- recreate_listw(nb, wt)
  spdep::listw2mat(listw)
}

#' @rdname wt_as_matrix
#' @export
nb_as_matrix <- function(nb) {
  spdep::nb2mat(nb, style = "B")
}





