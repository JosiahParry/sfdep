#' Calculate neighbor cardinalities
#'
#' Identify the cardinality of a neighbor object. Utilizes `spdep::card()` for objects with class `nb`, otherwise returns `lengths(nb)`.
#'
#' @param nb A neighbor list object as created by `st_neighbors()`.
#' @family other
#' @export
#' @examples
#' nb <- st_contiguity(sf::st_geometry(guerry))
#' st_cardinalties(nb)
#' @returns an integer vector with the same length as `nb`.
st_cardinalties <- function(nb) {

  if (any(class(nb) == "nb")) {
   return(spdep::card(nb))
  }

  lengths(nb)
}
