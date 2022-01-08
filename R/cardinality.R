#' Calculate neighbor cardinalities
#'
#' Identify the cardinality of a neighbor object. Utilizes `spdep::card()` for objects with class `nb`, otherwise returns `lengths(nb)`.
#'
#' @param nb A neighbor list object as created by `st_neighbors()`.
#' @family other
#' @export
st_cardinalties <- function(nb) {

  if (any(class(nb) == TRUE)) {
   return(spdep::card(nb))
  }

  lengths(nb)
}
