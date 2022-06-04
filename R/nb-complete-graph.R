#' Create Neighbors as Complete Graph
#'
#' Create a neighbors list where every element is related to every other element.
#' This creates a complete graph.
#'
#' @param n_elements the number of observations to create a neighbors list for
#' @inheritParams st_block_nb
#' @export
#' @returns
#' A neighbors list representing a complete graph.
#' @examples
#' st_complete_nb(5)
st_complete_nb <- function(n_elements, diag = FALSE) {
  res <- lapply(1:n_elements, function(x) 1:n_elements)
  attr(res, "self.included") <- TRUE

  if (!diag) res <- remove_self(res)
  class_modify(res, "nb")
}



