#' Includes self in neighbor list
#'
#' Includes observed region in list of own neighbors. For some neighbor lists, it is important to include the ith observation (or self) in the neighbors list, particularly for kernel weights.
#'
#' @param nb an object of class `nb` e.g. made by [st_contiguity()]
#' @examples
#' nb <- st_contiguity(guerry)
#' self_included <- include_self(nb)
#' self_included
#' remove_self(self_included)
#' @returns
#' An object of class `nb`.
#' @export
include_self <- function(nb) {
  spdep::include.self(nb)
}

#' @rdname include_self
#' @export
remove_self <- function(nb) {

  if (is.null(attr(nb, "self.included"))) {
    cli::cli_warn("Self not included. Cannot remove.")
    return(nb)
  }

  for (i in 1:length(nb)) {
    nb[[i]] <- nb[[i]][-which(nb[[i]] == i)]
  }

  attr(nb, "self.included") <- NULL

  return(nb)
}

