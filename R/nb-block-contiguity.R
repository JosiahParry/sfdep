#' Create Block Contiguity for Spatial Regimes
#'
#' @description
#' libpysal write that "block contiguity structures are relevant when defining neighbor relations based on membership in a regime. For example, all counties belonging to the same state could be defined as neighbors, in an analysis of all counties in the US."
#'
#' Source: [libpysal](https://pysal.org/libpysal/generated/libpysal.weights.block_weights.html)
#'
#' @param id a column identifying unique observations
#' @param regime a column identifying which spatial regime each element of `id` belongs
#' @param diag default `FALSE`. If `TRUE`, includes diagonal element / the self.
#' @returns
#' An object of class `nb`. When `diag = TRUE` the attribute `self.included = TRUE`.
#' @examples
#' id <- guerry$code_dept
#' regime <- guerry$region
#' st_block_nb(regime, id)
#' @export
st_block_nb <- function(regime, id = 1:length(regime), diag = FALSE) {
  id <- as.character(id)
  regime <- as.character(regime)

  # create empty list to fill
  res <- vector(mode = "list", length = length(id))
  names(res) <- id

  # identify unique regimes
  reg <- as.character(unique(regime))

  # create regime lookup table
  regime_lu <- vector(mode = "list", length = length(reg))
  names(regime_lu) <- reg

  for (i in 1:length(reg)) {
    regime_lu[[i]] <- which(regime == reg[i])
  }

  # fill results based on regime
  for (i in 1:length(regime)) {
    res[i] <- regime_lu[regime[i]]
  }

  # self is included by default
  attr(res, "self.included") <- TRUE
  attr(res, "region.id") <- id

  if (!diag) res <- remove_self(res)

  class_modify(res, "nb")
}


