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
#' @param neighbors A neighbor list object as created by `st_neighbors()`.
#' @param weights A weights list as created by `st_weights()`.
#' @keywords internal
recreate_listw <- function(neighbors, weights) {
  which_style <- c(attr(weights, "W") %||% NA,
                   attr(weights, "B") %||% NA,
                   attr(weights, "C") %||% NA,
                   attr(weights, "U") %||% NA,
                   attr(weights, "minmax") %||% NA,
                   attr(weights, "S") %||% NA)

  possible_styles <- c("W", "B", "C", "U", "minmax", "S")

  listw <- list(style = possible_styles[!is.na(which_style)],
                neighbours = neighbors,
                weights = weights)

  class(listw) <- c("listw", "nb", "list")

  listw
}



#' Match kernel functions
#'
#' Kernel functions for creating kernel based weights with [st_kernel_weight()].
#'
#' @details
#'
#' Supported kernels are below.
#'
#' Formulas come from Anselin & Morrison's [notes](https://spatialanalysis.github.io/lab_tutorials/Spatial_Weights_as_Distance_Functions.html#kernal-weights).
#'
#' - `uniform`:  K(z) = 1/2 for ∣z∣<1
#' - `triangular`: K(z) = (1−∣z∣) for ∣z∣ < 1
#' - `epanechnikov`: K(z) = (3/4)(1−z^2) for ∣z∣ < 1
#' - `quartic`: K(z) = (15/16)*(1−(z/threshold)^2^)2 for ∣z∣ < 1
#' - `guassian`: K(z) = (2pi)^{1/2} * exp(−z^2/2)
#'
#' @param x kernel distances
#' @param thresh critical threshold
#' @param ... unused
#' @export
kernels <- list(
  uniform = function(x, ...) x * 0 + .5,
  triangular = function(x, thresh) 1 - abs(x / thresh),
  epanechnikov = function(x, thresh) .75 * (1- (x / thresh)^2),
  quartic = function(x, thresh) (15/16)*(1-(x/thresh)^2)^2,
  gaussian =  function(x, thresh) sqrt(2*pi)*exp((-(x/thresh)^2)/2)
)


