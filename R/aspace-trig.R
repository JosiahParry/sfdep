#' Trigonometric functions
#'
#' These functions are wrappers around the base trigonometric functions.
#' However these return degree values rather than radians.
#'
#' These functions were adopted from the now archived CRAN package `{aspace}`.
#'
#' @keywords internal
sin_d <- function(theta = 0) {
  sin(theta*pi/180)
}

cos_d <- function(theta = 0) {
  cos(theta*pi/180)
}

atan_d <- function(theta = 0) {
  atan(theta)*180/pi
}


# Trigonometric functions which returns degrees
# Taken from aspace 3.2
# Definitions cleaned up a bit, though.
# https://github.com/cran/aspace/tree/master/R



