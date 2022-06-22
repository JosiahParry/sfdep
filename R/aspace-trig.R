# Trigonometric functions which returns degrees
# Taken from aspace 3.2
# Definitions cleaned up a bit, though.
# https://github.com/cran/aspace/tree/master/R
sin_d <- function(theta = 0) {
  sin(theta*pi/180)
}

cos_d <- function(theta = 0) {
  cos(theta*pi/180)
}

atan_d <- function(theta = 0) {
  atan(theta)*180/pi
}
