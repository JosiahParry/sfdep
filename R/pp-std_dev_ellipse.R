#' Calculation Standard Deviational Ellipse
#'
#' From an sf object containing points, calculate the standard deviational ellipse.
#'
#' @inheritParams center_mean
#'
#' @details
#'
#' The bulk of this function is derived from the archived CRAN package aspace version 3.2.0.
#'
#' @returns
#' An sf object with three columns
#'
#' - `sx`: major axis radius in CRS units,
#' - `sy`: minor axis radius in CRS units,
#' - `theta`: degree rotation of the ellipse.
#'
#' sf object's geometry is the center mean point.
#' @examples
#' #' # Make a grid to sample from
#' grd <- sf::st_make_grid(n = c(1, 1), cellsize = c(100, 100), offset = c(0,0))
#'
#' # sample 100 points
#' pnts <- sf::st_sample(grd, 100)
#' std_dev_ellipse(pnts)
#' @export
std_dev_ellipse <- function(geometry) {
  crs <- sf::st_crs(geometry)
  xy <- sf::st_coordinates(geometry)
  cent <- center_mean(geometry)
  cent_xy <- as.numeric(sf::st_coordinates(cent))
  n <- nrow(xy)

  # ADD COLUMNS TO xy FOR SQUARED x,y TERMS
  xy <- cbind(xy, xy[, 1] ^ 2, xy[, 2] ^ 2)

  # ADD COLUMNS TO xy FOR TRANSPOSED TERMS
  xy <- cbind(xy, xy[, 1] - cent_xy[1], xy[, 2] - cent_xy[2])

  # ADD COLUMNS FOR SQUARED TRANSPOSED TERMS AND PRODUCT OF TRANSPOSED TERMS
  xy <- cbind(xy, xy[, 5] ^ 2, xy[, 6] ^ 2, xy[, 5] * xy[, 6])

  # COMPUTE THETA (as in EBDON, 1985)
  top1 <- sum(xy[, 7]) - sum(xy[, 8])
  top2 <- sqrt((sum(xy[, 7]) - sum(xy[, 8])) ^ 2 + 4 * (sum(xy[, 9])) ^
                 2)
  bottom <- (2 * sum(xy[, 9]))
  tantheta <- (top1 + top2) / bottom

  if (tantheta < 0) {
    theta <- 180 + (atan_d(tantheta))
  } else {
    theta <- atan_d(tantheta)
  }

  sintheta <- sin_d(theta)
  costheta <- cos_d(theta)
  sin2theta <- sintheta ^ 2
  cos2theta <- costheta ^ 2
  sinthetacostheta <- sintheta * costheta
  sigmax <-
    sqrt(2) * sqrt((
      (sum(xy[, 7])) * (cos2theta) - 2 * (sum(xy[, 9])) * (sinthetacostheta) + (sum(xy[, 8])) *
        (sin2theta)
    ) / (n - 2))
  sigmay <- sqrt(2) *
    sqrt((
      (sum(xy[, 7])) * (sin2theta) +
        2 * (sum(xy[, 9])) * (sinthetacostheta) +
        (sum(xy[, 8])) * (cos2theta)
    ) /
      (n - 2))

  res <- sf::st_as_sf(cent)
  res[["sx"]] <- sigmax
  res[["sy"]] <- sigmay
  res[["theta"]] <- theta
  res <- rename_geometry(res, "geometry")
  res[, c("sx", "sy", "theta")]
}


# Function to rename geometry column of an sf object
# This is helpful because st_as_sf() will create a geometry column
# called `x`
# Taken from Ecoserv tool
#https://github.com/ecoservR/ecoserv_tool/blob/master/R/fun_spatial.R
rename_geometry <- function(g, name){
  current = attr(g, "sf_column")
  names(g)[names(g) == current] = name
  sf::st_geometry(g) = name
  g
}

#
# df <- sf::read_sf('/Users/josiahparry/Library/r-miniconda-arm64/envs/geo/lib/python3.8/site-packages/libpysal/examples/virginia/vautm17n_points.shp')
#
# geometry <- sf::st_geometry(df)
#
# std_dev_ellipse(geometry)
