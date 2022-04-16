
#' Identifies the minimum distance in which each observation will have at least one neighbor.
critical_threshold <- function(geometry) {
  pnts <- check_polygon(geometry)
  knb <- spdep::knn2nb(spdep::knearneigh(pnts, 1))
  max(unlist(spdep::nbdists(knb, pnts)))
}

