% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/neighbors.R
\name{check_polygon}
\alias{check_polygon}
\title{Checks geometry for polygons.}
\usage{
check_polygon(geometry)
}
\arguments{
\item{geometry}{an sfc object}
}
\value{
Point geometry
}
\description{
If the provided geometry is a polygon, a point will be generated using \code{\link[sf:geos_unary]{sf::st_point_on_surface()}}. If a centroid is preferred, a new column can be created that contains the output of \code{\link[sf:geos_unary]{sf::st_centroid()}}.
}
\keyword{internal}
