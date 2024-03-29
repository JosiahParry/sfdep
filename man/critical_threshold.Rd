% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-kernels.R
\name{critical_threshold}
\alias{critical_threshold}
\title{Identify critical threshold}
\usage{
critical_threshold(geometry, k = 1)
}
\arguments{
\item{geometry}{an sf geometry column}

\item{k}{the minimum number of neighbors to check for}
}
\value{
a numeric scalar value.
}
\description{
Identifies the minimum distance in which each observation will have at least one neighbor.
}
\examples{
critical_threshold(sf::st_geometry(guerry))
}
