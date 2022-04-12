#' "Essay on the Moral Statistics of France" data set.
#'
#' This dataset has been widely used to demonstrate geospatial methods and techniques. As such it is useful for inclusion to this R package for the purposes of example. The dataset in this package is modified from Guerry by [Michael Friendly](https://www.datavis.ca/).
#'
#'`guerry` and `guerry_nb` objects are sf class objects. These are polygons of the boundaries of France (excluding Corsica) as they were in 1830.
#'
"guerry"

#' @rdname guerry
#' @format `guerry` an sf object with 85 observations and 27 variables. `guerry_nb` has 2 additional variables created by `sfdep`.
#' @source `Guerry::gfrance85`
"guerry_nb"
