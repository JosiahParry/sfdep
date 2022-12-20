# Set arbitrary column ----------------------------------------------------

#' Set columns from `geometry` to `data`
#'
#' Set a column from the `geometry` context of a spacetime object to the `data`
#' context.
#'
#' @param x a spacetime object
#' @param .from_geo the name of the column in the `geometry` context
#' @param .to_data the name of the new variable to create in the `data` context
#' @param .nb_col the name of neighbor column in the `geometry` context
#' @param .wt_col the name of the weights column in the `geometry` context
#'
#' @details
#'
#' These functions will reorder the spacetime object to ensure that it is ordered
#' correctly based on the location time columns in the `geometry` context defined
#' by the `loc_col` and `time_col` attributes respectively.
#'
#' [set_wts()] and [set_nbs()] create a new column in the data context with the
#' same name as the column in the geometry context. If a different name is desired
#' use [`set_col()`]
#'
#' @returns
#'
#' A spacetime object with an active data context and a new column from the geometry
#' context.
#'
#' @export
#' @examples
#' df_fp <- system.file("extdata", "bos-ecometric.csv", package = "sfdep")
#' geo_fp <- system.file("extdata", "bos-ecometric.geojson", package = "sfdep")
#'
#' # read in data
#' df <- readr::read_csv(df_fp, col_types = "ccidD")
#' geo <- sf::read_sf(geo_fp)
#'
#' # Create spacetime object called `bos`
#' bos <- spacetime(df, geo,
#'                 .loc_col = ".region_id",
#'                 .time_col = "time_period")
#' bos <- activate(bos, "geometry")
#' bos$nb <- st_contiguity(bos)
#' bos$wt <- st_weights(bos$nb)
#' bos$card <- st_cardinalties(bos$nb)
#'
#' set_nbs(bos)
#' set_wts(bos)
#' set_col(bos, "card")
#' set_col(bos, "card", "cardinalities")

set_col <- function(x, .from_geo, .to_data = .from_geo) {

  if (!is_spacetime_cube(x)) cli::cli_abort(
    c("`x` must be a valid spacetime cube.",
      i = "see `?is_spacetime_cube()`.")
  )

  if (active(x) == "geometry") x <- activate(x, "data")

  # determine number of time periods
  n_times <- length(attr(x, "times"))

  # extract var
  geo_col <- attr(x, "geometry")[[.from_geo]]
  if (is.null(geo_col)) cli::cli_abort("`.from_geo` is missing from `geometry`.")

  # determine row ordering to match regions
  .loc_col = attr(x, "loc_col")
  .time_col = attr(x, "time_col")
  geo_locs <- attr(x, "geometry")[[.loc_col]]
  region_index <- stats::setNames(seq_along(geo_locs), geo_locs)
  data_loc_id <- region_index[x[[.loc_col]]]

  # reorder x appropriately by regions and time
  # order by time then data loc id
  x <- x[order(x[[.time_col]], data_loc_id),]
  # repeat wt object n_times to fill. Order is correct based on above
  # ordering
  x[[.to_data]] <- rep(geo_col, n_times)
  x

}

# Set wt from geometry ----------------------------------------------------

#' @rdname set_col
#' @export
set_wts <- function(x, .wt_col = "wt") {

  if (!is_spacetime_cube(x)) cli::cli_abort(
    c("`x` must be a valid spacetime cube.",
      i = "see `?is_spacetime_cube()`.")
    )

  if (active(x) == "geometry") x <- activate(x, "data")

  # determine number of time periods
  n_times <- length(attr(x, "times"))

  # extract wt list
  wt <- attr(x, "geometry")[[.wt_col]]
  if (is.null(wt)) cli::cli_abort("`.wt_col` is missing from `geometry`.")

  # determine row ordering to match regions
  .loc_col = attr(x, "loc_col")
  .time_col = attr(x, "time_col")
  geo_locs <- attr(x, "geometry")[[.loc_col]]
  region_index <- stats::setNames(seq_along(geo_locs), geo_locs)
  data_loc_id <- region_index[x[[.loc_col]]]

  # reorder x appropriately by regions and time
  # order by time then data loc id
  x <- x[order(x[[.time_col]], data_loc_id),]
  # repeat wt object n_times to fill. Order is correct based on above
  # ordering
  x[[.wt_col]] <- rep(wt, n_times)
  x

}


# Set nb from geometry ----------------------------------------------------

#' @rdname set_col
#' @export
set_nbs <- function(x, .nb_col = "nb") {

  if (!is_spacetime_cube(x)) cli::cli_abort(
    c("`x` must be a valid spacetime cube.",
      i = "see `?is_spacetime_cube()`.")
  )

  if (active(x) == "geometry") x <- activate(x, "data")

  # determine number of time periods
  n_times <- length(attr(x, "times"))

  # extract nb list
  nb <- attr(x, "geometry")[[.nb_col]]
  if (is.null(nb)) cli::cli_abort("`.nb_col` is missing from `geometry`.")

  # determine row ordering to match regions
  .loc_col = attr(x, "loc_col")
  .time_col = attr(x, "time_col")
  geo_locs <- attr(x, "geometry")[[.loc_col]]
  region_index <- stats::setNames(seq_along(geo_locs), geo_locs)
  data_loc_id <- region_index[x[[.loc_col]]]

  # reorder x appropriately by regions and time
  x <- x[order(x[[.time_col]], data_loc_id),]
  # repeat nb object n_times to fill. Order is correct based on above
  # ordering
  x[[.nb_col]] <- class_modify(rep(nb, n_times), "nb")
  x
}




# Ordering function -------------------------------------------------------

#' Order a spacetime cube
#'
#' When performing analysis on a spacetime cube, the order is of the utmost
#' importance. This function ensures that a spacetime cube is ordered
#' appropriately.
#'
#' @param x a spacetime cube object.
#'
#' @keywords internal
#' @returns a spacetime object that orders based on time and location
spt_order <- function(x) {
  if (!is_spacetime_cube(x)) cli::cli_abort(
    c("`x` must be a valid spacetime cube.",
      i = "see `?is_spacetime_cube()`.")
  )

  if (active(x) == "geometry") x <- activate(x, "data")
  # determine number of time periods
  n_times <- length(attr(x, "time"))

  # determine row ordering to match regions
  .loc_col = attr(x, "loc_col")
  .time_col = attr(x, "time_col")
  geo_locs <- attr(x, "geometry")[[.loc_col]]
  region_index <- stats::setNames(seq_along(geo_locs), geo_locs)
  data_loc_id <- region_index[x[[.loc_col]]]

  # reorder x appropriately by regions and time
  # order by time then data loc id
  x[order(x[[.time_col]], data_loc_id),]

}



