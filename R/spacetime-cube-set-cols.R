# Set wt from geometry ----------------------------------------------------

set_wts <- function(x, .wt_col = "wt") {

  if (!is_spacetime_cube(x)) cli::cli_abort(
    c("`x` must be a valid spacetime cube.",
      i = "see `?is_spacetime_cube()`.")
    )

  if (active(x) == "geometry") x <- activate(x, "data")

  # determine number of time periods
  n_times <- length(attr(x, "time"))

  # extract wt list
  wt <- attr(x, "geometry")[[.wt_col]]
  if (is.null(wt)) cli::cli_abort("`.wt_col` is missing from `geometry`.")

  # determine row ordering to match regions
  .loc_col = attr(x, "loc_col")
  .time_col = attr(x, "time_col")
  geo_locs <- attr(x, "geometry")[[.loc_col]]
  region_index <- setNames(seq_along(geo_locs), geo_locs)
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

set_nbs <- function(x, .nb_col = "nb") {

  if (!is_spacetime_cube(x)) cli::cli_abort(
    c("`x` must be a valid spacetime cube.",
      i = "see `?is_spacetime_cube()`.")
  )

  if (active(x) == "geometry") x <- activate(x, "data")

  # determine number of time periods
  n_times <- length(attr(x, "time"))

  # extract nb list
  nb <- attr(x, "geometry")[[.nb_col]]
  if (is.null(nb)) cli::cli_abort("`.nb_col` is missing from `geometry`.")

  # determine row ordering to match regions
  .loc_col = attr(x, "loc_col")
  .time_col = attr(x, "time_col")
  geo_locs <- attr(x, "geometry")[[.loc_col]]
  region_index <- setNames(seq_along(geo_locs), geo_locs)
  data_loc_id <- region_index[x[[.loc_col]]]

  # reorder x appropriately by regions and time
  x <- x[order(x[[.time_col]], data_loc_id),]
  # repeat nb object n_times to fill. Order is correct based on above
  # ordering
  x[[.nb_col]] <- rep(nb, n_times)
  x
}


# Set arbitrary column ----------------------------------------------------

set_col <- function(x, .from_geo, .to_data = .from_geo) {

  if (!is_spacetime_cube(x)) cli::cli_abort(
    c("`x` must be a valid spacetime cube.",
      i = "see `?is_spacetime_cube()`.")
  )

  if (active(x) == "geometry") x <- activate(x, "data")

  # determine number of time periods
  n_times <- length(attr(x, "time"))

  # extract var
  geo_col <- attr(x, "geometry")[[.from_geo]]
  if (is.null(geo_col)) cli::cli_abort("`.from_geo` is missing from `geometry`.")

  # determine row ordering to match regions
  .loc_col = attr(x, "loc_col")
  .time_col = attr(x, "time_col")
  geo_locs <- attr(x, "geometry")[[.loc_col]]
  region_index <- setNames(seq_along(geo_locs), geo_locs)
  data_loc_id <- region_index[x[[.loc_col]]]

  # reorder x appropriately by regions and time
  # order by time then data loc id
  x <- x[order(x[[.time_col]], data_loc_id),]
  # repeat wt object n_times to fill. Order is correct based on above
  # ordering
  x[[.to_data]] <- rep(geo_col, n_times)
  x

}
