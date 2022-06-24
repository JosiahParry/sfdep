#' Test if a spacetime object is a spacetime cube
#'
#' Given an object with class `spacetime`, determine if it is a _spacetime cube_.
#' If the time-series is is irregular a warning is emitted
#' (see [`validate_spacetime()`] for more on the restrictions on the time column.
#'
#'
#' @details
#'
#' A spacetime object is a spacetime cube when it contains a regular time-series
#' representation of each geometry. That is, only one observation for at each
#' time period per geography is present.
#'
#' The number of rows in a spacetime cube is the number of geographies multiplied
#' by the number of time periods. For example if there are 10 locations and 20
#' time periods, the number of rows must be 200.
#'
#' @section Validation:
#'
#' `is_spacetime_cube()` runs a number of checks that to ensure that the provided
#' object is in fact a spacetime cube. It checks that:
#'
#' - the number of rows is equal to the number of locations multiplied by the number
#'   of time periods
#' - each time period has an equal number of observations
#' - each location has an equal number of observations
#' - each combination of time period and location has only one observation
#' - that the time-series is regular
#'
#' @param x a spacetime object
#' @param ... unused
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
#'
#' is_spacetime_cube(bos)
#' is_spacetime_cube(bos[round(runif(1000, 0, nrow(bos))),])
#' is_spacetime_cube(guerry)
is_spacetime_cube <- function(x, ...) {

  check_pkg_suggests("zoo")
  # check if spacetime
  if (!inherits(x, "spacetime")) {
    # cli::cli_inform("Object is not of class {.cls spacetime}.")
    return(FALSE)
  }

  # ensure that data is active
  if (active(x) == "geometry") x <- activate(x, "data")

  .time_col <- attr(x, "time_col")
  .loc_col <- attr(x, "loc_col")

  times <- sort(unique(x[[.time_col]]))
  n_times <- length(times)
  n_locs <- length(attr(x, "geometry")[[.loc_col]])

  # check if it is a complete spacetime object
  # meaning every location has 1 obs for each time period
  # check if every time-period and location have one obs
  # check number of rows = n_times * n_locs
  if (!nrow(x) ==  (n_times * n_locs)) {
    cli::cli_alert_warning(
      "Number of rows does not equal `n time-periods x n locations`"
    )
    return(FALSE)
  }

  # Check that all locations have n_time obs
  if (!all(table(x[[.loc_col]]) == n_times)) {
    cli::cli_alert_warning(
      "Not every location has an observation for each time period."
    )

    return(FALSE)
  }

  # check that all times have n_locs obs
  if (!all(table(x[[.time_col]]) == n_locs)) {
    cli::cli_alert_warning(
      "Not every time period has an observation for each location."
    )

    return(FALSE)
  }

  # check that each loc <> time is only 1 observation
  if (!all(table(x[[.loc_col]], x[[.time_col]]) == 1)) {
    cli::cli_alert_warning(
      "Not every location-time combination has exactly 1 value."
    )
    return(FALSE)
  }

  # checks if time series is regular
  # uses zoo here
  is_reg <- zoo::is.regular(zoo::zoo(seq_along(times), times))

  if (!is_reg) cli::cli_alert_warning("Be careful: time-series is not regular.")

  TRUE
}
