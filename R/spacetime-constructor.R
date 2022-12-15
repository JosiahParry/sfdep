
# Class constructor -------------------------------------------------------

#' Construct a `spacetime` object
#'
#' A spacetime object is a collection of a linked data frame and an sf objects.
#' It can be thought of as geography linked to a table that represents those
#' geographies over one or more time periods.
#'
#' Create a spacetime representation of vector data from a `data.frame` and an
#' `sf` object with `spacetime()`
#'
#'
#' @param .data an object with base class `data.frame` containing location and time
#'   identifiers `.loc_col` and `.time_col` respectively.
#' @param .geometry an `sf` object with columns `.loc_col` and `.time_col`
#' @param .loc_col the quoted name of the column containing unique location identifiers. Must be present in both `.data` and `.geometry`.
#' @param .time_col the quoted name of the column containing time periods must be present `.data`. See details for more
#' @param active default `"data"`. The object to make active. See [`activate()`] for more.
#'
#' @details
#'
#' `.time_col` must be able to be sorted. As such, `.time_col`
#' cannot be a character vector. It must have a base type of (`typeof()`) either
#' `double` or `integer`—the case in dates or factors respectively. An edge case
#' exists with `POSIXlt` class objects as these can be sorted appropriately but
#' have a base type of `list`.
#'
#' [`spacetime()`] is a wrapper around [`new_spacetime()`]. Spacetimes are
#' validated before creation with [`validate_spacetime()`].
#'
#' Check if an object is a spacetime object with [`is_spacetime()`] or
#' [`is.spacetime()`].
#'
#' @section Validation:
#'
#' `validate_spacetime()` checks both `.data` and `.geometry` to ensure that
#' the constructed spacetime object meets minimum requirements.:
#'
#'  - `.data` inherits the `data.frame` class
#'  - `.geometry` is an `sf` object
#'  - ensures that `.time_col` is of the proper class
#'  - ensures there are no missing geometries in `.geometry`
#'  - checks for duplicate geometries
#'  - ensures `.loc_col` are the same type in `.data` and `.geometry`
#'  - lastly informs of missing values in additional columns in `.data`
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
#' bos <- spacetime(df, geo, ".region_id", "year")
#' is_spacetime(bos)
#' bos
#' @returns
#' - `spacetime()` and `new_spacetime()` construct spacetime clss objects
#' - `validate_spacetime()` returns nothing but will elicit a warning or error if the spacetime object is not validly constructed
#' - `is_spacetime()` and `is.spacetime()` return a logical scalar indicating if an object inherits the spacetime class
spacetime <- function(.data, .geometry, .loc_col, .time_col, active = "data") {
  new_spacetime(.data, .geometry, .loc_col, .time_col, active = active)
}

#' @rdname spacetime
#' @export
new_spacetime <- function(.data, .geometry, .loc_col,
                          .time_col, active = "data") {
  match.arg(active, c("data", "geometry"))

  # validate inputs
  validate_spacetime(.data, .geometry, .loc_col, .time_col)

  switch(active,
         geometry = new_spacetime_geo(.data, .geometry, .loc_col, .time_col),
         data = new_spacetime_data(.data, .geometry, .loc_col, .time_col),
  )
}

#' @keywords internal
new_spacetime_geo <- function(.data, .geometry, .loc_col, .time_col) {
  times <- sort(unique(.data[[.time_col]]))
  n_times <- length(times)
  locs <- .geometry[[.loc_col]]
  n_locs <- length(.geometry[[.loc_col]])
  structure(.geometry,
            active = "geometry",
            data = .data,
            loc_col = .loc_col,
            locs = locs,
            n_locs = n_locs,
            time_col = .time_col,
            times = times,
            n_times = n_times,
            class = union("spacetime", class(.geometry)))
}

#' @keywords internal
new_spacetime_data <- function(.data, .geometry, .loc_col, .time_col) {

  times <- sort(unique(.data[[.time_col]]))
  n_times <- length(times)
  locs <- .geometry[[.loc_col]]
  n_locs <- length(locs)

  structure(.data,
            active = "data",
            data = .data,
            geometry = .geometry,
            loc_col = .loc_col,
            locs = locs,
            n_locs = n_locs,
            time_col = .time_col,
            times = times,
            n_times = n_times,
            class = union("spacetime", class(.data)))
}



# Class validator ---------------------------------------------------------
#'
#' @rdname spacetime
#' @export
validate_spacetime <- function(.data, .geometry, .loc_col, .time_col) {

  if (!inherits(.geometry, "sf")) {
    cli::cli_abort("{.var .geometry} must be an `sf` object.")
  }

  if (!inherits(.data, "data.frame")) {
    cli::cli_abort("{.var .data} must be a `data.frame`.")
  }

  # check if time-column is orderable
  # must have basetype of integer or double
  #  valid types: date, factor, ordered, int, dbl, posixlt
  times <- unique(.data[[.time_col]])
  can_be_ordered <- (typeof(times) %in% c("double", "integer") | inherits(times, "POSIXlt"))

  if (!can_be_ordered) {
    cli::cli_abort(c(
      "Unable to order `.time_col`.",
      i = "{.var .time_col} must have a base `typeof()` {.cls numeric} or {.cls integer},",
      "*" = "alternatively, must be of class {.cls POSIXlt}."))
  }

  # verify no missingness in geometry locations
  n_miss_loc_geo <- sum(is.na(.geometry[[.loc_col]]))

  if (n_miss_loc_geo > 0) {
    cli::cli_abort("Missing location IDs, `.loc_id`, in .geometry.")
  }

  # verify no duplicate geometry in .geometry
  n_unique_loc_geo <- length(unique(sf::st_geometry(.geometry)))
  if (n_unique_loc_geo < nrow(.geometry)) {
    cli::cli_abort("Duplicate geometries present.")
  }


  # Compare regions
  #  check types:
  .data_loc_class <- class(.data[[.loc_col]])
  .geo_loc_class <- class(.geometry[[.loc_col]])

  if (!identical(.data_loc_class, .geo_loc_class)) {
    cli::cli_abort(
      c("Differing class types for {.var .loc_col}.",
        i = "{.var .data}:       {.cls {.data_loc_class}}",
        "i" = "{.var .geometry}: {.cls {.geo_loc_class}}.")
    )
  }


  # check missingness in each column in .data that isn't necessary
  addtl_colnames <- setdiff(colnames(.data), c(.loc_col, .time_col))

  n_missing <- unlist(lapply(addtl_colnames, function(.x) sum(is.na(.data[[.x]]))))
  names(n_missing) <- addtl_colnames
  to_report <- n_missing[n_missing > 0]

  if (any(to_report > 0)) {
    cli::cli_alert_warning("Vars(s) {.var {names(to_report)}} {?is/are} missing {to_report} value(s).")
  }
}


# Class test --------------------------------------------------------------
#' @rdname spacetime
#' @export
#' @param x an object to test
#' @param ... unused
is_spacetime <- function(x, ...) {
  inherits(x, "spacetime")
}

#' @export
#' @rdname spacetime
is.spacetime <- function(x, ...) {
  is_spacetime(x, ...)
}


