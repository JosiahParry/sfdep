#' @param .data a spacetime object
#' @export
#' @rdname activate
active <- function(.data) attr(.data, "active")

#' Activate spacetime context
#'
#' @description
#'
#' From a [`spacetime`] object, activate either the data or geometry contexts. The active object will then become available for manipulation.
#'
#' @details
#' A  [`spacetime`] object contains both a data frame and an sf object. The
#' data frame represents geographies over one or more time periods and the sf
#' object contains the geographic information for those locations.
#'
#' @param .data a spacetime object
#' @param what default NULL. Determines which context to activate. Valid argument values
#'   are `"geometry"` and `"data"`. If left null, returns `.data`.
#' @returns
#' For `activate()` an object of class spacetime with the specified context activated. `active()` returns a scalar character with the active context can be either "goemetry" or "data".
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
#'                  .loc_col = ".region_id",
#'                  .time_col = "time_period")
#'
#' active(bos)
#' activate(bos, "geometry")
activate <- function(.data, what) UseMethod("activate")

#' @export
activate.spacetime <- function(.data, what = NULL) {
  match.arg(what, c("geometry", "data", NULL))
  is_active <- active(.data)
  # if missing, set "what" to whatever is presently active
  if (is.null(what)) return(.data)
  # if what is the same as active, return input object
  if (is_active == what) return(.data)

  switch(what,
         geometry = activate_geometry(.data),
         data = activate_data(.data))

}

activate_geometry <- function(.data) {
  # class(x) <- setdiff(class(x), "spacetime")
  new_spacetime_geo(.data,
                    attr(.data, "geometry"),
                    attr(.data, "loc_col"),
                    attr(.data, "time_col"))
}


activate_data <- function(.data) {
  new_spacetime_data(attr(.data, "data"),
                     .data,
                     attr(.data, "loc_col"),
                     attr(.data, "time_col"))
}



