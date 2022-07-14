#' @param x a spacetime object
#' @export
#' @rdname activate
active <- function(x) attr(x, "active")

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
#' @param x a spacetime object
#' @param what default NULL. Determines which context to activate. Valid argument values
#'   are `"geometry"` and `"data"`. If left null, returns `x`.
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
activate <- function(x, what = NULL) {
  match.arg(what, c("geometry", "data", NULL))
  is_active <- active(x)
  # if missing, set "what" to whatever is presently active
  if (is.null(what)) return(x)
  # if what is the same as active, return input object
  if (is_active == what) return(x)

  switch(what,
         geometry = activate_geometry(x),
         data = activate_data(x))

}

activate_geometry <- function(x) {
  # class(x) <- setdiff(class(x), "spacetime")
  new_spacetime_geo(x,
                    attr(x, "geometry"),
                    attr(x, "loc_col"),
                    attr(x, "time_col"))
}


activate_data <- function(x) {
  new_spacetime_data(attr(x, "data"),
                     x,
                     attr(x, "loc_col"),
                     attr(x, "time_col"))
}
