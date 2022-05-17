
# Selection ---------------------------------------------------------------

# Selection method
# TODO update `times` attribute when subset and data is active
`[.spacetime` <- function(x, ...) {
  x <- NextMethod()
}

`[[.spacetime` <- function(x, ...) {
  NextMethod()
}


# To sf -------------------------------------------------------------------

#' Cast between `spacetime` and `sf` classes
#'
#' @param x for [sfdep::st_as_sf()] a spacetime object. For [sfdep::as_spacetime()]
#'   an sf object.
#' @param ... unused
#' @export
#' @rdname as_spacetime
#' @importFrom sf st_as_sf
st_as_sf.spacetime <- function(x, ...) {
  if (active(x) == "geometry") x < activate(x, "data")

  merge(attr(x, "geometry"), x,
        by = attr(x, "loc_col"))
}



# From sf -----------------------------------------------------------------

#' Convert sf object to spacetime
#'
#'
#' @param .loc_col the quoted name of the column containing unique location identifiers.
#' @param .time_col the quoted name of the column containing time periods.
#' @export
as_spacetime <- function(x, .loc_col, .time_col, ...) {
  UseMethod("as_spacetime")
}

#' @rdname as_spacetime
#' @export
as_spacetime.sf <- function(x, .loc_col, .time_col, ...) {
  geometry <- getFromNamespace("distinct.sf", "sf")(x, !!rlang::sym(.loc_col),
                               !!rlang::sym(attr(x, "sf_column")))

  new_spacetime_data(sf::st_drop_geometry(x), geometry,
                     .loc_col, .time_col)
}
