
# Selection ---------------------------------------------------------------

# Selection method
#' TODO update `times` attribute when subset and data is active
`[.spacetime` <- function(x, ...) {
  x <- NextMethod()
}

`[[.spacetime` <- function(x, ...) {
  NextMethod()
}


# To sf -------------------------------------------------------------------

# convert spacetime cube to sf object
st_as_sf.spacetime <- function(x, ...) {
  merge(attr(x, "geometry"), x,
        by = attr(x, "loc_col"))
}

# st_as_sf(spt)


# From sf -----------------------------------------------------------------

as_spacetime <- function(x, ...) {
  UseMethod("as_spacetime")
}

as_spacetime.sf <- function(x, .loc_col, .time_col, ...) {
  geometry <- sf:::distinct.sf(x, !!rlang::sym(.loc_col),
                               !!rlang::sym(attr(x, "sf_column")))

  new_spacetime_data(st_drop_geometry(x), geometry,
                     .loc_col, .time_col)
}
