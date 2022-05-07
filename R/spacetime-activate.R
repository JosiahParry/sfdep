active <- function(x) attr(x, "active")

#' Activate context
activate <- function(x, what = NULL) {
  is_active <- active(x)
  # if missing, set "what" to whatever is presently active
  if (is.null(what)) what = is_active
  # if what is the same as active, return input object
  if (is_active == what) return(x)

  switch(what,
         geometry = activate_geometry(x),
         data = activate_data(x))

}

activate_geometry <- function(x) {
  class(x) <- setdiff(class(x), "spacetime")
  new_spacetime_geo(x, attr(x, "geometry"),
                    attr(x, "loc_col"),
                    attr(x, "time_col"))
}


activate_data <- function(x) {
  new_spacetime_data(attr(x, "data"),
                     x,
                     attr(x, "loc_col"),
                     attr(x, "time_col"))
}
