# spacetime S3 Class implementation
# According to https://adv-r.hadley.nz/s3.html
# S3 classes need a constructor, validator, and helper

# What does an s3 class object need?
# 1. Data Frame with location_id and time_col and any other values values
# 2. An sf object with location_id and geometry
#   - probably set as an attribute
#   - additional attributions containing location_id column name
# NOTE: there needs to be a way to make the dataset complete timeseries
geometry <- select(x, CT_ID_10)
x <- guns

#' We can switch between activating the data and the geometry.
#' One may want to activate the geometry for calculating spatial neighbors
#' and weights. Activating the data can be used for aggregations.
#' To do this, we create an attribute "active" which informs us of which
#' object is present and which is contained in the attributes.
#'
#' There must be a function to construct a spacetime object with active
#' geometry and then active data.


# Class constructors ------------------------------------------------------
# TODO check for unique repeating geometries in "geometry".
# Fail if repeating geometries

new_spacetime <- function(.data, .geometry, .loc_col, .time_col, active = "data") {
  match.arg(active, c("data", "geometry"))
  switch(active,
         geometry = new_spacetime_geo(.data, .geometry, .loc_col, .time_col),
         data = new_spacetime_data(.data, .geometry, .loc_col, .time_col),
         )
}

new_spacetime_geo <- function(.data, .geometry, .loc_col, .time_col) {
  times <- sort(unique(.data[[.time_col]]))
  structure(.geometry,
            active = "geometry",
            data = .data,
            loc_col = .loc_col,
            time_col = .time_col,
            times = times,
            class = c("spacetime", class(.geometry)))
}

new_spacetime_data <- function(.data, .geometry, .loc_col, .time_col) {

  times <- sort(unique(.data[[.time_col]]))

  structure(.data,
            geometry = .geometry,
            loc_col = .loc_col,
            time_col = .time_col,
            time = times,
            class = c("spacetime", class(.data)),
            active = "data")

}



# Casting functions -------------------------------------------------------
repeating_geo <- left_join(x, geometry) |>
  st_as_sf()


as_spacetime <- function(x, ...) {
  UseMethod("as_spacetime")
}

as_spacetime.sf <- function(x, .loc_col, .time_col, ...) {
  geometry <- sf:::distinct.sf(x, !!rlang::sym(.loc_col),
                               !!rlang::sym(attr(x, "sf_column")))

  new_spacetime_data(st_drop_geometry(x), geometry,
                     .loc_col, .time_col)
}

as_spacetime(repeating_geo, .loc_col = "CT_ID_10", "year")


# Activation --------------------------------------------------------------

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


# spt <- as_spacetime(repeating_geo, "CT_ID_10", "year")
#
# activate_geometry(spt) |>
#   activate_data()
#
# activate(spt, "geometry") |>
#   activate("data")

# st_as_sf ----------------------------------------------------------------

# convert spacetime cube to sf object
st_as_sf.spacetime <- function(x, ...) {
  merge(attr(x, "geometry"), x,
        by = attr(x, "loc_col"))
}

# st_as_sf(spt)

# Selection method
`[.spacetime` <- function(x, ...) {
  x <- NextMethod()
}


# Setting Neighbors -------------------------------------------------------

#'vTo give the end user control over creating their own neighbors and weights
#' matrixes, it is important that neighbors are created on the activated
#' geometry sf object.
#' Then, it is the role of sfdep to propagate those neighbor values to `data`.
#' It is important that each time period has only one of each value in `loc_col`.
#' Additionally, it is important that each time period has all of the geometries
#' present in `geometry`.
#'

# Should an attribute be kept of all present geometries values? Then, that attribute can be used to filter




# Identify all present location -------------------------------------------


#' Return all location IDs present in geometry
#'
#' Note that if used in a pipe, it is recommended to use the magrittr pipe to
#' provide the data with the `.` placeholder.
current_locations <- function(x, ...) {
  is_active <- active(x)
  loc_col <- attr(x, "loc_col")
  if (is_active == "geometry") res <- unique(x[[loc_col]])
  if (is_active == "data") res <- unique(attr(x, "geometry")[[loc_col]])
  res
}

missing_locs <- spt |>
  group_by(CT_ID_10) |>
  summarise(missing_pct = sum(is.na(value)) / n()) |>
  # arrange(-missing_pct) |>
  filter(missing_pct > 0.1) |>
  pull(CT_ID_10)

filt_spt <- spt |>
  activate("geometry") |>
  filter(!CT_ID_10 %in% missing_locs) |>
  mutate(nb = st_contiguity(geometry),
         wt = st_weights(nb)) |>
  activate("data") %>%
  filter(!CT_ID_10 %in% missing_locs)


#
# spt |>
#   activate("geometry") |>
#   as_tibble() |>
#   sample_n(10) |>
#   st_as_sf() |>
#   activate("data") %>%
#   filter(CT_ID_10 %in% current_locations(.))
# -------------------------------------------------------------------------
x <- filt_spt
# verify data is correct
filt_spt |>
  count(CT_ID_10, year) |>
  filter(n > 1)

if (active(x) == "geometry") cli::cli_abort("`data` must be active.")

n_times <- length(attr(x, "time"))

if (any(table(x[[attr(x, "loc_col")]]) != n_times))
  cli::cli_abort("Locations don't have observations for each time period.")

# Order based on time period and location column
regions <- current_locations(x)
geo_index <- setNames(seq_len(length(regions)), regions)
new_index <- unlist(purrr::map(1:n_times, ~geo_index * .x))
# extract nb list
nb <- attr(x, "geometry")[["nb"]]
# reorder appropriately
res <- x[new_index,]

# add nb list
res[["nb"]] <- rep(nb, n_times)
res


# Function for retrieving neighbors and weights from geometry
# PRESERVING ORDER IS VERY VERY IMPORTANT HERE!!!!!

set_nbs <- function(x, .nb_col = "nb") {
  if (active(x) == "geometry") cli::cli_abort("`data` must be active.")

  n_times <- length(attr(x, "time"))

  if (any(table(x[[attr(x, "loc_col")]]) != n_times))
    cli::cli_abort("Locations don't have observations for each time period.")

  # extract nb list
  nb <- attr(x, "geometry")[[.nb_col]]
  if (is.null(nb)) cli::cli_abort("`.nb_col` is missing from `geometry`.")

  # Order based on time period and location column
  regions <- current_locations(x)
  geo_index <- setNames(seq_len(length(regions)), regions)
  new_index <- unlist(purrr::map(1:n_times, ~geo_index * .x))

  # reorder appropriately
  res <- x[new_index,]

  # add nb list
  res[[.nb_col]] <- rep(nb, n_times)
  res

}



filt_spt |>
  set_nbs("nb")

# if (active(x) == "data")
nb <- attr(xx, "geometry")[[.nb_col]]
nb <- setNames(nb, attr(nb, "region.id"))

xx$nb <- nb[xx[[attr(xx, "loc_col")]]]

# Order based on time period and location column
n_periods <- length(attr(xx, "time"))
ind <- setNames(1:nrow(geometry), attr(xx, "geometry")[["CT_ID_10"]])
xx[order(xx$year, rep(ind, length(attr(xx, "time")))),]

xx$nb <- sfdep:::class_modify(rep(nb, n_periods), "nb")
xx |>
  group_by(CT_ID_10) |>
  mutate(wt = st_weights(nb),
         value = zoo::na.spline(value)) |>
  ungroup() |>
  pull(value)
  group_by(year) |>
  summarise(g = local_g(value, nb, wt))

# xts imputation ----------------------------------------------------------

# need to check if any regions have completely missing values
#
splits <- split(xx, xx[[attr(xx, "loc_col")]])

i <- 153
xts::xts(splits[[i]]$value, splits[[i]]$year) -> xts_res
xts_res
splits[[20]]
