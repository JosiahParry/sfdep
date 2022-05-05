#' Two separate cases:
#'
#' 1. Spacetime
#' 2. Spacetime cube
#'
#' Built on the idea of a relational representation of data and geometry.
#' Geometry is always stored in an sf object with non-repeating geometries and
#' data is always stored in a data frame. Data and geometry and linked through
#' a unique identifier—e.g. census tract IDs or other identifiers— refered to
#' as `.loc_col`. In addition, to keep track of time, a time column is required,
#' `.time_col`. The time column does not have to be explicitly a time related
#' column. Panel data, for example, is often represented through other types of
#' value such as integer of character data.
#'
#' Spacetime data can contain any representation of data and geometry as long
#' as there are valid `.loc_col` and `.time_col`s. The supposition is that the user
#' will be actively manipulating `.data` and preparing it for further analysis.
#'
#' In the case of space-time trend analysis, a completed and regular time-series
#' is necessary. Time must be separated by intervals of equal units. Additionally,
#' every location must have a complete time-series. When each location is a
#' regular time-series, we can call this a _"space-time cube."_ Space-time cubes
#' are necessary for emerging hot-spot analysis (spatial trend analysis).
#'
#'
#' TODO / note: if `.time_col` is not a date, numeric, or otherwise order-able
#'    object should it be an ordered factor?
#' TODO set_wts and set_nbs can only be used on regular space-time objects
#'    create `is_regular()` fx to determine regularity (don't set attribute)
#'    as attribute could be set and then observations filtered.
#' TODO create function to set columns from `.geometry` to `.data`
#' TODO spatial interpolation for missing values in space-time cube
#' TODO time-series interpolation for missing values


# spacetime S3 Class implementation
# According to https://adv-r.hadley.nz/s3.html
# S3 classes need a constructor, validator, and helper

# What does an s3 class object need?
# 1. Data Frame with location_id and time_col and any other values values
# 2. An sf object with location_id and geometry
#   - probably set as an attribute
#   - additional attributions containing location_id column name
# NOTE: there needs to be a way to make the dataset complete timeseries
.geometry <- select(x, CT_ID_10)
.data <- guns

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
# TODO Fail if repeating geometries
# TODO create ID column in geometry


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


# Validate spacetime cube -------------------------------------------------

#' Time-series' must be complete. This means that for every time period, every
#' location must have an observation. n = (n time periods) * (n locations)
#' Check for incomplete space-time series
#' Check for # missing bins
#' Check that every location in geometry is in data
#' # time-intervals
#' # bins
spt <- new_spacetime(guns, select(x, CT_ID_10),
              .loc_col = "CT_ID_10",
              .time_col = "year")





validate_spacetime <- function(.data, .geometry, .loc_col, .time_col) {

  if (!inherits(.geometry, "sf")) {
    cli::cli_abort("{.var .geometry} must be an `sf` object.")
  }

  # ensure that there is no missing data in .time_col and .loc_col
  n_miss_loc_data <- sum(is.na(.data[[.loc_col]]))
  if (n_miss_loc_data > 0) cli::cli_abort("Missing locations in .data.")

  n_miss_time_data <- sum(is.na(.data[[.time_col]]))
  if (n_miss_time_data > 0) cli::cli_abort("Missing time periods in .data.")

  # verify no missingness in geometry locations
  n_miss_loc_geo <- sum(is.na(.geometry[[.loc_col]]))

  if (n_miss_loc_geo > 0) {
    cli::cli_abort("Missing location IDs, `.loc_id`, in .geometry.")
  }

  # verify no duplicate geometry
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

  # ensure that
  .data_locs <- sort(unique(.data[[.loc_col]]))
  .geo_locs <- sort(unique(.geometry[[.loc_col]]))

  if (!identical(.data_locs, .geo_locs)) {
    cli::cli_abort(c("Locations differ between .geometry and .data.",
                     i = "There {?is/are} "))
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


validate_spacetime(.data, .geometry, .loc_col, .time_col)


# Complete time-series ----------------------------------------------------

# Functionality for completing a space-time-series if the present one is incomplete

times <- unique(.data[[.time_col]])
locs <- .geometry[[.loc_col]]

complete_spts <- expand.grid(locs = locs, times = times)
names(complete_spts) <- c(.loc_col, .time_col)

.data_class <- class(.data)

res <- merge(complete_spts, .data, by = names(complete_spts))
class(res) <- .data_class
res

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

spt <- as_spacetime(repeating_geo, .loc_col = "CT_ID_10", "year")


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

`[[.spacetime` <- function(x, ...) {
  NextMethod()
}


# Setting Neighbors -------------------------------------------------------

#' To give the end user control over creating their own neighbors and weights
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
#
# if (active(x) == "geometry") cli::cli_abort("`data` must be active.")
#
# n_times <- length(attr(x, "time"))
#
# if (any(table(x[[attr(x, "loc_col")]]) != n_times))
#   cli::cli_abort("Locations don't have observations for each time period.")
#
# # Order based on time period and location column
# regions <- current_locations(x)
# geo_index <- setNames(seq_len(length(regions)), regions)
# new_index <- unlist(purrr::map(1:n_times, ~geo_index * .x))
# # extract nb list
# nb <- attr(x, "geometry")[["nb"]]
# # reorder appropriately
# res <- x[new_index,]
#
# # add nb list
# res[["nb"]] <- rep(nb, n_times)
# res


# Set nb from geometry ----------------------------------------------------

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

  # determine row ordering to match regions
  .loc_col = attr(x, "loc_col")
  geo_locs <- attr(x, "geometry")[[.loc_col]]
  region_index <- setNames(seq_along(geo_locs), geo_locs)
  data_loc_id <- region_index[x[[.loc_col]]]

  # reorder x appropriately by regions and time
  x <- x[order(x[[.loc_col]], data_loc_id),]
  # repeat nb object n_times to fill. Order is correct based on above
  # ordering
  x[[.nb_col]] <- rep(nb, n_times)
  x
}

#
# .loc_col = attr(x, "loc_col")
# geo_locs <- attr(x, "geometry")[[.loc_col]]
# region_index <- setNames(seq_along(geo_locs), geo_locs)
# data_loc_id <- region_index[x[[.loc_col]]]
#
# x <- x[order(x[[.loc_col]], data_loc_id),]
# x[[.nb_col]] <- rep(nb, n_times)


# Set wt from geometry ----------------------------------------------------

set_wts <- function(x, .wt_col = "wt") {
  if (active(x) == "geometry") cli::cli_abort("`data` must be active.")


  n_times <- length(attr(x, "time"))

  if (any(table(x[[attr(x, "loc_col")]]) != n_times)) {
    cli::cli_abort("Locations don't have observations for each time period.")
  }

  # extract wt list
  wt <- attr(x, "geometry")[[.wt_col]]
  if (is.null(nb)) cli::cli_abort("`.wt_col` is missing from `geometry`.")

  # determine row ordering to match regions
  .loc_col = attr(x, "loc_col")
  geo_locs <- attr(x, "geometry")[[.loc_col]]
  region_index <- setNames(seq_along(geo_locs), geo_locs)
  data_loc_id <- region_index[x[[.loc_col]]]

  # reorder x appropriately by regions and time
  x <- x[order(x[[.loc_col]], data_loc_id),]
  # repeat nb object n_times to fill. Order is correct based on above
  # ordering
  x[[.wt_col]] <- rep(wt, n_times)
  x

}


# Spacetime Trend Test ----------------------------------------------------


filt_spt |>
  set_nbs() |>
  set_wts() |>
  group_by(year) |>
  mutate(g = local_g(value, nb, wt)) |>
  ungroup()

sptc |>
  group_by(CT_ID_10) |>
  arrange(year) |>
  summarise(res = data.frame(unclass(Kendall::MannKendall(g)))) |>
  tidyr::unnest(res)

lisa = sptc[["g"]]
loc_col = sptc[["CT_ID_10"]]
time_col = sptc[["year"]]
emerging_hotspot <- function(lisa, loc_col, time_col) {
  df <- data.frame(lisa = class_modify(lisa,
                                       class(unclass(lisa))),
                   loc = loc_col,
                   time = time_col)

  split(df, loc_col)

}
do.call(rbind,
        lapply(split(do.call(rbind, all_gis),
                     complete_obs[[".region_id"]]), function(.x) {
                       data.frame(
                         unclass(Kendall::MannKendall(.x[["gi_star"]]))
                       )
                     }))



# xts imputation ----------------------------------------------------------

# need to check if any regions have completely missing values
#
splits <- split(xx, xx[[attr(xx, "loc_col")]])

i <- 153
xts::xts(splits[[i]]$value, splits[[i]]$year) -> xts_res
xts_res
splits[[20]]
