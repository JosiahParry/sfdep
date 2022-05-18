
# Selection ---------------------------------------------------------------

# Selection method
# TODO update `times` attribute when subset and data is active
`[.spacetime` <- function(x, ...) {
  x <- NextMethod()
  context <- active(x)
  .loc_col <- attr(x, "loc_col")
  .time_col <- attr(x, "time_col")

  if (context == "data") {
    times <- sort(unique(x[[.time_col]]))
    n_times <- length(times)
    n_locs <- length(attr(x, "geometry")[[.loc_col]])
  }

  if (context == "geometry") {
    times <- sort(unique(attr(x, "data")[[.time_col]]))
    n_times <- length(times)
    n_locs <- length(x[[.loc_col]])
  }
  attr(x, "n_times") <- n_times
  attr(x, "n_locs") <- n_locs
  x
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
as_sf <- function(x, ...) {
  if (active(x) == "geometry") x <- activate(x, "data")

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


# Print -------------------------------------------------------------------
print.spacetime <- function(x, ...) {
  context <- active(x)
  n_locs <- attr(x, "n_locs")
  n_times <- attr(x, "n_times")
  cli::cli_div(theme = list(rule = list(color = "grey")))
  cli::cli_text(cli::style_bold(cli::style_italic("spacetime ───")))
  cli::cli_text(cli::col_grey("Context:", cli::style_italic("{.var {context}}")))
  cli::cli_text(cli::col_grey("{.emph {n_locs}} locations {.var {attr(x, 'loc_col')}}"))
  cli::cli_text(
    cli::col_grey("{.emph {n_times}} time periods {.var {attr(x, 'time_col')}}")
    )
  cli::cli_rule(cli::col_grey(cli::style_italic("{context} context")))
  NextMethod()
  cli::cli_end()
}
#
