# Functionality for completing a space-time-series if the present one is incomplete


#' Convert spacetime object to spacetime cube
#'
#' Given a spacetime object, convert it to a spacetime cube. A spacetime cube
#' ensures that there is a regular time-series for each geometry present.
#'
#' If observations are missing for a time period and location combination,
#' columns will be populated with NAs.
#'
#' See [`is_spacetime_cube()`] for more details on spacetime cubes.
#'
#' @param x a spacetime object.
#' @param ... unused
#' @export
complete_spacetime_cube <- function(x, ...) {
  # if already spacetime cube return x
  suppressMessages({
    if (is_spacetime_cube(x)) return(x)
  })

  # activate data if not already
  if (active(x) == "geometry") x <- activate(x, "data")

  .loc_col <- attr(x, "loc_col")
  .time_col <- attr(x, "time_col")
  locs <- attr(x, "locs")
  times <- attr(x, "times")

  # if there are more than 1 observation per-time period
  # exit as it cannot be regular
  if (any(table(x[[.time_col]], x[[.loc_col]]) > 1)) {
    cli::cli_abort(c(
      "Location and time combinations are not unique.",
      i = "There should only be one observation per time and location combination."
    ))
  }

  complete_spts <- expand.grid(locs = locs,
                               times = times,
                               stringsAsFactors = FALSE)

  names(complete_spts) <- c(.loc_col, .time_col)

  .data_class <- setdiff(class(x), "spacetime")

  res <- merge(complete_spts, x, by = names(complete_spts), all.x = TRUE)
  class(res) <- .data_class

  new_spacetime(res, attr(x, "geometry"), .loc_col, .time_col)

}
