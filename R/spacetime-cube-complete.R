# Functionality for completing a space-time-series if the present one is incomplete
complete_spacetime_cube <- function(x, ...) {
  # if already spacetime cube return x
  suppressMessages({
    if (is_spacetime_cube(x)) return(x)
  })

  # activate data if not already
  if (active(x) == "geometry") x <- activate(x, "data")

  .time_col <- attr(x, "time_col")
  .loc_col <- attr(x, "loc_col")

  times <- unique(x[[.time_col]])
  locs <- attr(x, "geometry")[[.loc_col]]

  complete_spts <- expand.grid(locs = locs, times = times)
  names(complete_spts) <- c(.loc_col, .time_col)

  .data_class <- class(x)

  res <- merge(complete_spts, x, by = names(complete_spts), all.x = TRUE)
  class(res) <- .data_class

  new_spacetime(res, attr(x, "geometry"), .loc_col, .time_col)

}
