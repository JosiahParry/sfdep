emerging_hotspot_analysis <- function(x, .var, k, include_gi = FALSE,
                                      nb_col = NULL, wt_col = NULL,
                                      nsim = 199, threshold = 0.05, ...) {

  x <- spt_order(x)

  .loc_col <- attr(x, "loc_col")
  .time_col <- attr(x, "time_col")
  times <- x[[.time_col]]
  # identify number of lcoations and times
  # TODO these should be automatically stored in attributes and updated with [.spacetime method
  n_locs <- length(unique(x[[.loc_col]]))
  n_times <- length(unique(times))

  if (any(is.null(nb_col), is.null(wt_col))) {
    # create neighbors and weights if not present
    nb <- include_self(st_contiguity(sf::st_geometry(attr(x, "geometry"))))
    wt <- st_weights(nb)
  } else {
    nb <- attr(x, "geometry")[[nb_col]]
    wt <- attr(x, "geometry")[[wt_col]]
  }

  # convert nb and wt to time-lagged spacetime
  nbt <- spt_nb(nb, n_times, n_locs, k)
  wtt <- spt_wt(wt, nbt, n_times, n_locs, k)

  all_gis <- local_g_spt(x[[.var]], times, nbt, wtt, n_locs, nsim = nsim)

  res_ehs <- cbind(
    location = attr(x, "geometry")[[.loc_col]],
    do.call(
      rbind,
      lapply(split(all_gis, x[[.loc_col]]), function(.x) {
        classify_hotspot(.x, threshold)
      })
    )
  )

  if (include_gi) {
    attr(res_ehs, "gi_star") <- all_gis
  }

  res_ehs



}


# tictoc::tic()
# res <- emerging_hotspot_analysis(x, "value", 1, nsim = 199, include_gi = TRUE)
# tictoc::toc()
