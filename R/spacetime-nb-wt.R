
# Neighbors ---------------------------------------------------------------
#' Create time lagged spatial neighbors
#'
#' Given a an spdep neighbor list (or any other list indicating row position)
#' of neighbors in a `spacetime` object's geometry context, create a neighbor
#' list across space and time in the `data` context.
#'
#' Note that this should only be used for spacetime cubes.
#'
#' This function is only available for internal development and should be used
#' with extreme caution.
#'
#' @param nb a neighbors list created from a spacetime object's geometry context.
#' @param n_times the number of time slices
#' @param n_locs the number of locations
#'
#' @details
#'
#' The number of bins is equal to `n_times * n_locs` where each location has
#' a bin time-series with `n_times` observations. This is what makes it a
#' spacetime cube function.
#'
#' @seealso [spt_wt()] for converting spacetime neighbors to spacetime weights.
#' @references https://pro.arcgis.com/en/pro-app/2.8/tool-reference/space-time-pattern-mining/create-space-time-cube.htm
#'
#' @keywords internal
spt_nb <- function(nb, n_times, n_locs, k = 1) {
  # if k is negative, exit
  if (k < 0) cli::cli_abort("`k` ({k}) must be a positive integer.")
  # if k isn't an integer, exit
  if ((k %% 1) != 0) cli::cli_abort("`k` ({k}) must be a positive integer")

  nbt0 <- nb_time_index(n_times, n_locs, nb)

  # If k = 0 have an early exit
  if (k == 0) return(nbt0)

  nb_lag_k <- lapply(1:k, function(.k) nb_time_index_lag(nbt0, n_locs, .k))
  zip_lists(c(list(nbt0), nb_lag_k))
}

# nb_time_lag(nb_geo, n_times, n_locs)

# Get original time index for a single time-slice
nb_time_index_obs <- function(time_index, n_locs, nb) {
  lapply(nb, function(x) x + n_locs * (time_index - 1))
}

# Get original time index for all time-slices
nb_time_index <- function(n_times, n_locs, nb) {
  unlist(
    lapply(1:n_times, nb_time_index_obs, n_locs, nb),
    recursive = FALSE
  )
}


# Given the original time-index, identify the lagged time
nb_time_index_lag <- function(nbt, n_locs, k) {
  lapply(nbt, function(.nb) {
    res <- .nb - (k * n_locs)
    stats::na.omit(ifelse(res < 1, NA, res))
  })
}



# Weights -----------------------------------------------------------------
# nbt <- spt_nb(nb, n_times, n_locs, k)

# Neighbors are simpler because we can rely on the nb

#' Create time lagged spatial weights
#'
#' Given a space-time-lagged neighbor list and a weights lists, e.g. made by
#' [st_dist_band()] or [st_contiguity()], create a corresponding weights list.
#'
#' @param wt a weights lists created from a spacetime object's geometry context.
#' @param nbt a time-lagged spatial neighbors list created by [`spt_nb()`]
#' @inheritParams spt_nb
#'
#' @details
#'
#' It is intended that [`spt_wt()`] is used following the use of [`spt_nb()`]
#' using the same input arguments. This ensures that the time-lagged spatial
#' weights are correctly associated with the time-lagged spatial neighbors.
#'
#' @seealso [spt_nb()] for creating spacetime neighbors which are inputs into spacetime weights
#'
#' @keywords internal
spt_wt <- function(wt, nbt, n_times, n_locs, k) {
  wt_tlag <- zip_lists(replicate(k + 1, rep(wt, n_times), simplify = FALSE))

  # for the n_locs * k observations, we select only the first card(nbt)
  # obs to omit weights where lag is 0
  n_omit <- (n_locs * k)
  lag_cards <- lengths(nbt)[1:n_omit]

  # iterate through those observations to subset for weights
  if (n_omit > 0) {
    for (i in 1:n_omit) {
      wt_tlag[[i]] <- wt_tlag[[i]][1:(lag_cards[i])]
    }
  }

  wt_tlag
}

# wtt <- spt_wt(wt, nbt, n_times, n_locs, k)
# lengths(wtt) == lengths(nbt)
# Utils -------------------------------------------------------------------


# combine all lists element-wise
zip_lists = function (lists) {
  do.call('Map', c(`c`, lists))
}
