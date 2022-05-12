# Object created in lines 1-47 of emerging-dev.R
xnb <- x |>
  activate("geometry") |>
  mutate(nb = st_contiguity(geometry),
         wt = st_weights(nb)) |>
  # ensure order is correct
  set_nbs() |>
  activate("geometry")


.loc_col <- attr(xnb, "loc_col")
n_locs <- length(attr(xnb, "geometry")[[.loc_col]])
n_times <- length(attr(xnb, "times"))

nb <- xnb[["nb"]]
wt <- xnb[["wt"]]


# Parameters
k = 1

# identify nb position based on given time index
# to get time lag = 1 subtract n_locs * lag
# if the lag is 2, need to get union of these two.
# if lagged nb ID is < 1, NA
# specify lag
# If k = 1 then all nbs for the lag at t = 1 are missing
# Then all nbs at t = 2 are those from t = 1

# Get original time index for a single observation
nb_time_index_obs <- function(time_index, n_locs, nb) {
  lapply(nb, function(x) x + n_locs * (time_index - 1))
}

# Get original time index for all observations
nb_time_index <- function(n_times, n_locs, nb) {
  unlist(
    lapply(1:n_times, nb_time_index_obs, n_locs, nb),
    recursive = FALSE
    )
}

nbt0 <- nb_time_index(n_times, n_locs, nb)


# Given the original time-index, identify the lagged time
# TODO: do this for more than one lag period
nb_time_index_lag <- function(nbt, n_locs, k) {
    lapply(nbt, function(.nb) {
    res <- .nb - (k * n_locs)
    na.omit(ifelse(res < 1, NA, res))
  })
}

# identify time lag for k
nbt1 <- nb_time_index_lag(nbt, n_locs, k)

#   function(time_index, n_locs, nb, k) {
#   lapply(nb, function(x) {
#     res <- x + n_locs * (time_index - 1)
#     res <- res - (k * n_locs)
#     na.omit(ifelse(res < 1, NA, res))
#   })
# }

# these are the original neighbors for t =  we then need to combine
# with original nbs (t = 0). For the first obs (1:n_locs) they will
# be missing.
# card for time-lagged neighbors = card * k

nb_tlag <- Map(c, rep(nb, n_times), nbt)

# create list for spatial weights
# must filter out wherever card(nbt) == card(nb)
wt_tlag <- Map(c, rep(wt, n_times), rep(wt, n_times))

wtk <- replicate(k + 1, rep(wt, n_times), simplify = FALSE)

# combine all lists rowwise
zip_lists = function (lists) {
  do.call('Map', c(`c`, lists))
}

# combine wts lists
zip_lists(wtk)

# TODO: remove weights elements from 1:n_loc*k for each time-lag


# this will filter out where there was no available lag
index <- (lengths(nbt0) != lengths(nb_tlag))

# These are the values that we'll use to calculate local Gi*
all_nbs <- nb_tlag[index]
all_wts <- wt_tlag[index]
xvar <- xnb[[.var]][index]
time_index <- xnb[[.time_col]][index]

# Find xj for x and time-lagged xj
xj <- find_xj(xvar, all_nbs)

# Calculate local G on splits
# local G params
n <- n_locs

x <- xvar[1:168]
wj <- all_wts[1:168]
xj <- xj[1:168]

# adapted from the body of spdep::localG
n <- length(wj)
xibar <- rep(mean(x), n)
lx <- mapply(xj, wj, FUN = function(x, y) sum(x * y))
si2 <- rep(sum(scale(x, scale = FALSE)^2)/n, n)
Wi <- sapply(wj, sum)
S1i <- sapply(wj, function(x) sum(x^2))
EG <- Wi * xibar
res <- (lx - EG)
VG <- si2 * ((n * S1i - Wi^2)/(n - 1))
res <- res/sqrt(VG)
res
# Notes -------------------------------------------------------------------

# nb_time_index <- function(time_index, n_locs, nb) {
#   lapply(nb, function(x) x + n_locs * (time_index - 1))
# }


# include time lagged nbs
n_time_periods = 1
xx <- x |>
  activate("geometry") |>
  mutate(nb = st_contiguity(geometry),
         wt = st_weights(nb)) |>
  set_nbs() |>
  set_wts() |>
  group_by(time_period) |>
  mutate(value_nbs = find_xj(x = value, nb)) |>
  group_by(.region_id) |>
  # create time-lag neighbors for variable of interest
  mutate(value_nb_lag = dplyr::lag(value_nbs,
                                   k = n_time_periods))
# we then need to filter out the lagged time-periods
# we then effectively have n_times - n_lagged_periods
# so if we need 10 obs but lag 1, we need n_times == 11
# then after this is all filtered out, calculate Gi*
xyz <- xx |>
  filter(time_period == "2015-01-01")




