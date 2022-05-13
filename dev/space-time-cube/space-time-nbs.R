# Object created in lines 1-47 of emerging-dev.R
xnb <- x |>
  activate("geometry") |>
  mutate(nb = st_contiguity(geometry),
         wt = st_weights(nb)) |>
  # ensure order is correct
  set_nbs() |>
  activate("geometry")


.loc_col <- attr(xnb, "loc_col")
.time_col <- attr(xnb, "time_col")

# identify number of unique locations and times
# th3ese should be automatically stored in attributes and updated with [.spacetime method
n_locs <- length(unique(activate(xnb, "data")[[.loc_col]]))
n_times <- length(unique(activate(xnb, "data")[[.time_col]]))

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

# Get t = 0 simeslice indexes
# nbt0 = neighbors time 0
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
nbt1 <- nb_time_index_lag(nbt0, n_locs, k)

# combine all lists element-wise
zip_lists = function (lists) {
  do.call('Map', c(`c`, lists))
}

# these are the original neighbors for t =  we then need to combine
# with original nbs (t = 0). For the first obs (1:n_locs) they will
# be missing.
# card for time-lagged neighbors = card * k
nb_tlag <- zip_lists(list(nbt0, nbt1))

# create list for spatial weights
# must filter out wherever card(nbt) == card(nb)
wt_tlag <- zip_lists(replicate(k + 1, rep(wt, n_times), simplify = FALSE))

# for the n_locs * k observations, we select only the first card(nbt)
# obs to omit weights where lag is 0
n_omit <- (n_locs*k)
lag_cards <- lengths(nb_tlag)[1:n_omit]

# iterate through those observations to subset for weights
for (i in 1:n_omit) {
  wt_tlag[[i]] <- wtk[[i]][1:(lag_cards[i])]
}


# combine all lists rowwise
zip_lists = function (lists) {
  do.call('Map', c(`c`, lists))
}
# combine wts lists

# TODO: remove weights elements from 1:n_loc*k for each time-lag

# this will filter out where there was no available lag
index <- (n_omit + 1):(n_locs * n_times)

# These are the values that we'll use to calculate local Gi*
# I cant subset this! Because it messes up the calculation!!!!
# then the row inderxes get shifted and become meaning lessss
# UGH FUCK
all_nbs <- nb_tlag
all_wts <- wt_tlag
xvar <- activate(xnb, "data")[[.var]]
time_index <- activate(xnb, "data")

# Find xj for x and time-lagged xj
xj <- find_xj(xvar, all_nbs)

# Calculate local G on splits
# local G params
n <- n_locs

x <- xvar[1:168]
wj <- all_wts[1:168]
xj <- xj[1:168]

# adapted from the body of spdep::localG



local_g_spt_calc <- function(x, xj, wj) {
  # This comes from the body of spdep::localG()
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
}

# GREAT! Now apply this to every split in the dataset
# TODO try making a version of this function that uses a for loop based on
# indexes. That may actually be faster than creating a dataframe, splitting it,
# lapply, etc etc.
local_g_spt_impl <- function(x, times, nb, wt) {
  x <- data.frame(
    x = x,
    xj = I(find_xj(x, nb)),
    nb = I(nb),
    wt = I(wt),
    time_period = times
  )

  # returns a vector
  unname(unlist(lapply(split(x, x[["time_period"]]),
           function(.x) {
              local_g_spt_calc(
                 .x[["x"]],
                 .x[["xj"]],
                 .x[["nb"]])
           })))

  # returns a data frame
  # do.call(
  #   rbind,
  #   lapply(split(x, x[["time_period"]]),
  #          function(.x) {
  #            data.frame(
  #              gi_star = local_g_spt_calc(
  #                .x[["x"]],
  #                .x[["xj"]],
  #                .x[["nb"]]),
  #              time_period = .x[["time_period"]]
  #            )
  #          })
  # )

}


obs <- local_g_spt_impl(xnb[[.var]],
                        xnb[[.time_col]],
                        all_nbs, all_wts)



reps <- matrix(ncol = 199, nrow = n_locs * n_times)

for (i in 1:nsim) {
  reps[,i] <- local_g_spt_impl(xnb[[.var]],
                            xnb[[.time_col]],
                            cond_permute_nb(all_nbs),
                            all_wts)
}

l <- (rowSums(obs >= reps)  + 1)/ (nsim + 1)
g <- (rowSums(obs <= reps) + 1) / (nsim + 1)

data.frame(
  gi_star = obs,
  p_sim = pmin(l, g)
)

# TODO
local_g_spt_perm <- function(x, times, nb, wt) {
  p_nb = cond_permute_nb(nb)
  #p_wt = find_xj(wt, p_nb)
  res <- local_g_spt_impl(x, times, p_nb, wt)
}


x <- xnb[["value"]]
times <- xnb[["time_period"]]

local_g_spt_perm(x, times, nb, wt)


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



#   function(time_index, n_locs, nb, k) {
#   lapply(nb, function(x) {
#     res <- x + n_locs * (time_index - 1)
#     res <- res - (k * n_locs)
#     na.omit(ifelse(res < 1, NA, res))
#   })
# }





xyz <- cbind(activate(xnb, "data")[, c("time_period", "value")],
             nb = I(all_nbs),
             wt = I(all_wts),
             xj = I(xj))

all_gis <- do.call(
  rbind,
  lapply(split(xyz, xyz[[.time_col]]),
         function(.x) {
           data.frame(
             gi_star = local_g_spt(
               .x[["value"]],
               .x[["xj"]],
               .x[["nb"]]),
             time_period = .x[[.time_col]]
           )
         })
)
