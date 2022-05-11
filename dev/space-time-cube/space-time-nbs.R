# Object created in lines 1-47 of emerging-dev.R

xnb <- x |>
  activate("geometry") |>
  mutate(nb = st_contiguity(geometry),
         wt = st_weights(nb)) |>
  # ensure order is correct
  set_nbs()


.loc_col <- attr(xnb, "loc_col")
n_locs <- length(attr(xnb, "geometry")[[.loc_col]])
n_times <- length(attr(xnb, "times"))

nb <- xnb[["nb"]]
wt <- xnb[["wt"]]

# identify nb position based on given time index
nb_time_index <- function(time_index, n_locs, nb) {
  lapply(nb, \(x) x + n_locs * (time_index - 1))
}

# get neighbor for positions and time order from a neighbor list
nbt <- unlist(lapply(1:n_times, nb_time_index, n_locs, nb), recursive = FALSE)

# to get time lag = 1 subtract n_locs * lag
# if the lag is 2, need to get union of these two.
# if lagged nb ID is < 1, NA
# specify lag
k = 1

# TODO: do this for more than one lag period
nbt_k <- lapply(nbt, \(.nb) {
  res <- .nb - (k * n_locs)
  na.omit(ifelse(res < 1, NA, res))
} )

# check that this application was correct. And INDEED
all(unlist(lapply(1:1680, \(.i) nbt[[.i]] - nbt_k[[.i]])) == 168)

# Find xj for x and time-lagged xj
xj <- find_xj(xnb$value, nbt)
xtkj <- find_xj(xnb$value, nbt_k)

# union all xjs including time lagged as these are considered neighbors
# must do the samee for weights
all_xjs <- mapply(function(.xj, .xtkj) c(.xj, .xtkj),
                  .xj = xj, .xtkj = xtkj,
                  SIMPLIFY = FALSE)

# duplicate wt as many time periods there are
# we're not going to worry about width at the moment
wtt <- rep(wt, n_times)
wttk <- wtt

wtt[1:n_locs] <- vector("list", n_locs)

# get all weights, however, i think they need to be divided by k + 1
# because now the weights will equal k + 1 rather than 1 except when there hasn't
# been lagged so t=1
all_wts <- mapply(function(x, y) {
  res <- c(x, y)
  if (length(x) == 0) return(res)
  res / 2
}, x = wtt, y = wttk,
SIMPLIFY = FALSE)


# Calculate local G
# observed x
ind <- 1:168
x <- xnb[["value"]][ind]
xj <- all_xjs[ind]
wj <- all_wts[ind]


# In contrast,
# the G∗i statistic includes the value xi in both numerator and denominator:
# G∗i=∑j wijxj / ∑jxj.
# Note that in this case, the denominator is constant across all observations and simply consists of the total sum of all values in the data set.
# in the case of time-lagged, what do we include in the denominator?
# for this case, i am only going to use the x values in our given time slice
# even though we are including values from other time periods


denom <- sum(x)
numerator <- mapply(xj, wj, FUN = function(x, y) sum(x * y))


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

# Notes -------------------------------------------------------------------



# function to append list elements together into vector
c_list <- function(x, y) {
  mapply(function(x, y) c(x, y),
         x, y,
         SIMPLIFY = FALSE)
}


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




