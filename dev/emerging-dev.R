
# data prep ---------------------------------------------------------------
library(sf)
library(sfdep)
library(tidyverse)

x <- read_sf("/Users/josiahparry/Downloads/911 CT Yearly Ecometrics 2010-2020 Shapefile/911 CT Yearly Ecometrics 2010-2020 Shapefile.shp")

ecometric_tidy <- x |>
  st_drop_geometry() |>
  pivot_longer(cols = -CT_ID_10, names_sep = "_",
               names_to = c("ecometric", "year")) |>
  filter(year != "") |>
  mutate(year = paste0("20", year))

guns <- filter(ecometric_tidy, ecometric == "Guns") |>
  rename(.region_id = CT_ID_10) |>
  mutate(time_period = anytime::anydate(year))

regions <- select(x, .region_id = CT_ID_10)

# Identify regions with only missing values -------------------------------
# Creates objects complete_geo and complete_obs
# maybe should create a threshold fore completeness? like 75%+ completeness
splits <- split(guns, guns$.region_id)

incompletes <- do.call(rbind, lapply(splits, function(.x) {
  sum(is.na(.x[["value"]])) == length(.x[["value"]])
}))

incomplete_region_ids <- rownames(incompletes)[which(incompletes)]

complete_index <- !guns[[".region_id"]] %in% incomplete_region_ids
complete_obs <- guns[complete_index, ]

complete_geo <- regions[!regions[[".region_id"]] %in% incomplete_region_ids,]

# remove previous splits
rm(splits)


# cast to spacetime
.var = "value"

spt <- spacetime(complete_obs, complete_geo, ".region_id", "time_period")

x <- spt
# function dev ------------------------------------------------------------

#' TODO arguments to add
#' distance band, for distance weights
#' default uses queen contiguity
#' return final calculation, intermediate (each time period), or both
emerging_hotspot_analysis <- function(x, var, nsim = 199, ...) {
  if (!is_spacetime_cube(x)) cli::cli_abort("`x` is not a spacetime cube.")
  if (active(x) == "geometry") x <- activate(x, "data")

  nb <- include_self(st_contiguity(sf::st_geometry(attr(x, "geometry"))))
  wt <- st_weights(nb)
  listw <- recreate_listw(nb, wt)

  .time_col = attr(x, "time_col")
  .loc_col = attr(x, "loc_col")

  x <- x[, c(.time_col, .loc_col, .var)]

  # TODO ensure splits are ordered properly!!
  all_gis <- lapply(split(x, x[[.time_col]]), function(.x) {
     gis <- spdep::localG_perm(.x[[.var]], listw, nsim = nsim)
    data.frame(gi_star = as.numeric(gis),
               p_sim = as.numeric(attr(gis, "internals")[,"Pr(folded) Sim"]),
               .region_id = .x[[.loc_col]],
               time_period = .x[[.time_col]])
  })

  # Bind together
  trend_res <- do.call(rbind,
          lapply(split(do.call(rbind, all_gis),
                       x[[.loc_col]]), function(.x) {
                         data.frame(
                           unclass(Kendall::MannKendall(.x[["gi_star"]]))
                         )
                       }))
}


splits <- split(do.call(rbind, all_gis), x[[.loc_col]])


xps <- do.call(rbind, all_gis) |>
  filter(.region_id == spt[[.loc_col]][3]) |>
  print()

gs <- xps[["gi_star"]]
ps <- xps[["p_sim"]]
ps <= 0.05

trend_res
threshold = 0.05

# Hotspot clssification ---------------------------------------------------
# https://pro.arcgis.com/en/pro-app/2.8/tool-reference/space-time-pattern-mining/learnmoreemerging.htm#GUID-09587AFC-F5EC-4AEB-BE8F-0E0A26AB9230

#' New Hot Spot : A location that is a statistically significant hot spot for the final time step and has never been a statistically significant hot spot before.

gs <- c(0, 0.15, 0.7, 1.5)
ps <- c(0.7, 0.5, 0.6, 0.01)
sigs <- ps <= threshold
n <- length(gs)

# condition for new hot spot
(sum(sigs) == 1) && sigs[n] && gs[n] > 0



# Intensifying Hot Spot ---------------------------------------------------
#
# A location that has been a statistically significant hot spot for ninety percent
# of the time-step intervals, including the final time step. In addition, the intensity of clustering of high counts in each time step is increasing overall and that increase is statistically significant.
gs <- seq(0, 2, length.out = 15)
ps <- seq(0.051, 0.00000001, length.out = 15)
sigs <- ps <= threshold
n <- length(gs)
first_obs <- n - floor(n * 0.9)

# check that 90% of spots have been increasing
sum(sigs) / n  >= .9

# check last spot is significant
sigs[n]

# check that intensity is increasing and is significant for 90% and increasing
all(sigs[first_obs:n]) & !is.unsorted(gs[first_obs:n])

# Persistent Hot Spot
#
# A location that has been a statistically significant hot spot for ninety percent of the time-step intervals with no discernible trend indicating an increase or decrease in the intensity of clustering over time.
#
# Diminishing Hot Spot
#
# A location that has been a statistically significant hot spot for ninety percent of the time-step intervals, including the final time step. In addition, the intensity of clustering in each time step is decreasing overall and that decrease is statistically significant.
#
# Sporadic Hot Spot
#
# A location that is an on-again then off-again hot spot. Less than ninety percent of the time-step intervals have been statistically significant hot spots and none of the time-step intervals have been statistically significant cold spots.
#
# Oscillating Hot Spot
#
# A statistically significant hot spot for the final time-step interval that has a history of also being a statistically significant cold spot during a prior time step. Less than ninety percent of the time-step intervals have been statistically significant hot spots.
#
# Historical Hot Spot
#
# The most recent time period is not hot, but at least ninety percent of the time-step intervals have been statistically significant hot spots.
#
# New Cold Spot
#
# A location that is a statistically significant cold spot for the final time step and has never been a statistically significant cold spot before.
#
# Consecutive Cold Spot
#
# A location with a single uninterrupted run of statistically significant cold spot bins in the final time-step intervals. The location has never been a statistically significant cold spot prior to the final cold spot run and less than ninety percent of all bins are statistically significant cold spots.
#
# Intensifying Cold Spot
#
# A location that has been a statistically significant cold spot for ninety percent of the time-step intervals, including the final time step. In addition, the intensity of clustering of low counts in each time step is increasing overall and that increase is statistically significant.
#
# Persistent Cold Spot
#
# A location that has been a statistically significant cold spot for ninety percent of the time-step intervals with no discernible trend, indicating an increase or decrease in the intensity of clustering of counts over time.
#
# Diminishing Cold Spot
#
# A location that has been a statistically significant cold spot for ninety percent of the time-step intervals, including the final time step. In addition, the intensity of clustering of low counts in each time step is decreasing overall and that decrease is statistically significant.
#
# Sporadic Cold Spot
#
# A location that is an on-again then off-again cold spot. Less than ninety percent of the time-step intervals have been statistically significant cold spots and none of the time-step intervals have been statistically significant hot spots.
#
# Oscillating Cold Spot
#
# A statistically significant cold spot for the final time-step interval that has a history of also being a statistically significant hot spot during a prior time step. Less than ninety percent of the time-step intervals have been statistically significant cold spots.
#
# Historical Cold Spot
#
# The most recent time period is not cold, but at least ninety percent of the time-step intervals have been statistically significant cold spots.




# Identify nb and wts -----------------------------------------------------
nb <- include_self(st_contiguity(st_geometry(complete_geo)))
wt <- st_weights(nb)
listw <- sfdep:::recreate_listw(nb, wt)

# calculate localGi* ------------------------------------------------------
all_gis <- lapply(split(complete_obs, complete_obs[["time_period"]]), function(.x) {
  gis <- spdep::localG(.x[["value"]], listw)
  data.frame(gi_star = as.numeric(gis), .region_id = .x[[".region_id"]],
             time_period = .x[["time_period"]])
})



# Calculate Mann Kendall Test ---------------------------------------------


# Bind together
do.call(rbind,
        lapply(split(do.call(rbind, all_gis),
      complete_obs[[".region_id"]]), function(.x) {
        data.frame(
          unclass(Kendall::MannKendall(.x[["gi_star"]]))
        )
      }))

