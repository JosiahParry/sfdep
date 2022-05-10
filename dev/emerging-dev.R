
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
splits <- split(guns, guns[[".region_id"]])
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
nsim = 199

spt <- spacetime(complete_obs, complete_geo, ".region_id", "time_period")

x <- spt
# function dev ------------------------------------------------------------

#' TODO incorporate time-lagged neighbors
#' TODO arguments to add own neighbors and wieghts
#' TODO arg for return final calculation, intermediate (each time period), or both
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

  x <- do.call(rbind, all_gis)
  # Bind together
  trend_res <- do.call(rbind,
          lapply(split(x, x[[.loc_col]]), function(.x) {
                         data.frame(
                           unclass(Kendall::MannKendall(.x[["gi_star"]]))
                         )
                       }))
}


df <- do.call(rbind, all_gis)

splits <- split(x, x[[.loc_col]])


xps <- do.call(rbind, all_gis) |>
  filter(.region_id == spt[[.loc_col]][3]) |>
  print()

gs <- xps[["gi_star"]]
ps <- xps[["p_sim"]]
ps <= 0.05

trend_res
threshold = 0.05




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

