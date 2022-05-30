
# data prep ---------------------------------------------------------------
library(sf)
library(sfdep)
library(tidyverse)
# devtools::load_all()
# downloaded from BARI ecometrics
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

write_csv(complete_obs, "inst/extdata/bos-ecometric.csv")
sf::write_sf(complete_geo, "inst/extdata/bos-ecometric.geojson")
