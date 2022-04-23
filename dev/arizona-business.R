library(sf)
library(tidyverse)

pnts_raw <- read_sf("/Users/josiahparry/Downloads/Employers_Having_Five_or_More_Employees%2C_Arizona%2C_2019.geojson")


st_geometry(pnts_raw) |>
  st_union() |>
  st_con
st_convex_hull(st_geometry(pnts_raw)) |> plot()


center_circle <- pnts_raw |>
  filter(OBJECTID == 69779) |>
  st_geometry() |>
  st_buffer(dist = 10000)


pnts_new <- st_intersection(pnts_raw, center_circle) |>
  janitor::clean_names()

cats <- pnts_new |>
  as_tibble() |>
  count(naics2_des, sort = TRUE) |>
  slice_max(n, n = 7) |>
  pull(naics2_des)

pnts <- pnts_new |>
  filter(naics2_des %in% cats) |>
  # removing duplicate point locations
  # not really here for analytical accuracy
  distinct(geometry, .keep_all = TRUE) |>
  select(objectid, emp_name, naics2_des, sub_cluster, cluster, employees) |>
  mutate(nb = st_knn(geometry, 5))

pnts

cond_perm <- function(nb) {
  n <- length(nb)
  cards <- lengths(nb)
  res <- mapply(shuffle_nbs, 1:n, n, cards, SIMPLIFY = FALSE)
  res
}

cond_perm(pnts$nb)
A <- as.factor(pnts$naics2_des)



global_colocation(pnts$naics2_des, pnts$nb)

