library(sf)
library(sfdep)
library(tidyverse)

df_311 <- read_csv("/Users/josiahparry/Downloads/311 Cases 2020_2024 Unrestricted.csv") |>
  janitor::clean_names() |>
  filter(!is.na(y), !is.na(x))


sf_311 <- st_as_sf(df_311, coords = c("x", "y"), crs = 4326)

points_grid <- st_make_grid(sf_311, n = c(5, 5))
g <- st_make_grid(sf_311, n = c(5, 5), square = FALSE)

g2 <- g[st_geometry(sf_311)]

plot(st_convex_hull(st_geometry(sf_311)))
index_intersect <- st_intersects(points_grid, sf_311)

pnt_index <- index_intersect |>
  tibble::enframe() |>
  tidyr::unnest(value) |>
  select(.grid_id = 1, .point_id = 2)


pnts <- st_geometry(sf_311)

pnt_union <- st_union(pnts)
