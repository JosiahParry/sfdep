library(sf)
library(dplyr)
library(sfnetworks)

nodes <- st_centroid(sfdep::guerry)

es <- geos::geos_make_collection(nodes) |>
  geos::geos_delaunay_edges() |>
  geos::geos_unnest(keep_multi = FALSE)


# cast to sfnetwork
sfn <- es |>
  st_as_sf() |>
  as_sfnetwork()

plot(sfn)

# an idea as how to get the attributes back to the points
nodes |> st_as_sf() |>
  mutate(id = row_number()) |>
  st_join(sfn |>
            st_as_sf() |>
            mutate(id2 = row_number())) |>
  select(id, id2)


# spdep -------------------------------------------------------------------
library(sf)
library(dplyr)
library(spdep)
library(sfnetworks)

nodes <- st_centroid(sfdep::guerry)

geo <- st_geometry(nodes)
nb <- tri2nb(geo)

plot(nb, geo)

wt <- sfdep::st_nb_dists(geo, nb)

sfdep::st_as_graph(geo, nb, wt) |>
  plot()

# gabriel neighbors
# ensure that it is symmetric to make sure no missing links
nb <- graph2nb(gabrielneigh(geo), sym = TRUE)
wt <- sfdep::st_nb_dists(geo, nb)
sfdep::st_as_graph(geo, nb, wt) |>
  plot()

