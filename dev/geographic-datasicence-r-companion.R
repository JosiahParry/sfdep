library(sf)

countries <- read_sf("https://raw.githubusercontent.com/gdsbook/book/master/data/countries/countries_clean.gpkg")


head(countries)

st_geometry_type(countries, by_geometry = FALSE)

countries |>
  st_geometry() |>
  sf::st_simplify(dTolerance = 100) |>
  plot()


# countries is big and takes too long to plot # simplifying
world <- st_simplify(countries, dTolerance = 2000)

library(ggplot2)
library(dplyr)



ggplot(world) +
  geom_sf(lwd = 0.2, color = "black",
          fill = NA) +
  geom_sf(
    data = mutate(countries, geom = st_centroid(geom)),
    mapping = aes(color = ADMIN)
  ) +
  # remove legend
  theme_light() +
  guides(color = "none")


world |>
  filter(ADMIN == "Bolivia") |>
  ggplot() +
  geom_sf()


world |>
  filter(ADMIN == "Indonesia") |>
  ggplot() +
  geom_sf()


gt_points <- readr::read_csv("https://raw.githubusercontent.com/gdsbook/book/master/data/tokyo/tokyo_clean.csv") |>
  # clean names using janitor
  janitor::clean_names()

# convert to sf object
gt_points <- st_as_sf(gt_points,
                      coords = c("longitude", "latitude"),
                      crs = 4326)


# No idea how to do raster work -------------------------------------------



pop <- stars::read_stars("https://raw.githubusercontent.com/gdsbook/book/master/data/ghsl/ghsl_sao_paulo.tif")

sf::st_coordinates(pop)
sf::st_as_sf(pop) |>
  mutate(x = ghsl_sao_paulo.tif) -> pop_sf


ggplot(pop_sf, aes(fill = log(x))) +
  geom_sf()



# OSMNX netowrk -----------------------------------------------------------

# read it in
g <- igraph::read_graph("https://raw.githubusercontent.com/gdsbook/book/master/data/cache/yoyogi_park_graph.graphml",
                        format = "graphml")

# covert to tbl_graph
nodes <- tidygraph::as_tbl_graph(g)


# create notes geometry
n_geo <- as_tibble(nodes) |>
  transmute(across(c(x, y), as.numeric)) |>
  st_as_sf(coords = c("x", "y"), crs = 4326) |>
  st_geometry()


g_nodes <- nodes |>
  mutate(geometry = n_geo)

g_sf <- sfnetworks::as_sfnetwork(g_nodes)

plot(g_sf)


# n nodes
igraph::gorder(g_sf)

# n edges
igraph::gsize(g_sf)

# gab a single element
g_sf |>
  filter(id == 1520546819)

# create loko up for integer positions
lu <- setNames(as.integer(V(g_sf)), pull(g_sf, id))

g_sf |>
  sfnetworks::activate(edges) |>
  filter(from == lu["3010293622"],
         to == lu["1520546819"])


g_sf |>
  sfnetworks::activate(nodes) |>
  mutate(id_nbs = tidygraph::node_is_adjacent(id == 1520546819)) |>
  filter(id_nbs)




# Spatial weights chapter -------------------------------------------------
library(sf)
library(sfdep)

gdf <- st_make_grid(cellsize = c(1, 1), n = c(3, 3), offset = c(0,0))

# rook

nb <- st_contiguity(gdf, queen = FALSE)

graph <- st_as_graph(gdf, nb, st_weights(nb))

plot(gdf)
plot(graph, add = TRUE, col = "red", lty = 2)


nb[1:9]

nb_mat <- nb_as_matrix(nb)
nb_mat

nonzero <- sum(nb_mat != 0)
nonzero

# queen

nb <- st_contiguity(gdf)
graph <- st_as_graph(gdf, nb, st_weights(nb))
plot(gdf)
plot(graph, add = TRUE, col = "red", lty = 2)


wt <- st_weights(nb, style = "B")
wt

cards <- st_cardinalties(nb)

hist(cards, breaks = 1:max(cards))

szero(wt)

pct_nonzero <- sum(lengths(nb)) / length(nb)^2

library(dplyr)
library(ggplot2)

sandiego <- sf::read_sf("https://raw.githubusercontent.com/gdsbook/book/master/data/sandiego/sandiego_tracts.gpkg") |>
  mutate(nb = st_contiguity(geom),
         wt = st_weights(nb))


nodes <- st_as_nodes(sandiego, nb)
edges <- st_as_edges(sandiego, nb, wt)

gg <- ggplot() +
  geom_sf(data = sandiego, fill = NA,
          color = "black", lwd = 0.25) +
  geom_sf(data = edges, lwd = 0.15, lty = 2, color = "red") +
  geom_sf(data = nodes, size = 0.5, alpha = 0.75) +
  theme_void()

# first plot on left
gg

# cropped one on the right
gg +
  coord_sf(xlim = c(-13040000,  -13020000),
           ylim = c(3850000, 3860000))

pct_nonzero <- (sum(lengths(sandiego$nb)) / length(sandiego$nb)^2) * 100


s <- sandiego |>
  mutate(cards = st_cardinalties(nb))


ggplot(s, aes(cards)) +
  geom_histogram(bins = length(unique(s$cards)))

s_rook <- sandiego |>
  mutate(nb = st_contiguity(geom, queen = FALSE),
         wt = st_weights(nb),
         cards = st_cardinalties(nb))

pct_nonzero(s_rook$nb)

ggplot(s_rook, aes(cards)) +
  geom_histogram(bins = length(unique(s_rook$cards)))


## sao paulo
sp_sf <- st_as_sf(pop)

sp_nb <- sp_sf |>
  mutate(nb = st_contiguity(geometry),
         wt = st_weights(nb))

pct_nonzero(sp_nb$nb)

sp_nb |>
  mutate(cards = st_cardinalties(nb)) |>
  ggplot(aes(cards)) +
  geom_histogram(bins = 7)

# Knn

sd_knn <- sandiego |>
  mutate(nb = st_knn(geom, k = 4),
         cards = st_cardinalties(nb))

sd_knn |>
  as_tibble() |>
  mutate(is_island = cards == 0) |>
  count(is_island)

sd_knn |>
  as_tibble() |>
  count(cards)

sandiego |>
  mutate(nb = st_knn(geom, k = 2),
         wt = st_kernel_weights(nb, geom, "triangular")) |>
  pull(wt) |>
  head()


# sub_30 = san_diego_tracts.query("sub_30 == True")
sub30 <- sandiego |>
  filter(sub_30 == TRUE) |>
  mutate(nb = include_self(st_knn(geom, k = 15)),
         wt = st_kernel_weights(nb, geom,
                                "triangular",
                                adaptive = TRUE),
         wt_mat = wt_as_matrix(nb, wt))

# first location
sub30 |>
  ggplot() +
  geom_sf(aes(fill = wt_mat[1,]),
          color = "black", lwd = 0.25) +
  geom_sf(data = st_centroid(sub30[1, "geom"])) +
  scale_fill_viridis_c(option = "plasma") +
  theme_void()

# 18th location
sub30 |>
  ggplot() +
  geom_sf(aes(fill = wt_mat[18,]),
          color = "black", lwd = 0.25) +
  geom_sf(data = st_centroid(sub30[18, "geom"])) +
  scale_fill_viridis_c(option = "plasma") +
  theme_void()

st_contiguity(sub30$geom) |>
  pct_nonzero()
pct_nonzero(sub30$wt)
pct_nonzero


gdf <- st_make_grid(cellsize = c(1, 1), n = c(3, 3), offset = c(0,0))


gdf |>
  st_as_sf() |>
  as_tibble() |>
  st_as_sf() |>
  mutate(nb = st_knn(x, k = 2),
         wt = st_kernel_weights(nb, x, "triangular")) |>
  pull(wt)

critical_threshold(gdf)


# Distance Band & Hybrid Weights ------------------------------------------


nb <- st_dist_band(gdf, upper = 1.5)

# Create binary weights
wt_b <- st_weights(nb, style = "B")

wt_b[[5]]

# Create inverse distance weights
wt_inverse <- st_inverse_distance(nb, gdf, scale = 1, alpha = 1)

wt_inverse[[5]]


# Great Circle Distances --------------------------------------------------

data_path <- "https://raw.githubusercontent.com/gdsbook/book/master/data"

tx <- read_sf("/Users/josiahparry/Downloads/book-master/data/texas/texas.shp")

# By default sf, spdep, and sfdep use s2. So knn measure _are_ good.
knn4_good <- tx |>
  mutate(nb = st_knn(geometry, k = 4))


knn4_good$nb[[1]]

# to make bad points, we have to specify the longlat argument = FALSE
knn4_bad <- st_knn(tx$geometry, 4, longlat = FALSE)
knn4_bad[[1]]


# Block Weights -----------------------------------------------------------

nb_block <- st_block_nb(sandiego$GEOID, sandiego$county)

which(sandiego$GEOID == "06073000201") %in% nb_block[["06073000100"]]



# Editing/connecting disconnected observations ----------------------------

# changing nb_block element 104 to its nearest neighbor
nb_knn1 <- st_knn(sandiego$geom)
nb_block[[104]] <- nb_knn1[[104]]

nb_block[104]


# Using the union of matrices ---------------------------------------------

wk1 <- st_knn(sandiego$geom, k = 1)
wk_rook <- st_contiguity(sandiego$geom, queen = FALSE)

nb_union(wk_rook, wk1)


# Visualizing weight set operations ---------------------------------------

mx <- read_sf("/Users/josiahparry/Downloads/book-master/data/mexico/mexicojoin.shp")

mx_queen <- st_contiguity(mx$geometry)
mx_knn4 <- st_knn(mx$geometry, k = 4)

mx_bw <- st_block_nb(mx$INEGI2)

mx_union <- nb_union(mx_queen, mx_bw)

ggplot() +
  geom_sf(data = mx,
          fill = "#2077B4",
          color = "black", lwd = 0.25) +
  geom_sf(data = st_as_edges(mx$geometry, mx_queen, wt = st_weights(mx_queen)),
          lwd = 0.5, lty = 2, color = "black") +
  geom_sf(data = st_point_on_surface(mx$geometry))


ggplot() +
  geom_sf(data = mx,
          fill = "#2077B4",
          color = "black", lwd = 0.25) +
  geom_sf(data = st_as_edges(mx$geometry, mx_knn4, wt = st_weights(mx_knn4)),
          lwd = 0.5, lty = 2, color = "black") +
  geom_sf(data = st_point_on_surface(mx$geometry))

ggplot() +
  geom_sf(data = mx,
          mapping = aes(fill = as.character(INEGI2)),
          color = "black", lwd = 0.25) +
  geom_sf(data = st_as_edges(mx$geometry, mx_bw, wt = st_weights(mx_bw)),
          lwd = 0.4, lty = 2, color = "black") +
  geom_sf(data = st_point_on_surface(mx$geometry))


ggplot() +
  geom_sf(data = mx,
          mapping = aes(fill = as.character(INEGI2)),
          color = "black", lwd = 0.25) +
  geom_sf(data = st_as_edges(mx$geometry, mx_union, wt = st_weights(mx_union)),
          lwd = 0.4, lty = 2, color = "black") +
  geom_sf(data = st_point_on_surface(mx$geometry))

pct_nonzero(mx_bw)
pct_nonzero(mx_queen)


# Use case: Boundary detection --------------------------------------------

ggplot(sandiego, aes(fill = median_hh_income)) +
  geom_sf(lwd = 0.05, color = "#ffffff50") +
  scale_fill_viridis_c()

ggplot(sandiego, aes(median_hh_income)) +
  geom_histogram(bins = 11)

sd_graph <- sandiego |>
  mutate(nb = st_contiguity(geom, queen = FALSE),
         wt = st_weights(nb, style = "B")) |>
  st_as_graph(nb, wt)

adjlist_income <- sd_graph |>
  sfnetworks::activate("edges") |>
  as_tibble() |>
  as_tibble() |>
  select(-geometry) |>
  left_join(
    transmute(as_tibble(sandiego),
           id = row_number(), median_hh_income),
    by = c("from" = "id")
  ) |>
  left_join(
    transmute(as_tibble(sandiego),
              id = row_number(), median_hh_income),
    by = c("to" = "id"),
    suffix = c("", "_nb")
  )

adjlist_income |>
  mutate(diff = median_hh_income - median_hh_income_nb)


diffs <- sandiego |>
  transmute(
    nb = st_contiguity(geom, FALSE),
    wt = st_weights(nb, "B"),
    nb_inc = find_xj(median_hh_income, nb),
    inc_diff_nb = mapply(
           function(.x, .xij) .x - .xij,
           median_hh_income, nb_inc
           ),
    non_nb = nb_setdiff(st_complete_nb(n()), nb),
    non_nb_inc = find_xj(median_hh_income, non_nb),
    inc_diff_non_nb = mapply(
      function(.x, .xij) .x - .xij,
      median_hh_income, non_nb_inc
    )
  )


nb_diffs <- diffs |>
  as_tibble() |>
  select(inc_diff_nb) |>
  tidyr::unnest(inc_diff_nb)

non_nb_diffs <- diffs |>
  as_tibble() |>
  select(inc_diff_non_nb) |>
  tidyr::unnest(inc_diff_non_nb)



ggplot() +
  geom_histogram(data = non_nb_diffs,
               mapping = aes(inc_diff_non_nb, ..density..)) +
  geom_histogram(data = nb_diffs,
                 mapping = aes(x = inc_diff_nb, y = ..density..),
                 fill = "orange", alpha = 0.5) +
  theme_light() +
  scale_x_continuous(labels = scales::dollar)
