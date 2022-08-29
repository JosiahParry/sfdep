library(sf)
library(dplyr)
library(sfdep)
library(sfnetworks)

geo <- sfdep::guerry_nb |>
  select(geometry) |>
  mutate(nb = st_complete_nb(n()),
         wt = st_nb_dists(geometry, nb))

listw <- recreate_listw(geo$nb, structure(geo$wt,  "B" = TRUE))

ms <- spdep::mstree(listw)

ms_df <- as.data.frame(ms) |>
  rlang::set_names(c("from", "to", "edge_length"))


geom <- sf::st_centroid(geo)[["geometry"]]
nodes <- geom

from <- geom[ms_df$from]
to <- geom[ms_df$to]

from <- as_tibble(st_coordinates(from)) |>
  mutate(id = row_number())
to <- as_tibble(st_coordinates(to)) |>
  mutate(id = row_number())



# need to find a way to arrange using base R
edges2 <- sfheaders::sf_linestring(
  arrange(bind_rows(from, to), id),
  x = "X",
  y = "Y",
  linestring_id = "id") |>
  bind_cols(ms_df)

plot(sfnetwork(nodes, edges2))




# less efficient way of doing this ----------------------------------------



new_linestring <- function(from, to) {
  sfheaders::sf_linestring(
    rbind(
      st_coordinates(from),
      st_coordinates(to)
    )
  )
}




purrr::map2(geom[ms_df$from],
            geom[ms_df$to],
            new_linestring) -> res


edges <- cbind(do.call("rbind", res), ms_df)


nodes <- geom

sfnetwork(nodes, edges) |>
  plot()



par(mar=c(0,0,0,0))
plot(st_geometry(geo), border=gray(.5))
plot(ms, sp::coordinates(as(geo, "Spatial")), col=2,
     cex.lab=.6, cex.circles=0.035, fg="blue", add=TRUE)




