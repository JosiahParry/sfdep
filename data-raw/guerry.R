## code to prepare `DATASET` dataset goes here
library(sfdep)

guerry <- sf::st_as_sf(Guerry::gfrance85) |>
  janitor::clean_names() |>
  sf::st_set_crs(27572) |>
  tibble::as_tibble() |>
  sf::st_as_sf()

guerry_nb <- guerry |>
  dplyr::mutate(nb = st_contiguity(geometry),
                wt = st_weights(nb))

save(guerry, file = "data/guerry.rda", ascii = TRUE, compress = "xz")
save(guerry_nb, file = "data/guerry_nb.rda", ascii = TRUE, compress = "xz")

# load("data/guerry.rda")
# load("data/guerry_nb.rda")
