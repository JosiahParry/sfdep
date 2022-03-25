## code to prepare `DATASET` dataset goes here

guerry <- sf::st_as_sf(Guerry::gfrance85) |>
  janitor::clean_names() |>
  sf::st_set_crs(27572) |>
  #sf::st_transform(crs = "EPSG:4326") %>%
  tibble::as_tibble() |>
  sf::st_as_sf()

guerry_nb <- guerry |>
  dplyr::mutate(nb = st_contiguity(geometry),
                wt = st_weights(nb))


usethis::use_data(guerry, guerry_nb, overwrite = TRUE)
