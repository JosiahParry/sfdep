
library(testthat)

geometry <- sf::st_geometry(guerry)

nbd <- st_nb_delaunay(geometry)
nbg <- st_nb_gabriel(geometry)
nbr <- st_nb_relative(geometry)


test_that("neighbors are of appropriate length", {
  expect_length(nbd, 85)
  expect_length(nbg, 85)
  expect_length(nbr, 85)
})

test_that("gabriel nb is a subgraph of delaunary", {
  expect_equal(
    nb_intersect(nbd, nbg),
    nbg,
    ignore_attr = TRUE
  )
})

test_that("relative nb is a subgraph of delaunary", {
  expect_equal(
    nb_intersect(nbd, nbr),
    nbr,
    ignore_attr = TRUE
  )
})

test_that("graph neighbors don't work with sf objects", {
  expect_error(st_nb_delaunay(guerry))
  expect_error(st_nb_gabriel(guerry))
  expect_error(st_nb_relative(guerry))
})

test_that("message is emitted when providing polygons", {
  expect_message(st_nb_delaunay(geometry))
  expect_message(st_nb_gabriel(geometry))
  expect_message(st_nb_relative(geometry))
})

test_that("error on using lines", {
  g2 <- sfnetworks::roxel[["geometry"]]

  # errors emitted thanks to spdep
  expect_error(st_nb_delaunay(g2))
  expect_error(st_nb_gabriel(g2))
  expect_error(st_nb_relative(g2))

})
