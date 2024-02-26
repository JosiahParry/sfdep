test_that("local_g_perm", {
  df_fp <- system.file("extdata", "bos-ecometric.csv", package = "sfdep")
  geo_fp <- system.file("extdata", "bos-ecometric.geojson", package = "sfdep")

  # read in data
  df <- read.csv(
    df_fp, colClasses = c("character", "character", "integer", "double", "Date") # makese .region_id chr to match geo's chr .region_id
  )

  geo <- sf::st_read(geo_fp)
  res <- spacetime(df, geo, ".region_id", "year")
  expect_snapshot(res)
})
