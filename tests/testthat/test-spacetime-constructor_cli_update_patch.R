test_that("local_g_perm", {
  df_fp <- system.file("extdata", "bos-ecometric.csv", package = "sfdep")
  geo_fp <- system.file("extdata", "bos-ecometric.geojson", package = "sfdep")

  # read in data
  df <- read.csv(
    df_fp, colClasses = c("numeric", "character", "integer", "double", "Date")
  )

  geo <- sf::st_read(geo_fp)
  expect_error(spacetime(df, geo, ".region_id", "year"))
})
