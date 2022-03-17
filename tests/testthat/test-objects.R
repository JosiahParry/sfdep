# test_that("multiplication works", {
#   expect_equal(2 * 2, 4)
# })
#
# sc <- sf::st_geometry(uitk::suffolk_county)
#
# sc
#
#
# nb <- st_contiguity(sc)
#
# wt <- st_weights(nb)
#
#
# x <- uitk::acs_raw$bach
#
#
# # Missing values ----------------------------------------------------------
# library(spdep)
# library(dplyr)
# devtools::load_all()
#
# acs <- select(uitk::acs_raw,
#               fips = ct_id_10, med_house_income,
#               by_pub_trans, bach) %>%
#   mutate(fips = as.character(fips))
#
# acs_sf <- left_join(uitk::suffolk_county, acs, by = "fips")
#
# x <- acs_sf$med_house_income
#
# nb <- st_contiguity(st_geometry(acs_sf))
#
# wt <- st_weights(nb)
#
# listw <- recreate_listw(nb, wt)
#
# lag.listw(listw, x, NAOK = F)
