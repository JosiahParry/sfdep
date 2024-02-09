test_that("local_g_perm", {
  set.seed(19870630)
  x <- guerry$crime_pers
  nb <- st_contiguity(guerry)
  wt <- st_weights(nb)

  res <- local_g_perm(x, nb, wt)
  expect_snapshot(res)
})
