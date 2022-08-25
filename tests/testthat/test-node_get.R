library(sfdep)
library(dplyr)
library(sfnetworks)

net <- as_sfnetwork(roxel, directed = FALSE)

test_that("identical adjacency lists", {

  net_nb <- mutate(net, nb = node_get_nbs())
  ig_adj_list <- igraph::get.adjlist(net)

  expect_equal(
    as_tibble(net_nb)$nb,
    lapply(ig_adj_list, as.integer)
  )

})

