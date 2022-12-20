# to test -----------------------------------------------------------------

# attributes of nb class
# attributes of wt columns such as style
# check lengths on nbs and weights
# check edge lists match weight list values


# tests -------------------------------------------------------------------

test_that("nbs is spdep compliant", {

  skip_on_cran()

  library(sfdep)
  library(dplyr)
  library(tidygraph)
  library(sfnetworks)

  # create object with edge list and neighbor list
  net <- as_sfnetwork(roxel, directed = FALSE) |>
    tidygraph::activate("edges") |>
    mutate(len = edge_length()) |>
    tidygraph::activate("nodes") |>
    mutate(
      nb = node_get_nbs(),
      elist = node_get_edge_list(),
      wt = node_get_edge_col(elist, "len")
    )


  node_df <- as_tibble(net)

  edge_df <- net %E>%
    as_tibble()

  nb <- as_tibble(net)[["nb"]]

  # test that nb does not include self
  expect_equal(attr(nb, "self.included"), FALSE)

  # that nb is a nb class & list class
  expect_s3_class(nb, c("nb", "list"))

  # expect identical adjacency lists
  ig_adj_list <- igraph::get.adjlist(net)

  expect_equal(
    nb,
    lapply(ig_adj_list, as.integer),
    ignore_attr = TRUE
  )

  # edge list column is identical to igraph
  n2 <- mutate(net, edge_list = node_get_edge_list())
  el <- as_tibble(n2)[["edge_list"]]

  elig <- lapply(igraph::get.adjedgelist(net), as.integer)

  expect_equal(el, elig)

  # attr edge col has same lengths as nb
  expect_equal(
    lengths(node_df[["nb"]]),
    lengths(node_df[["wt"]])
  )


  # "edge list contains correct neighbor nodes
  nb <- node_df[["nb"]]
  elist <- node_df[["elist"]]

  correct_indexes <- logical(length(nb))

  for (i in seq_along(elist)) {
    tmp <- dplyr::slice(as_tibble(edge_df), elist[[i]])
    res <- all(nb[[i]] %in% c(tmp[["from"]], tmp[["to"]]))
    correct_indexes[i] <- res
  }

  expect_true(all(correct_indexes))

  # "edge_get_col retrieves correct indexes
  # check that edge list correctly matches neighbors
  # the ith observation should all be from i to others
  elist <- node_df[["elist"]]

  # test that adjacency matrix matches edge list
  # each element in the edge list should include `i` either in
  # from or to as many times as there are neighbors
  adj_vec <- integer(length(elist))
  for (i in seq_along(elist)) {
    tmp <- dplyr::slice(as_tibble(edge_df), elist[[i]])
    res <- sum(c(tmp[["from"]], tmp[["to"]]) == i)
    adj_vec[i] <- res
  }

  expect_equal(lengths(elist), adj_vec)
})


