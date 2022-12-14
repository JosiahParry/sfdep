library(sf)
library(spdep)

bh <- st_read(system.file("etc/shapes/bhicv.shp",
                          package="spdep")[1], quiet=TRUE)

st_crs(bh) <- "+proj=longlat +ellps=WGS84"

dpad <- data.frame(scale(as.data.frame(bh)[,5:8]))

bh.nb <- poly2nb(bh)

nb <- st_contiguity(bh)

### calculating costs
lcosts <- nbcosts(bh.nb, dpad)

### making listw
nb.w <- nb2listw(bh.nb, lcosts, style="B")

### find a minimum spanning tree
mst.bh <- mstree(nb.w,5)



# Creating weighted igraph ------------------------------------------------

# can either cast as a matrix or set the weights manually
# can just unlist the weights
m <- sfdep::wt_as_matrix(bh.nb, lcosts)
g <- igraph::graph_from_adjacency_matrix(m, weighted = TRUE)

g2 <- igraph::graph_from_adj_list(bh.nb)
E(g2)$weight <- unlist(lcosts)

all.equal(E(g2)$weight, E(g)$weight)

all.equal(E(g)$weight,unlist(lcosts))


# creating minimum spanning tree ------------------------------------------

gmst <- igraph::mst(g)





# impl so far -------------------------------------------------------------

.skater <- function(x, nb, k, .method = "euclidean",
                    .ini = NULL, scale = TRUE, ...) {

  if (inherits(x, "numeric")) x <- list(x)
  m <- Reduce(cbind.data.frame, x)

  if (scale) m <- scale(m)

  costs <- spdep::nbcosts(nb, m, method = .method)

  listw <- spdep::nb2listw(nb, costs, style = "B")

  tree <- spdep::mstree(listw, ini = .ini)


  skater(tree[,1:2], m, ncuts = k - 1 , ...)

}
#
res <- .skater(list(bh$HLCI, bh$ELCI), nb, 5)

# The result is still super messy
plot(st_geometry(bh), col = res$groups)


groups <- res$groups

bh |>
  dplyr::mutate(group = as.factor(groups)) |>
  ggplot(aes(fill = group)) +
  geom_sf()

debugonce(.skater)

bh |>
  dplyr::mutate(y = data.frame(HLCI:ELCI_1)) |>
  dplyr::pull(y)
