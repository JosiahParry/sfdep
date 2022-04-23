# .method = "manhattan"
# scale = TRUE
# .p = 2
# k = 10
# x <- list(guerry$crime_parents, guerry$suicides)

st_knn_attr <- function(x, k = 5, .method = "euclidian", scale = TRUE, .p = 2) {
  if (!requireNamespace("dbscan")) rlang::abort("Package `dbscan` must installed.")
  m <- Reduce(cbind.data.frame, x)
  if (scale) m <- scale(m)
  # kNN in attribute space
  d <- stats::dist(m, .method, p = .p)
  kd <- dbscan::kNN(d, k = k)
  # find neighbor ids
  knn_nb <- dbscan::adjacencylist(kd)
  attr(knn_nb, "knn_attr") <- kd
  class_modify(knn_nb, "nb")
}

st_attr_dists <- function(nb) {

  kd <- attr(knn_nb, "knn_attr")

  if (is.null(kd)) cli::cli_abort("Missing `knn_attr` attribute. Was `nb` created using `st_knn_attr()`?")

  split(kd$dist, f = 1:nrow(kd$dist))
}




#
#
# attr(knn_nb, "attr_knn")$dist
