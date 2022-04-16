#' Calculate Kernel Weights
#'
#' Create a weights list using a kernel function.
#'
#' @details
#'
#' By default `st_kernel_weight()` utilizes a critical threshold of the maximum neighbor distance using `critical_threshold()`. If desired, the critical threshold can be specified manually. The `threshold` will be passed to the underlying kernel.
#'
#'
#' @param nb an object of class `nb` e.g. created by [`st_contiguity()`] or [`st_knn()`].
#' @param geometry the geometry an sf object.
#' @param kernel One of "uniform", "gaussian",  "triangular", "epanechnikov", or "quartic". See [kernels] for more.
#' @param threshold a scaling threshold to be used in calculating
#' @param adaptive default `FALSE`. If `TRUE` uses the maximum neighbor distance for each region as the threshold. Suppresses the `threshold` argument.
#' @param self_kernel default `FALSE`. If `TRUE` applies the kernel function to the observed region.
#' @importFrom spdep nbdists dnearneigh include.self
#' @family weights
#' @export
#' @returns a list where each element is a numeric vector.
#' @examples
#' geometry <- sf::st_geometry(guerry)
#' nb <- st_contiguity(geometry)
#' nb <- include_self(nb)
#' res <- st_kernel_weights(nb, geometry)
#' head(res, 3)
st_kernel_weights <- function(nb, geometry, kernel = "uniform",
                              threshold = critical_threshold(geometry),
                              adaptive = FALSE,
                              self_kernel = FALSE
                              ) {

  # if self isnt included elicit warning
  self_included <- !is.null(attr(nb, "self.included"))
  if (!self_included) {
    cli::cli_warn("It is recommended to include the ith observation.
                    Consider `nb = include_self(nb)`.")
  }

  # verify kernels
  match.arg(kernel, names(kernels))

  # retrieve points
  pnts <- check_polygon(geometry)

  # calculate distances
  dists <- spdep::nbdists(nb, pnts)

  # check if adaptive, if T, calculate thresholds
  if (adaptive) {
    adaptive_thresholds <- unlist(lapply(dists, max))
    # kernel with adaptive thresholds
    res <- Map(kernels[[kernel]], dists, adaptive_thresholds)
  } else {
    # apply kernel
    res <- lapply(dists, kernels[[kernel]], threshold)
  }

  # set self to 1 if required
  if (!self_kernel && self_included) {
    n <- length(nb)
    for(i in 1:n) {
      res[[i]][which(nb[[i]] == i)] <- 1
    }
  }

  res

}


