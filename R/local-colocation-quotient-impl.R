#' Local indicator of Colocation Quotient
#'
#' @description
#'
#' The local indicator of the colocation quotient (LCLQ) is a Local Indicator of Spatial Association (LISA) that evaluates if a given observation's subcategory in A is colocated with subcategories in B. Like the CLQ, the LCLQ provides insight into the asymmetric relationships between subcategories of A and B (where B can also equal A) but at the local level.
#'
#' The LCLQ is defined using Gaussian kernel weights and an adaptive bandwidth (see [`st_kernel_weights()`]). However, any type of weights list can be used. Kernel weights are used to introduce a decay into the calculation of the CLQ. This ensures that points nearer to the focal point have more influence than those that are more distant.
#'
#' @details
#'
#' The LCLQ is defined as \eqn{LCLQ_{A_i \to B} = \frac{N_{A_i \to B}}{N_B / (N - 1)}} where \eqn{N_{A_i \to B} = \sum_{j = 1(j \ne i)}^{N}(\frac{w_{ij}f_{ij}}{\sum_{j = 1(j \ne i)}^{N}w_{ij}})}. And the weights matrix, wij, uses adaptive bandwidth Gaussian kernel weights.
#'
#' LCLQ is only calculated for those subcategories which are present in the neighbor list. If a subcategory is not present, then the resultant LCLQ and simulated p-value will be `NA`.
#'
#' @references
#' {Fahui Wang, Yujie Hu, Shuai Wang & Xiaojuan Li (2017) Local Indicator of Colocation Quotient with a Statistical Significance Test: Examining Spatial Association of Crime and Facilities, The Professional Geographer, 69:1, 22-31, \doi{10.1080/00330124.2016.1157498}}
#'
#' @export
#' @inheritParams pairwise_colocation
#' @param wt a weights list. Recommended that it is a Gaussian kernel weights list using an adaptive bandwidth e.g. created by `st_kernel_weights(nb, geometry, "gaussian", addaptive = TRUE)` that does not include the self.
#' @returns a data frame with as many rows as observations in A and two times as many columns as unique values in B. Columns contain each unique value of B as well as the simulated p-value for each value of B.
#'
#' @examples
#' if (FALSE) {
#' A <- guerry$main_city
#' B <- guerry$region
#' geo <- sf::st_geometry(guerry)
#' nb <- st_knn(geo, 5)
#' wt <- st_kernel_weights(nb, geo, "gaussian", adaptive = TRUE)
#' res <- local_colocation(A, B, nb, wt, 49)
#' tail(res)
#' }

local_colocation <- function(A, B, nb, wt, nsim) {
  listw <- recreate_listw(nb, wt)
  local_colocation_impl(A, B, listw, nsim)
}

#' Calculate the local colocation quotient
#'
#' @keywords internal
#' @returns a matrix
local_colocation_calc <- function(A, B, listw) {
  nb <- listw[["neighbours"]]
  wt <- listw[["weights"]]
  # Check if kernel weights are used
  if (is.null(attr(wt, "kernel"))) {
    cli::cli_alert_warning("Guassian kernel weights are recommended.")
  }

  A <- as.factor(A)
  B <- as.factor(B)
  bij <- find_xj(B, nb)
  b_vals <- levels(B)
  # denominator is dependent upon its an A -> A comparison or A -> B
  # if A -> A N'B is subtracted by 1
  # pg 312 Leslie, T.F. and Kronenfeld, B.J. (2011),
  # https://doi.org/10.1111/j.1538-4632.2011.00821.x
  # Note that Wang et al. don't explicity state this same approach
  # but i will use it none the less
  denominator <- if (identical(A, B)) {
    (table(B) - 1) / (length(B) - 1)
  } else {
    table(B) / (length(B) - 1)
  }

  lclq <- Map(function(.xj, .wt) {
    if (any(is.na(c(.xj, .wt)))) return(NA)
    res <- stats::aggregate(. ~ .xj, data.frame(.xj, .wt),
                     sum,
                     na.rm = TRUE)
    res[["prop"]] <- res[[".wt"]] / sum(.wt, na.rm = TRUE)
    res <- res[["prop"]] / denominator[res[[".xj"]]]
    res[b_vals]
  }, bij, wt)


  lclq <- do.call(rbind, lclq)
  colnames(lclq) <- b_vals
  lclq
}

#' spdep implementation of local colocation quotient
#'
#' Internal implementation of the local CLQ that is compatible with spdep.
#'
#' @keywords internal
#' @returns a data frame where the number of rows is the same length as `A` and the number of columns is the same as unique values in `B`.
local_colocation_impl <- function(A, B, listw, nsim = 99) {
  obs <- local_colocation_calc(A, B, listw)

  reps <- replicate(nsim, local_colocation_calc(A, B, permute_listw(listw)),
                    simplify = "array")

  res_ps <- matrix(ncol = ncol(obs), nrow = nrow(obs))

  for (j in 1:ncol(obs)) {
    obs_j <- obs[,j]
    reps_j <- reps[,j,]
    l <- (rowSums(obs_j >=  reps_j, na.rm = TRUE) + 1) / (nsim + 1)
    g <- (rowSums(obs_j <=  reps_j, na.rm = TRUE) + 1)/ (nsim + 1)
    p <- pmin(l, g)
    p[is.na(obs_j)] <- NA
    res_ps[,j] <- p
  }

  colnames(res_ps) <- paste0("p_sim_", colnames(obs), sep = "")

  cbind(as.data.frame(obs),
        as.data.frame(res_ps))

}

# local_colocation_perm_impl(A, B, listw)
# local_colocation_calc(A, B, listw)

#
#
# xj <- bij[[1]]
# wtj <- wt[[1]]
#
# zero_index <-  which(lengths(bij) == 0)
# # replace zero_index with NA for nb
# for (i in zero_index) {
#   bij[[i]] <- NA
# }
#
# A <- guerry$main_city
# B <- guerry$region
# geo <- sf::st_geometry(guerry)
# nb <- st_knn(geo, 5)
# wt <- st_kernel_weights(nb, geo, "gaussian")
#
# bij <- find_xj(B, nb)
# listw <- recreate_listw(nb, wt)
# b_vals <- levels(B)

