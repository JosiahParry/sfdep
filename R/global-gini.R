#' Spatial Gini Index
#'
#' Calculates the spatial Gini index for a given numeric vector and neighbor list.
#' Based on the formula provided Rey and Smith (2013).
#'
#' The Gini index is a global measure of inequality based on the Lorenz curve. It ranges between 0 and 1 where 0 is perfect equality and 1 is perfect inequality.
#'
#' The spatial Gini index decomposes the Gini coefficient based on spatial neighbors.
#'
#' @references
#' \doi{http://dx.doi.org/10.1007/s12076-012-0086-z}
#'
#' @param x a numeric vector without missing values
#' @param nb a neighbor list, for example created with [`st_contiguity()`]
#'
#' @examples
#' nb <- st_contiguity(guerry)
#' x <- guerry$wealth
#' spatial_gini(x, nb)
#'
#' @returns
#' A data frame with columns:
#'
#' - `G`: the Gini index
#' - `NBG`: the neighbor composition of the Gini coefficient
#' - `NG`: the non-neighbor composition of the Gini coefficient
#' - `SG`: the Spatial Gini which is equal to \eqn{NG * \frac{1}{G}}
#'
#' @export
spatial_gini <- function(x, nb) {

  # cast as a matrix
  w <- nb_as_matrix(nb)

  # constants
  xbar <- mean(x, na.rm = TRUE)
  n <- length(x)

  # instantiate vectors to fill
  lhs_num <- numeric(n)
  rhs_num <- numeric(n)
  xdiffs <- vector("list", n)
  for (i in 1:n) {
    xi <- x[i]
    wij <- w[i,]
    xdiff <- abs(xi - x)
    xdiffs[[i]] <- xdiff
    lhs_num[i] <- sum(xdiff * wij)
    rhs_num[i] <- sum(xdiff * (1 - wij))
  }

  # calculate denominator
  denom <- 2 * (n^2) * xbar

  # result
  Gnb <- (sum(lhs_num) / denom)
  NG <- (sum(rhs_num) / denom)
  G <- Gnb + NG
  # SG can be interpreted as the share of overall inequality
  # that is associated with non-neighbor pair of locations.
  SG <- NG * (1 / G)

  data.frame(G, NG, NBG = Gnb, SG)
}

# tracts <- tidycensus::get_acs(
#   state = "VA",
#   # county = "Orange",
#   geography = "tract",
#   variables = "B19013_001",
#   geometry = TRUE,
#   year = 2020
# )
#
# library(sfdep)
# library(dplyr)
#
# tract_clean <- tracts |>
#   filter(!sf::st_is_empty(geometry),
#          !is.na(estimate)) |>
#   mutate(
#     geometry = sf::st_make_valid(geometry),
#     nb = st_contiguity(geometry)
#   )
#
#
# x <- tract_clean$estimate
# nb <- tract_clean$nb

spatial_gini <- function(x, nb) {

  n <- length(x)
  m <- matrix(rep(x, n), nrow = n, byrow = T)
  xdiff <- abs(x - m)

  spatial_gini_calc(x, nb, xdiff)
}

spatial_gini_calc <- function(x, nb, xdiff) {
  # cast as a matrix
  w <- nb_as_matrix(nb)
  # cast nbs as matrix
  xj <- find_xj(x, nb)
  listw <- recreate_listw(nb, xj)
  xij <- spdep::listw2mat(listw)

  # constants
  xbar <- mean(x, na.rm = TRUE)
  n <- length(x)
  lhs_sum <- rowSums(xdiff * w)
  rhs_sum <- rowSums(xdiff * (1 - w))

  # calculate denominator
  denom <- 2 * (n^2) * xbar

  # result
  Gnb <- (sum(lhs_sum) / denom)
  NG <- (sum(rhs_sum) / denom)
  G <- Gnb + NG
  # SG can be interpreted as the share of overall inequality
  # that is associated with non-neighbor pair of locations.
  SG <- NG * (1 / G)

  data.frame(G, NG, NBG = Gnb, SG)
}

spatial_gini_perm_impl <- function(x, nb, xdiff, nsim) {

  obs <- spatial_gini_calc(x, nb, xdiff)
  reps <- replicate(
    nsim,
    spatial_gini_calc(x, cond_permute_nb(nb), xdiff),
    simplify = FALSE
  )

  r <- do.call("rbind", reps)
  p_value <- pmin((sum(obs$SG >=  r$SG) + 1) / (nsim + 1),
                  (sum(obs$SG <=  r$SG) + 1) / (nsim + 1))

  cbind(obs, p_value)
}



spatial_gini_perm <- function(x, nb, nsim = 49) {

  n <- length(x)
  m <- matrix(rep(x, n), nrow = n, byrow = TRUE)
  xdiff <- abs(x - m)
  spatial_gini_perm_impl(x, nb, xdiff, nsim)
}


