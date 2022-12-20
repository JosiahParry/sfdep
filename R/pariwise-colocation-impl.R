#' Pairwise Colocation Quotient
#'
#' @description
#'
#' Calculate the pairwise colocation quotient (CLQ) for two categorical variables using conditional permutation.
#'
#' @details
#' ## Intuition
#'
#' The pairwise CLQ is used to test if there is a spatial directional association between subcategories of two vectors `A` and `B`. Compared to the cross-K metric and the join count statistic, the pairwise CLQ can elucidate the presence of an asymmetric relationship between subcategories of A and B. A and B can either be separate categorical vectors or the same categorical vector.
#'
#' "The null hypothesis for a CLQ-based analysis is 'given the clustering of the joint population, there is no spatial association between pairs of categorical subsets.'"
#'
#' ## Definition
#'
#' The pairwise colocation quotient is defined as "the ratio of observed to expected proportions of B among A's nearest neighbors. Formally this is given by
#' \eqn{CLQ_{A \to B} = \frac{{C_{A \to B} / N_A}}{N^{'}_{B} / (N - 1)}}" where \eqn{C_{A \to B} = \sum_{i = 1}^{N_A}\sum_{j = 1}^{v}\frac{B_{ij}(1,0)}{v}}.
#'
#' ## Inference
#'
#' Inference is done using conditional permutation as suggested by Anselin 1995 where a number of replicates are created. The observed values are compared to the replicates and a the simulated p-value is the proportion of cases where the observed is more extreme as compared to replicate. The simulated p-value returns the lower p-value of either tail.
#'
#' ## Interpretation
#'
#' Given that the CLQ is a ratio of the observed to expected, we interpret values larger than one to mean that there is more colocation than to be expected under the null hypothesis of no spatial association. When the value is smaller than 0, we interpret it to mean that there is less colocation than expected under the null.
#'
#' @param A a character or factor vector.
#' @param B a character or factor vector.
#' @param nb a neighbors list e.g. created by [`st_knn()`] or [`st_contiguity()`]
#' @param nsim default `99`. An integer representing how many simulations to run for calculating the simulated p-values.
#' @export
#' @examples
#' A <- guerry$main_city
#' B <- guerry$region
#' nb <- st_knn(sf::st_geometry(guerry), 5)
#' pairwise_colocation(B, A, nb)
#' pairwise_colocation(B, B, nb, 199)
#' @returns A matrix where the rownames are the unique values of A and the column names are the unique values of B and their simulated p-values in the form of `p_sim_{B}`.
pairwise_colocation <- function(A, B, nb, nsim = 99) {
  pairwise_colocation_perm_impl(A, B, nb, nsim)
}


#' Pairwise CLQ calculation
#'
#' Internal function to calculate the pairwise CQL.
#'
#' @inheritParams pairwise_colocation
#' @keywords internal
#' @returns a matrix where rownames are A values colnames are B values
pairwise_colocation_calc <- function(A, B, nb) {
  A <- as.factor(A)
  B <- as.factor(B)
  bij <- find_xj(B, nb)
  n_a = table(A)
  a_vals <- levels(A)
  b_vals <- levels(B)
  # denominator is dependent upon its an A -> A comparison or A -> B
  # if A -> A N'B is subtracted by 1
  # pg 312 Leslie, T.F. and Kronenfeld, B.J. (2011),
  # https://doi.org/10.1111/j.1538-4632.2011.00821.x
  denominator <- if (identical(A, B)) {
    (table(B) - 1) / (length(B) - 1)
  } else {
    table(B) / (length(B) - 1)
  }

  # calculate proportions for each obs
  bij_sums <- lapply(bij, function(.x) {
    res <- table(.x) / length(.x)
    if (length(res) == 0) res <- NA
    res
  })

  # split list based on values of A
  a_splits <- split(bij_sums, A)

  # calculate colocation quotient
  clq_ab <- lapply(a_vals, function(.x) {
    res <- colSums(do.call(rbind, a_splits[[.x]]), na.rm = TRUE)
    res <- res / n_a[.x]
    res[b_vals] / denominator[b_vals]
  })

  # return matrix where rownames are A values
  # colnames are B values
  res <- do.call(rbind, clq_ab)
  rownames(res) <- a_vals
  res
}

#' Pairwise CLQ conditional permutation implementation
#'
#' Internal implementation of the pairwise CQL using conditional permutation. Functionally the same as [`pairwise_colocation()`].
#'
#' @inheritParams pairwise_colocation
#' @keywords internal
#' @returns a matrix used by pairwise_colocation
pairwise_colocation_perm_impl <- function(A, B, nb, nsim = 199) {
  obs <- pairwise_colocation_calc(A, B, nb)
  reps <- replicate(nsim,
                    pairwise_colocation_calc(
                      A, B, cond_permute_nb(nb)
                      )
                    )
  a_vals <- names(reps[,1,1])
  p_vals <- do.call(rbind, lapply(a_vals, function(.x) {
    g <- rowSums(obs[.x,] >= reps[.x,,]) / (nsim + 1)
    l <- rowSums(obs[.x,] <= reps[.x,,]) / (nsim + 1)
    pmin(g, l)
  }))

  colnames(p_vals) <- paste0("p_sim_", colnames(p_vals))

  cbind(obs, p_vals)
}
