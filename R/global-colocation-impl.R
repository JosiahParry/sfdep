#' Global Colocation Quotient
#'
#' @description
#'
#' Calculate the Global Colocation Quotient (CLQ) for a categorical variable using simulation based significance testing.
#'
#' @details
#'
#' ## Definition
#'
#' The CLQ is defined as \eqn{CLQ_{Global} = \frac{\sum_{A \in X} C_{A \to A}}{\sum_{A \in X} N_A ({\frac{N_A - 1}{N-1})}}}. The numerator identifies the observed proportion of same-category neighbors while the denominator contains the _expected_ proportion of same-category neighbors under the assumption of no spatial association. Thus the CLQ is just a ratio of observed to expected.
#'
#' ## Inference
#'
#' Inference is done using conditional permutation as suggested by Anselin 1995 where a number of replicates are created. The observed values are compared to the replicates and a the simulated p-value is the proportion of cases where the observed is more extreme as compared to replicate. The simulated p-value returns the lower p-value of either tail.
#'
#' ## Interpretation
#'
#' Given that the CLQ is a ratio of the observed to expected, we interpret values larger than one to mean that there is more colocation than to be expected under the null hypothesis of no spatial association. When the value is smaller than 0, we interpret it to mean that there is less colocation than expected under the null.
#'
#' @export
#' @returns
#' A list of two elements `CLQ` and `p_sim` containing the observed colocation quotient and the simulated p-value respectively.
#' @examples
#' A <- guerry$main_city
#' nb <- st_contiguity(sf::st_geometry(guerry))
#' global_colocation(A, nb, 49)
#' @references
#' {Leslie, T.F. and Kronenfeld, B.J. (2011), The Colocation Quotient: A New Measure of Spatial Association Between Categorical Subsets of Points. Geographical Analysis, 43: 306-326. \doi{https://doi.org/10.1111/j.1538-4632.2011.00821.x}}
#' @inheritParams pairwise_colocation
global_colocation <- function(A, nb, nsim = 99) {
  global_colocation_perm_impl(A, nb, nsim)
}


#' Calculate the Global Colocation Quotient
#'
#' Given a categorical variable `A`, calculate the global colocation quotient (CLQ).
#'
#' @keywords internal
global_colocation_calc <- function(A, nb) {
  A <- as.factor(A)
  a_vals <- levels(A)
  aij <- find_xj(A, nb)

  sum_ij <- lapply(aij, function(.x) {
    res <- table(.x) / length(.x)
    if (length(res) == 0) res <- NA
    res
  })

  a_splits <- split(sum_ij, A)

  numerator <- sum(unlist(lapply(a_vals, function(.x) {
    colSums(do.call(rbind, a_splits[[.x]]), na.rm = TRUE)
    tmp <- colSums(do.call(rbind, a_splits[[.x]]), na.rm = TRUE)
    tmp[.x]
  })))

  denominator <- sum(((table(A) - 1) / (length(A) - 1) * table(A)))

  numerator / denominator
}

#' Global Colocation Quotient Conditional Permutation Implementation
#'
#' This function will repeat calculation of the global colocation quotient (CLQ)
#' a specified number of times and return a simulated p-value.
#'
#' @keywords internal
global_colocation_perm_impl <- function(A, nb, nsim = 99) {
  obs <- global_colocation_calc(A, nb)
  reps <- replicate(nsim, global_colocation_calc(A, cond_permute_nb(nb)))
  l <- (sum((obs >= reps)) + 1) / (nsim + 1)
  g <- (sum((obs <= reps)) + 1) / (nsim + 1)

  list(CLQ = obs,
       p_sim = min(g, l))

}
