
# Colocation Quotient -----------------------------------------------------
library(sf)
library(sfdep)
library(tidyverse)

crimes <- read_csv("/Users/josiahparry/Downloads/tmp7_f32p54.csv",
                   guess_max = 9999) |>
  rename_all(tolower) |>
  st_as_sf(coords = c("long", "lat"), crs = 4326)


top_crimes <- crimes |>
  filter(offense_description %in% c("THREATS TO DO BODILY HARM",
                                    "PROPERTY - LOST/ MISSING",
                                    "ASSAULT - SIMPLE",
                                    "INVESTIGATE PERSON",
                                    "VANDALISM"))

#
# top_crimes |>
#   as_tibble() |>
#   # including friday
#   mutate(is_weekend = day_of_week  %in% c("Friday", "Saturday", "Sunday")) |>
#   group_by(offense_description, is_weekend) |>
#   summarise(n = n()) |>
#   mutate(`%` = scales::percent(n / sum(n)))
#

x <- top_crimes |>
  bind_cols(
    data.frame(st_coordinates(top_crimes))
  ) |>
  filter(abs(X) > 0, abs(Y) > 0) |>
  mutate(is_weekend = day_of_week  %in% c("Friday", "Saturday", "Sunday")) |>
  select(offense_description, is_weekend) |>
  group_by(offense_description) |>
  slice_sample(n = 1000) |>
  ungroup() |>
  mutate(nb = st_dist_band(geometry),
         # grab only 5
         nb = lapply(nb, `[`, 1:5))

# lapply(unclass(x$nb), `[`, 1:5)

# variables ---------------------------------------------------------------

A <- as.factor(x$offense_description)
B <- as.factor(x$is_weekend)
nb <- x$nb
bij <- find_xj(B, nb)
n_prime_b <- length(B)
n_a = table(A)
a_vals <- names(n_a)
b_vals <- as.character(unique(B))

# Pairwise CLQ (A -> B) ---------------------------------------------------

# pairwise calculation
pairwise_colocation_calc <- function(A, B, nb) {
  A <- as.factor(A)
  B <- as.factor(B)
  bij <- find_xj(B, nb)
  n_a = table(A)
  a_vals <- names(n_a)
  b_vals <- as.character(unique(B))
  # denominator is dependent upon its an A -> A comparison or A ->
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


# Conditional permutation implementation
obs <- pairwise_colocation_calc(A = x$offense_description,
                         B = x$is_weekend,
                         nb)


reps <- replicate(199,
                  pairwise_colocation_calc(x$offense_description,
                                           x$is_weekend,
                                           cond_perm(x$nb)))

nsim = 199
a_vals <- names(reps[,1,1])
p_vals <- do.call(rbind, lapply(a_vals, function(.x) {
  g <- rowSums(obs[.x,] >= reps[.x,,]) / (nsim + 1)
  l <- rowSums(obs[.x,] <= reps[.x,,]) / (nsim + 1)
  pmin(g, l)
}))

colnames(p_vals) <- paste0("p_sim_", colnames(p_vals))

cbind(obs, p_vals)


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

profvis::profvis(
  pairwise_colocation_perm_impl(A, B, nb, 29)
)

pairwise_colocation_calc(A, B, nb)


# Global  CLQ (A -> A) ----------------------------------------------------

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

# TODO
# implement conditional permutation for p-value

reps <- replicate(99, global_colocation_calc(A[ind], cond_perm(nb[ind])))

obs <- global_colocation_calc(A, nb)

sum(reps >= obs) / (299 + 1)


# notes -------------------------------------------------------------------


mb <- bench::mark(

)

profvis::profvis({global_colocation_calc(A, nb)})




