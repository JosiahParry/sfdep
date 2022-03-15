# Types of p-values
# https://github.com/pysal/esda/issues/199

# Folded p values "fold" test values by centering them on either median or mean
# and taking the absolute value. That is where "folded" come from


# example with guerry based on pysal
x <- as.integer(guerry$donations > 10997)
nb <- st_contiguity(guerry)
wt <- st_weights(nb, style = "B")
xj <- find_xj(x, nb) # find xj values
obs <- mapply(jc_uni_calc, x, xj, wt) # observed join count

x_index <- which(x == 1L)
xj_index <- which(unlist(lapply(xj, function(x) any(x == 1L))) == TRUE)
index <- intersect(xj_index, x_index)

reps <- replicate(nsim, jc_uni_perm_impl(x, recreate_listw(nb, wt), index))

g <- (rowSums(obs[index] >= reps) + 1)/ (nsim + 1)
l <- (rowSums(obs[index] <= reps) + 1) / (nsim + 1)

p_sim <- pmin(l, (1 - l))

