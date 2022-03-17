# Multivariate
# pysal example
library(tidyverse)
library(spdep)
devtools::load_all()
x <- c(rep(0, 8), rep(1, 8))
z <- c(0, 1, 0, 1, 1, 1, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1)
y <- c(0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 0, 0, 1, 1)

f <- function(...) {
  dots <- rlang::list2(...)
  lhs_prod <- Reduce(`*`, dots)
}

g <- st_make_grid(cellsize = c(10, 10), offset = c(-180, -90),
             n = c(4, 4), crs = st_crs(4326), what = "polygons")

nb <- poly2nb(g, queen = F)
listw <- nb2listw(nb, style = "B")
wt <- listw$weights

x <- c(rep(0, 8), rep(1, 8))
z <- c(0, 1, 0, 1, 1, 1, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1)
y <- c(0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 0, 0, 1, 1)

dots <- list(x, y, z)


df <- data.frame(x,y,z)


queen <- rgeoda::rook_weights(st_as_sf(g))
rgeoda::local_multijoincount(queen, df)

lhs_prod <- Reduce(`*`, dots)

# find all nbs
all_nbs <- lapply(dots, find_xj, nb)

# get the neighbor values manually
xj  <- dots[[1]]
yj <- dots[[2]]
zj <- dots[[3]]


dots
# Get the join count for all of them
rhs_prod <- mapply(\(x,y,z, wt) sum(wt * x * y * z), xj, yj, zj,wt)

# Find instances where all surrounding focal and neighbor values == 1
all_nb_1 <- lapply(all_nbs, function(l) vapply(l, function(x) all(as.integer(x) == 1L), logical(1)))

# the join count is only used for locations where all neighbors are also == 1
# i guess? though i dont get that from the formula
# CLCi = Πmh=1xhi ∑ wijΠmh=1xhj
index <- rowSums(Reduce(cbind, all_nb_1)) == length(dots)

mvclc <- lhs_prod
mvclc[index] <- jc[index]

mvclc


# approach w/ arbitrary columns -------------------------------------------

dotnames <- paste0("var", 1:length(dots))
dots <- stats::setNames(dots, dotnames)


df <- list2DF(stats::setNames(all_nbs, dotnames))

all_nb_ones <- apply(df, 1, \(x) prod(unlist(x)))

jc <- mapply(\(x, wt) sum(x * wt), all_nb_ones, wt)

index <- which(all_nb_ones == 1)

res <- lhs_prod
res[index] <- jc[index]
res

