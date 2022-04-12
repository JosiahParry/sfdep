nb <- guerry_nb$nb

wts <- nb2listw(nb, style = "C")[["weights"]]

# C (globally standardized weights):
# n / sum(card(i)) eg
library(spdep)
n <- length(nb)
total_cards <- sum(card(nb))
# this is then assigned to every weight
c_wt <- n / total_cards



# U is C / n
wts <- nb2listw(nb, style = "U")[["weights"]]
c_wt / total_cards

# S
wts <- nb2listw(nb, style = "S")[["weights"]]
wt

wts

s <-1 / sqrt(card(nb))

eff.n <- n
cardnb <- card(nb)
glist <- vector(mode = "list", length = n)
for (i in 1:n) if (cardnb[i] > 0) {
  glist[[i]] <- rep(1, cardnb[i])
  mode(glist[[i]]) <- "numeric"
}

q <- sqrt(unlist(lapply(glist2, sum)))

glist2 <- lapply(glist, function(x) x^2)
q <- sqrt(unlist(lapply(glist2, sum)))
for (i in 1:n) {
  if (cardnb[i] > 0) {
    if (q[i] > 0)
      glist[[i]] <- (1/q[i]) * glist[[i]]
    else glist[[i]] <- 0 * glist[[i]]
  }
}
Q <- sum(unlist(glist))
if (is.na(Q) || !(Q > 0))
  stop(paste("Failure in sum of intermediate weights:",
             Q))

vlist <- vector(mode = "list", length = n)
for (i in 1:n) {
  if (cardnb[i] > 0)
    vlist[[i]] <- (eff.n/Q) * glist[[i]]
}


(n / Q) * rep(1, 7)
