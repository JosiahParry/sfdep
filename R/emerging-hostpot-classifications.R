# Hotspot clssification ---------------------------------------------------
# https://pro.arcgis.com/en/pro-app/2.8/tool-reference/space-time-pattern-mining/learnmoreemerging.htm#GUID-09587AFC-F5EC-4AEB-BE8F-0E0A26AB9230
# all definitions appended at end

new_hotspot <- function(gs, sigs, n, ...) {
  (sum(sigs) == 1) && sigs[n] && gs[n] > 0
}

consecutive_hotspot <- function(gs, sigs, n, ...) {
  if (!(sigs[n] && gs[n] > 0)) return(FALSE)

  n_final_run <- which(diff(cumsum(rev(sigs))) == 0)[1]
  run_index <- seq(n-n_final_run + 1, n, by = 1)

  all(!sigs[-run_index]) &&
    all(sigs[run_index]) &&
    all(!((sigs[-run_index]) & (gs[-run_index] > 0))) &&
    all(!sigs[-run_index]) &&
    (sum(sigs & gs > 0) / n <= 0.9)
}

intensifying_hotspot <- function(gs, sigs, tau, tau_p, threshold, ...) {
  (sum(sigs) / n  >= .9) && (sigs[n]) && (tau > 0) && (tau_p <= threshold)
}

persistent_hotspot <- function(gs, sigs, tau_p, threshold, ...) {
  (sum(sigs & gs > 0) / n  >= .9) && (tau_p > threshold)
}

diminishing_hotspot <- function(gs, sigs, tau, tau_p, threshold, ...) {
  (sum(sigs & gs > 0) / n  >= .9) &&
    (sigs[n]) && (tau < 0) &&
    (tau_p <= threshold)
}

sporadic_hotspot <- function(gs, sigs, n, ...) {
  (sum(gs > 0 & sigs) / n <= 0.9) && !any(gs < 0 & sigs)
}

sporadic_coldspot <- function(gs, sigs, n, ...) {
  (sum(gs < 0 & sigs) / n <= 0.9) && !any(gs > 0 & sigs)
}


oscilating_hotspot <- function(gs, sigs, n, ...) {
  (sum((gs > 0) & sigs) / n <= 0.9) & (gs[n] > 0 & sigs[n]) & any(gs < 0 & sigs)
}

historical_hotspot <- function(gs, sigs, n, ...) {
  (sum(sigs & gs > 0) / n >= 0.9) & (gs[n] < 0)
}

new_coldspot <- function(gs, sigs, n, ...) {
  (sum(sigs) == 1) && sigs[n] && gs[n] < 0
}

consecutive_coldspot <- function(gs, sigs, n, ...) {
  if (!(sigs[n] && gs[n] < 0)) return(FALSE)

  n_final_run <- which(diff(cumsum(rev(sigs))) == 0)[1]
  run_index <- seq(n-n_final_run + 1, n, by = 1)

  all(!sigs[-run_index]) &&
    all(sigs[run_index]) &&
    all(!((sigs[-run_index]) & (gs[-run_index] > 0))) &&
    all(!sigs[-run_index]) &&
    (sum(sigs & gs > 0) / n <= 0.9)
}

intensifying_coldspot <- function(gs, sigs, n, tau, tau_p, threshold, ...){
  (sum(sigs & gs < 0) / n  >= .9) &&
    (sigs[n]) &&
    (tau < 0) &&
    (tau_p <= threshold)
}

persistent_coldspot <- function(gs, sigs, n, tau_p, threshold, ...) {
  (sum(sigs & gs < 0) / n  >= .9) && (tau_p > threshold)
}

diminishing_coldspot <- function(gs, sigs, n, tau, tau_p, threshold, ...){
  (sum(sigs & gs < 0) / n  >= .9) && (sigs[n]) && (tau > 0) && (tau_p <= threshold)
}

oscilating_coldspot <- function(gs, sigs, n, ...) {
  (sum((gs < 0) & sigs) / n < 0.9) && (gs[n] < 0 & sigs[n]) && (any(gs > 0 & sigs))
}

historical_coldspot <- function(gs, sigs, n, ...) {
  (gs[n] > 0) && (sum(sigs & gs < 0) / n >= 0.9)
}

fxs <- list(
  "new_hotspot" = new_hotspot,
  "new_coldspot" = new_coldspot,
  "consecutive_hotspot" = consecutive_hotspot,
  "consecutive_coldspot" = consecutive_coldspot,
  "intensifying_hotspot" = intensifying_hotspot,
  "intensifying_coldspot" = intensifying_coldspot,
  "persistent_hotspot" = persistent_hotspot,
  "persistent_coldspot" = persistent_coldspot,
  "diminishing_hotspot" = diminishing_hotspot,
  "diminishing_coldspot" = diminishing_coldspot,
  "sporadic_hotspot" = sporadic_hotspot,
  "sporadic_coldspot" = sporadic_coldspot,
  "oscilating_hotspot" = oscilating_hotspot,
  "oscilating_coldspot" = oscilating_coldspot,
  "historical_hotspot" = historical_hotspot,
  "historical_coldspot" = historical_coldspot)

.x <- splits[[1]]
threshold = 0.05

test_res <- Kendall::MannKendall(gs)
tau = test_res[["tau"]]
tau_p = as.numeric(test_res[["sl"]])

sigs = .x[["p_sim"]] <= threshold
gs = .x[["gi_star"]]
n = length(gs)


classify_hotspot <- function(.x, mk_test, threshold) {
  res <- lapply(fxs, function(.x, ...) .x(...),
                gs = gs,
                sigs = sigs,
                n = n,
                tau = tau,
                tau_p = tau_p,
                threshold = threshold
  )
  names(res[unlist(res)])
}

classify_hotspot(.x)
res <- lapply(fxs, function(.x, ...) .x(...),
       gs = gs,
       sigs = sigs,
       n = n,
       tau = tau,
       tau_p = tau_p,
       threshold = threshold
       )


names(res[unlist(res)])

# "New Hot Spot
#
# A location that is a statistically significant hot spot for the final time step and has never been a statistically significant hot spot before.
#
# Consecutive Hot Spot
# Consecutive Hot Spot
#
# A location with a single uninterrupted run of statistically significant hot spot bins in the final time-step intervals. The location has never been a statistically significant hot spot prior to the final hot spot run and less than ninety percent of all bins are statistically significant hot spots.
#
# Intensifying Hot Spot
# Intensifying Hot Spot
#
# A location that has been a statistically significant hot spot for ninety percent of the time-step intervals, including the final time step. In addition, the intensity of clustering of high counts in each time step is increasing overall and that increase is statistically significant.
#
# Persistent Hot Spot
# Persistent Hot Spot
#
# A location that has been a statistically significant hot spot for ninety percent of the time-step intervals with no discernible trend indicating an increase or decrease in the intensity of clustering over time.
#
# Diminishing Hot Spot
# Diminishing Hot Spot
#
# A location that has been a statistically significant hot spot for ninety percent of the time-step intervals, including the final time step. In addition, the intensity of clustering in each time step is decreasing overall and that decrease is statistically significant.
#
# Sporadic Hot Spot
# Sporadic Hot Spot
#
# A location that is an on-again then off-again hot spot. Less than ninety percent of the time-step intervals have been statistically significant hot spots and none of the time-step intervals have been statistically significant cold spots.
#
# Oscillating Hot Spot
# Oscillating Hot Spot
#
# A statistically significant hot spot for the final time-step interval that has a history of also being a statistically significant cold spot during a prior time step. Less than ninety percent of the time-step intervals have been statistically significant hot spots.
#
# Historical Hot Spot
# Historical Hot Spot
#
# The most recent time period is not hot, but at least ninety percent of the time-step intervals have been statistically significant hot spots.
#
# New Cold Spot
# New Cold Spot
#
# A location that is a statistically significant cold spot for the final time step and has never been a statistically significant cold spot before.
#
# Consecutive Cold Spot
# Consecutive Cold Spot
#
# A location with a single uninterrupted run of statistically significant cold spot bins in the final time-step intervals. The location has never been a statistically significant cold spot prior to the final cold spot run and less than ninety percent of all bins are statistically significant cold spots.
#
# Intensifying Cold Spot
# Intensifying Cold Spot
#
# A location that has been a statistically significant cold spot for ninety percent of the time-step intervals, including the final time step. In addition, the intensity of clustering of low counts in each time step is increasing overall and that increase is statistically significant.
#
# Persistent Cold Spot
# Persistent Cold Spot
#
# A location that has been a statistically significant cold spot for ninety percent of the time-step intervals with no discernible trend, indicating an increase or decrease in the intensity of clustering of counts over time.
#
# Diminishing Cold Spot
# Diminishing Cold Spot
#
# A location that has been a statistically significant cold spot for ninety percent of the time-step intervals, including the final time step. In addition, the intensity of clustering of low counts in each time step is decreasing overall and that decrease is statistically significant.
#
# Sporadic Cold Spot
# Sporadic Cold Spot
#
# A location that is an on-again then off-again cold spot. Less than ninety percent of the time-step intervals have been statistically significant cold spots and none of the time-step intervals have been statistically significant hot spots.
#
# Oscillating Cold Spot
# Oscillating Cold Spot
#
# A statistically significant cold spot for the final time-step interval that has a history of also being a statistically significant hot spot during a prior time step. Less than ninety percent of the time-step intervals have been statistically significant cold spots.
#
# Historical Cold Spot
# Historical Cold Spot
#
# The most recent time period is not cold, but at least ninety percent of the time-step intervals have been statistically significant cold spots."
