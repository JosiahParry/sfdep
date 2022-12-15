# Hotspot clssification ---------------------------------------------------
# https://pro.arcgis.com/en/pro-app/2.8/tool-reference/space-time-pattern-mining/learnmoreemerging.htm#GUID-09587AFC-F5EC-4AEB-BE8F-0E0A26AB9230

new_hotspot <- function(gs, sigs, n, ...) {
  (sum(sigs) == 1) && sigs[n] && gs[n] > 0
}

consecutive_hotspot <- function(gs, sigs, n, ...) {
  if (!(sigs[n] && gs[n] > 0)) return(FALSE)

  n_final_run <- which(diff(cumsum(c(rev(sigs), FALSE))) == 0)[1]
  run_index <- seq(n - n_final_run + 1, n, by = 1)

  all(!sigs[-run_index]) &&
    all(sigs[run_index]) &&
    all(!((sigs[-run_index]) & (gs[-run_index] > 0))) &&
    all(!sigs[-run_index]) &&
    (sum(sigs & gs > 0) / n < 0.9)
}

intensifying_hotspot <- function(gs, sigs, n, tau, tau_p, threshold, ...) {
  (sum(sigs) / n  >= .9) && (sigs[n]) && (tau > 0) && (tau_p < threshold)
}

persistent_hotspot <- function(gs, sigs, tau_p, threshold, n, ...) {
  (sum(sigs & gs > 0) / n  >= .9) && (tau_p > threshold)
}

diminishing_hotspot <- function(gs, sigs, tau, tau_p, threshold, n, ...) {
  (sum(sigs & gs > 0) / n  >= .9) &&
    (sigs[n]) && (tau < 0) &&
    (tau_p <= threshold)
}

sporadic_hotspot <- function(gs, sigs, n, ...) {
  # A location that is an on-again then off-again hot spot.
  # Less than ninety percent of the time-step intervals have been statistically
  # significant hot spots and none of the time-step intervals have been
  # statistically significant cold spots.
  # JP 2022-05-09: missing condition here that i think is implicit that
  #   there needs to be _at least_ 1 significant hotspot
  (sum(gs > 0 & sigs) / n < 0.9) &&
    # no sig cold spots
    !any(gs < 0 & sigs) &&
    # at least one hot spot
    any(gs > 0 & sigs)
}

sporadic_coldspot <- function(gs, sigs, n, ...) {
  (sum(gs < 0 & sigs) / n < 0.9) &&
    # not sig hot spots
    !any(gs > 0 & sigs) &&
    # at least 1 cold spot
    any(gs < 0 & sigs)
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
  # A location with a single uninterrupted run of statistically significant
  # cold spot bins in the final time-step intervals. The location has never been
  # a statistically significant cold spot prior to the final cold spot run and
  # less than ninety percent of all bins are statistically significant cold spots
  if (!(sigs[n] && gs[n] < 0)) return(FALSE)

  n_final_run <- which(diff(cumsum(c(rev(sigs), FALSE))) == 0)[1]
  run_index <- seq(n-n_final_run + 1, n, by = 1)

  all(!sigs[-run_index]) &&
    all(sigs[run_index]) &&
    all(!((sigs[-run_index]) & (gs[-run_index] > 0))) &&
    all(!sigs[-run_index]) &&
    (sum(sigs & gs < 0) / n < 0.9)
}

intensifying_coldspot <- function(gs, sigs, n, tau, tau_p,
                                  threshold, ...){
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
  "new hotspot" = new_hotspot,
  "new coldspot" = new_coldspot,
  "consecutive hotspot" = consecutive_hotspot,
  "consecutive coldspot" = consecutive_coldspot,
  "intensifying hotspot" = intensifying_hotspot,
  "intensifying coldspot" = intensifying_coldspot,
  "persistent hotspot" = persistent_hotspot,
  "persistent coldspot" = persistent_coldspot,
  "diminishing hotspot" = diminishing_hotspot,
  "diminishing coldspot" = diminishing_coldspot,
  "oscilating hotspot" = oscilating_hotspot,
  "oscilating coldspot" = oscilating_coldspot,
  "historical hotspot" = historical_hotspot,
  "historical coldspot" = historical_coldspot,
  "sporadic hotspot" = sporadic_hotspot,
  "sporadic coldspot" = sporadic_coldspot,
  "no pattern detected" = function(...) TRUE)



#' Classify Hot Spot results
#'
#' Given the Gi* time-series and Mann Kendall scores classify the hotspot values
#' @keywords internal
classify_hotspot <- function(.x, threshold) {
  gs = .x[["gi_star"]]
  sigs = .x[["p_sim"]] <= threshold
  n = length(gs)
  mktest <- Kendall::MannKendall(gs)
  tau = mktest[["tau"]]
  tau_p = as.numeric(mktest[["sl"]])


  res <- lapply(fxs, function(.x, ...) .x(...),
                gs = gs,
                sigs = sigs,
                n = n,
                tau = tau,
                tau_p = tau_p,
                threshold = threshold
  )
  cbind(
    as.data.frame(unclass(mktest)),
    classification = as.character(names(res[unlist(res)])[1])
  )

}


