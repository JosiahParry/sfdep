#' Emerging Hot Spot Analysis
#'
#' Emerging Hot Spot Analysis identifies trends in spatial clustering over a
#' period of time. Emerging hot spot analysis combines the Getis-Ord Gi* statistic
#' with the Mann-Kendall trend test to determine if there is a temporal trend
#' associated with local clustering of hot and cold spots.
#'
#' @param x a spacetime object and must be a spacetime cube see details for more.
#' @param .var a numeric vector in the spacetime cube with no missing values.
#' @param k default `1`. The number of time lags to include in the neighborhood
#'   for calculating the local Gi*. See details for more.
#' @param include_gi default `FALSE`. If `TRUE`, includes the local Gi* calculations
#'   in the attribute `gi_star`.
#' @param nb_col Optional. Default `NULL`. The name of the column in the `geometry`
#'   context of `x` containing spatial neighbors. If `NULL`, Queen's contiguity
#'   neighbors are identified.
#' @param wt_col Optional. Default `NULL`. The name of the column in the `geometry`
#'   context of `x` containing spatial weights. If `NULL`, row standardized weights
#'   are used.
#' @param nsim default `199`. The number of simulations to run in calculating the
#'   simulated p-value for the local Gi*.
#' @param threshold default `0.01`. The significance threshold to use.
#' @param ... unused.
#'
#'
#' @details
#'
#'  ## How Emerging Hot Spot Analysis Works
#'
#'  Emerging Hot Spot Analysis is a somewhat simple process. It works by first
#'  calculating the Gi* statistic for each location in each time period (time-slice).
#'  Next, for each location across all time-periods, the Mann-Kendall trend test
#'  is done to identify any temporal trend in Gi* values over all time periods.
#'  Additionally, each location is classified into one of seventeen categories based
#'  on [ESRI's emerging hot spot classification criteria](https://pro.arcgis.com/en/pro-app/2.8/tool-reference/space-time-pattern-mining/learnmoreemerging.htm).
#'
#'  The Mann-Kendall trend test is done using [`Kendall::MannKendall()`]. `Kendall`
#'  is not installed with sfdep and should be installed prior to use.
#'
#'  ## Using your own neighbors and weights
#'
#'  If you would like to use your own neighbors and weights, they must be created
#'  in the `geometry` context of a spacetime object. The arguments `nb_col`
#'  and `wt_col` must both be populated in order to use your own neighbor and weights
#'  definitions.
#'
#'  ## Time lagged neighbors
#'
#'  In addition to identifying neighbors in space, emerging hotspot analysis also
#'  incorporates the same observations from `k` periods ago-called a time lag. If
#'  the time lag k is 1 and the unit of time is month, the neighbors for the
#'  calculation of Gi* would include the spatial neighbors' values at time `t`
#'  and the same spatial neighbors' values at time `t-1`. If `k = 2`, it would include
#'  `t`, `t-1`, and `t-2`.
#'
#'  ## Missing values
#'
#'  Presently, there is no method of missing value handling. If there are missing
#'  values, the emerging hot spot analysis will fail. Be sure to fill or omit
#'  time-slices with missing values _prior_ to using emerging hot spot analysis.
#'
#' @seealso
#' [How Emerging Hot Spot Analysis works](https://pro.arcgis.com/en/pro-app/2.8/tool-reference/space-time-pattern-mining/learnmoreemerging.htm), [Emerging Hot Spot Analysis (Space Time Pattern Mining)](https://pro.arcgis.com/en/pro-app/2.8/tool-reference/space-time-pattern-mining/emerginghotspots.htm), and the video [Spatial Data Mining II: A Deep Dive into Space-Time Analysis](https://www.youtube.com/watch?v=0aV6HHwJuo4&t=3848s&ab_channel=EsriEvents) by ESRI.
#'
#' @returns
#'
#' Returns a data.frame.
#'
#' @examples
#'
#' if (requireNamespace("Kendall")) {
#' df_fp <- system.file("extdata", "bos-ecometric.csv", package = "sfdep")
#' geo_fp <- system.file("extdata", "bos-ecometric.geojson", package = "sfdep")
#'
#' # read in data
#' df <- read.csv(df_fp, colClasses = c("character", "character", "integer", "double", "Date"))
#' geo <- sf::st_read(geo_fp)
#'
#' # Create spacetime object called `bos`
#' bos <- spacetime(df, geo,
#'                  .loc_col = ".region_id",
#'                  .time_col = "time_period")
#'
#'
#' # conduct EHSA
#' ehsa <- emerging_hotspot_analysis(
#'   x = bos,
#'   .var = "value",
#'   k = 1,
#'   nsim = 9
#' )
#'
#' ehsa
#' }
#'
#' @export
emerging_hotspot_analysis <- function(x, .var, k = 1, include_gi = FALSE,
                                      nb_col = NULL, wt_col = NULL,
                                      nsim = 199, threshold = 0.01, ...) {

  check_pkg_suggests("Kendall")

  # activate data if not already
  if (active(x) == "geometry") x <- activate(x, "data")

  x <- spt_order(x)

  geo_class <- sf::st_geometry_type(attr(x, "geometry"), by_geometry = FALSE)

  .loc_col <- attr(x, "loc_col")
  .time_col <- attr(x, "time_col")
  times <- x[[.time_col]]
  # identify number of lcoations and times
  n_locs <- length(unique(x[[.loc_col]]))
  n_times <- length(unique(times))

  if (any(is.null(nb_col), is.null(wt_col))) {
    # create neighbors and weights if not present
    # TODO check if points, if points, use distance band
    if (grepl("POLYGON|MULTIPOLYGON", geo_class)) {
      nb <- include_self(st_contiguity(sf::st_geometry(attr(x, "geometry"))))
      wt <- st_weights(nb)
    } else if (grepl("POINT|MULTIPOINT", geo_class)) {
      nb <- include_self(st_dist_band(sf::st_geometry(attr(x, "geometry"))))
      wt <- st_weights(nb)
    }

  } else {
    nb <- attr(x, "geometry")[[nb_col]]
    wt <- attr(x, "geometry")[[wt_col]]
  }

  # convert nb and wt to time-lagged spacetime
  nbt <- spt_nb(nb, n_times, n_locs, k)
  wtt <- spt_wt(wt, nbt, n_times, n_locs, k)

  all_gis <- local_g_spt(x[[.var]], times, nbt, wtt, n_locs, nsim = nsim)

  res_ehs <- cbind(
    location = attr(x, "geometry")[[.loc_col]],
    do.call(
      rbind,
      lapply(split(all_gis, x[[.loc_col]]), function(.x) {
        classify_hotspot(.x, threshold)
      })
    )
  )

  rownames(res_ehs) <- NULL


  res_ehs <- stats::setNames(
    res_ehs[, c("location", "tau", "sl", "classification")],
    c("location", "tau", "p_value", "classification")
    )

  class(res_ehs) <- setdiff(class(x), "spacetime")

  if (include_gi) {
    attr(res_ehs, "gi_star") <- all_gis
  }

  res_ehs

}


