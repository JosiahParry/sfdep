
# Selection ---------------------------------------------------------------
# Selection method
# to do  update `times` attribute when subset and data is active
#' @export
`[.spacetime` <- function(x, ...) {
  NextMethod()
}

#' @export
`[[.spacetime` <- function(x, ...) {
  NextMethod()
}

#' Update spacetime attributes
#'
#' Update's a spacetime object's number of locations and time periods.
#' A spacetime object's attributes are sticky and will not change if subsetted
#' for example by using [`dplyr::filter()`] or [`dplyr::slice()`]. Update the
#' locations and times of a spacetime object.
#'
#' @param x a spacetime object
#' @param ... unused
#' @returns an object of class spacetime with updated attributes
spt_update <- function(x, ...) {
  stopifnot(is_spacetime(x))
  context <- active(x)
  .loc_col <- attr(x, "loc_col")
  .time_col <- attr(x, "time_col")

  if (context == "data") {
    times <- sort(unique(x[[.time_col]]))
    n_times <- length(times)
    locs <- attr(x, "geometry")[[.loc_col]]
    n_locs <- length(locs)
  }

  if (context == "geometry") {
    times <- sort(unique(attr(x, "data")[[.time_col]]))
    n_times <- length(times)
    locs <- x[[.loc_col]]
    n_locs <- length(locs)
  }
  attr(x, "locs") <- locs
  attr(x, "times") <- times
  attr(x, "n_times") <- n_times
  attr(x, "n_locs") <- n_locs
  x
}


# To sf -------------------------------------------------------------------

#' Cast between `spacetime` and `sf` classes
#'
#' @param x for [sf::st_as_sf()] a spacetime object. For [sfdep::as_spacetime()]
#'   an sf object.
#' @param ... arguments passed to merge.
#' @export
#' @rdname as_spacetime
as_sf <- function(x, ...) {
  if (active(x) == "geometry") x <- activate(x, "data")

  merge(attr(x, "geometry"), x,
        by = attr(x, "loc_col"), ...)
}



# From sf -----------------------------------------------------------------

#' Convert sf object to spacetime
#'
#'
#' @param .loc_col the quoted name of the column containing unique location identifiers.
#' @param .time_col the quoted name of the column containing time periods.
#' @export
#' @examples
#'
#' if (require(dplyr, quietly = TRUE)) {
#'   df_fp <- system.file("extdata", "bos-ecometric.csv", package = "sfdep")
#'   geo_fp <- system.file("extdata", "bos-ecometric.geojson", package = "sfdep")
#'
#'   # read in data
#'   df <- read.csv(
#'     df_fp, colClasses = c("character", "character", "integer", "double", "Date")
#'   )
#'   geo <- sf::st_read(geo_fp)
#'
#'   # Create spacetime object called `bos`
#'   bos <- spacetime(df, geo,
#'                    .loc_col = ".region_id",
#'                    .time_col = "time_period")
#'
#'   as_sf(bos)
#'   if (require("dplyr", quietly=TRUE)) {
#'     as_spacetime(as_sf(bos) , ".region_id", "year")
#'   }
#'}
#' @returns
#' For `as_spacetime()` returns a spacetime object. For `as_sf()`, an sf object.
as_spacetime <- function(x, .loc_col, .time_col, ...) {
  UseMethod("as_spacetime")
}

#' @rdname as_spacetime
#' @export
as_spacetime.sf <- function(x, .loc_col, .time_col, ...) {
  d <- utils::getFromNamespace("distinct.sf", "sf")
# distinct.sf requires dplyr (suggested here) and rlang (imported here)
  geometry <- d(x, !!rlang::sym(.loc_col), !!rlang::sym(attr(x, "sf_column")))

  new_spacetime_data(sf::st_drop_geometry(x), geometry, .loc_col, .time_col)
}


# Print -------------------------------------------------------------------
#' @export
#' @returns
#' A spacetime object
print.spacetime <- function(x, ...) {
  context <- active(x)
  n_locs <- attr(x, "n_locs")
  n_times <- attr(x, "n_times")
  cli::cli_div(theme = list(rule = list(color = "grey")))
  cli::cli_text(cli::style_bold(cli::style_italic("spacetime \u2500\u2500\u2500\u2500")))
  cli::cli_text(cli::col_grey("Context:", cli::style_italic("{.var {context}}")))
  cli::cli_text(cli::col_grey("{.emph {n_locs}} locations {.var {attr(x, 'loc_col')}}"))
  cli::cli_text(
    cli::col_grey("{.emph {n_times}} time periods {.var {attr(x, 'time_col')}}")
    )
  cli::cli_rule(cli::col_grey(cli::style_italic("{context} context")))
  NextMethod()
}



# dplyr group_by ----------------------------------------------------------

#' tidyverse methods for spacetime objects
#'
#' dplyr methods for spacetime objects.
#'
#' @name tidyverse
#' @param .data a data frame
#' @param ... additional arguments
group_by.spacetime <- function(.data, ...) {
  res <- NextMethod(.data, ...)
  class(res) <- c("spacetime", class(res))
  res
}


#' @name tidyverse
#' @return a spacetime object
mutate.spacetime <- function(.data, ...) {
  res <- NextMethod(.data, ...)
  .cols <- colnames(res)
  attributes(res) <- attributes(.data)
  attr(res, "names") <- .cols
  res
}



#' @name tidyverse
ungroup.spacetime <- function(.data, ...) {
  res <- NextMethod(.data, ...)
  .cols <- colnames(res)
  attributes(res) <- attributes(.data)
  attr(res, "names") <- .cols
  res
}

# from: https://raw.githubusercontent.com/r-spatial/sf/main/R/tidyverse.R
# 2022-06-27 08:54:11
register_all_s3_methods = function() {
  register_s3_method("dplyr", "group_by", "spacetime")
  register_s3_method("dplyr", "ungroup", "spacetime")
  register_s3_method("dplyr", "mutate", "spacetime")

}

# from: https://github.com/tidyverse/hms/blob/master/R/zzz.R
# Thu Apr 19 10:53:24 CEST 2018
register_s3_method <- function(pkg, generic, class, fun = NULL) {
  stopifnot(is.character(pkg), length(pkg) == 1)
  stopifnot(is.character(generic), length(generic) == 1)
  stopifnot(is.character(class), length(class) == 1)

  if (is.null(fun)) {
    fun <- get(paste0(generic, ".", class), envir = parent.frame())
  } else {
    stopifnot(is.function(fun))
  }

  if (pkg %in% loadedNamespaces()) {
    registerS3method(generic, class, fun, envir = asNamespace(pkg))
  }

  # Always register hook in case package is later unloaded & reloaded
  setHook(
    packageEvent(pkg, "onLoad"),
    function(...) {
      registerS3method(generic, class, fun, envir = asNamespace(pkg))
    }
  )
}
