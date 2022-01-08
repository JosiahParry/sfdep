#' Apply a function to neighbors
#'
#' Sometimes one may want to create custom lag variables or create some other neighborhood level metric that may not be defined yet. This `st_nb_apply()` enables you to apply a function to each observation's (xi) neighbors (xij).
#'
#' @param x A vector that will be used for neighbor xij values.
#' @inheritParams st_lag
#' @param .f A function definition. There are three default objects that can be used inside of the function definition:
#'
#' - `.xij`: neighbor values of `x` for the ith observation. This is simply the subset of x based on the corresponding `nb` list values for each element.
#' - `.nb`: neighbor positions.
#' - `.wt`: neighbor weights value.
#'
#' If any of these three function arguments are omitted from `.f`, dots (`...`) must be supplied.
#'
#' @param suffix The `map` variant to use. Options are "dbl", "int", "lgl", "chr", "list".
#' @export
#' @examples
#' #TODO make acs_nb object and make examples
st_nb_apply <- function(x, nb, wt, .f, suffix = "dbl", ...) {

  if (!requireNamespace("purrr", quietly = TRUE)) {
    cli::cli_abort("`purrr` must be installed to use `st_nb_apply()")
  }

  match.arg(suffix, c("dbl", "int", "lgl", "chr", "list"))

  map_fn <- switch(suffix,
                   dbl = purrr::map2_dbl,
                   int = purrr::map2_int,
                   lgl = purrr::map2_lgl,
                   chr = purrr::map2_chr,
                   list = purrr::map2)

  f <- purrr::as_mapper(.f, ...)

  map_fn(nb, wt, function(.nb, .wt, ...) {
    .xij <- x[.nb]

    f(.xij = .xij, .nb = .nb, .wt = .wt, ...)

  })
}



#st_nb_apply(x, nb, wt, .f = function(.xij, ...) sum(.xij), suffix = "dbl" )
