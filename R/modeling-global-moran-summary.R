moran_i_vars <- function(..., nb, wt) {
  # TODO: check that each vector is a numeric
  # check that each vectors doesn't have missing
  # values.
  vars <- rlang::dots_list(..., .named = TRUE)
  vars_names <- names(vars)
  # check type of each variable
  # if not numeric throw error

  for (i in seq_along(vars)) {
    if (!is.numeric(vars[[i]])) {
      # TODO improve error message
      # https://cli.r-lib.org/articles/semantic-cli.html
      cli::cli_abort(
        "Expected numeric column, {vars_names[i]} is  {class(vars[[i]])}"
      )
    } else if (any(is.na(vars[[i]]))) {
      # Elicit error if missing values found
    }
  }

  all_is <- lapply(vars, \(.x) {
    broom::tidy(global_moran_perm(.x, nb, wt))
  })

  dplyr::bind_cols(
    variable = names(vars),
    dplyr::bind_rows(all_is)
  )
  # TODO rename statistic to Moran's I

}

moran_i_vars(letters)
#
# tst <- function(...) {
#   rlang::dots_list(...)
# }
#
# tst(x =1, letters, y = function(.y) 1 + .y)
