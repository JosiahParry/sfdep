x <- guerry$crime_pers
y <- guerry$suicides
nb <- guerry_nb$nb
wt <- guerry_nb$wt
listw <- recreate_listw(nb, wt)
yj <- find_xj(y, nb)


# mem allocation the same. but cpp faster
res <- bench::mark(
  sfdep = local_moran_bv_impl(x, y, listw, 299),
  cpp_impl = lm_bv_impl(x, y, listw, 299),
  check = FALSE
)


# Cpp is slower. how come?
# cpp saves a LOT of memory

bench::mark(
  sfdep = permute_listw(listw),
  cpp_impl = perm_lw_cpp(listw),
  check = FALSE
)

res <- bench::mark(
  cpp = cond_permute(1:85, 85, lengths(nb)),
  sfdep = cond_permute_nb(nb),
  check = FALSE
)

# cpp faster here
# memory allocation better for base R
bench::mark(
  cpp = lm_bv_calc(x, yj, wt),
  sfdep = local_moran_bv_calc(x, yj,  wt),
  check = FALSE
)
