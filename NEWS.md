# sfdep (development version)

- adds colocation quotient (CLQ) measures:
  - `global_colocation()` for global colocation of one categorical variable
  - `pairwise_colocation()` for asymmetric colocation of subcategories of one or two categories
  - `local_colocation()` for local CLQ measure
  - these functions are not fast! PRs welcomed to improve computation speed.
- `st_kernel_weights()` now returns an attribute `kernel` which is set to the value of the kernel argument
- `cond_permute_nb()` specifies `SIMPLIFY = FALSE` in internal `mapply()` call which was causing errors in creating conditionally permuted neighbor lists

# sfdep 0.1.0

* Released on CRAN!
* Sending in for initial CRAN release
