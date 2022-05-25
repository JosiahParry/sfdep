# sfdep (development version)

- implements `spdep::include.self()` function and a new `remove_self()` which is available separately in spdep due to [issue 83](https://github.com/r-spatial/spdep/issues/83) as `spdep::remove.self()`. 
- `local_g_perm()` returns correct G statistic. Previously returned only the internal measure.  
- `local_gstar()` and `local_gstar_perm()` do not warn when `self.included = TRUE`
- implement emerging hotspot analysis with spacetime objects
- implements a new s3 class spacetime to link geometry with time
  - see the [spacetime vignette](/articles/spacetime-s3.html).
- adds colocation quotient (CLQ) measures See the [colocation vignette](/articles/colocation-analysis.html):
  - `global_colocation()` for global colocation of one categorical variable
  - `pairwise_colocation()` for asymmetric colocation of subcategories of one or two categories
  - `local_colocation()` for local CLQ measure
  - these functions are not fast! PRs welcomed to improve computation speed.
- `st_kernel_weights()` now returns an attribute `kernel` which is set to the value of the kernel argument
- `cond_permute_nb()` specifies `SIMPLIFY = FALSE` in internal `mapply()` call which was causing errors in creating conditionally permuted neighbor lists

# sfdep 0.1.0

* Released on CRAN!
* Sending in for initial CRAN release
