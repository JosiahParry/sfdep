# Compute Local Geary statistic

The Local Geary is a local adaptation of Geary's C statistic of spatial
autocorrelation. The Local Geary uses squared differences to measure
dissimilarity unlike the Local Moran. Low values of the Local Geary
indicate positive spatial autocorrelation and large refers to negative
spatial autocorrelation. Inference for the Local Geary is based on a
permutation approach which compares the observed value to the reference
distribution under spatial randomness. The Local Geary creates a pseudo
p-value. This is not an analytical p-value and is based on the number of
permutations and as such should be used with care.

## Usage

``` r
local_c(x, nb, wt, ...)

local_c_perm(x, nb, wt, nsim = 499, alternative = "two.sided", ...)
```

## Arguments

- x:

  a numeric vector, or list of numeric vectors of equal length.

- nb:

  a neighbor list

- wt:

  a weights list

- ...:

  other arguments passed to
  [`spdep::localC_perm()`](https://r-spatial.github.io/spdep/reference/localC.html),
  e.g. `zero.policy = TRUE` to allow for zones without neighbors.

- nsim:

  The number of simulations used to generate reference distribution.

- alternative:

  A character defining the alternative hypothesis. Must be one of
  "two.sided", "less" or "greater".

## Value

a `data.frame` with columns

- `ci`: Local Geary statistic

- `e_ci`: expected value of the Local Geary based on permutations

- `z_ci`: standard deviation based on permutations

- `var_ci`: variance based on permutations

- `p_ci`: p-value based on permutation sample standard deviation and
  means

- `p_ci_sim`: p-value based on rank of observed statistic

- `p_folded_sim`: p-value based on the implementation of Pysal which
  always assumes a two-sided test taking the minimum possible p-value

- `skewness`: sample skewness

- `kurtosis`: sample kurtosis

## Details

### Overview

The Local Geary can be extended to a multivariate context. When `x` is a
numeric vector, the univariate Local Geary will be calculated. To
calculate the multivariate Local Moran provide either a list or a
matrix. When `x` is a list, each element must be a numeric vector of the
same length and of the same length as the neighbours in `listw`. In the
case that `x` is a matrix the number of rows must be the same as the
length of the neighbours in `listw`.

While not required in the univariate context, the standardized Local
Geary is calculated. The multivariate Local Geary is *always*
standardized.

The univariate Local Geary is calculated as \\c_i = \sum_j w\_{ij}(x_i -
x_j)^2\\ and the multivariate Local Geary is calculated as \\c\_{k,i} =
\sum\_{v=1}^{k} c\_{v,i}\\ as described in Anselin (2019).

### Implementation

These functions are based on the implementations of the local Geary
statistic in the development version of spdep. They are based on
[spdep::localC](https://r-spatial.github.io/spdep/reference/localC.html)
and
[spdep::localC_perm](https://r-spatial.github.io/spdep/reference/localC.html).

[spdep::localC_perm](https://r-spatial.github.io/spdep/reference/localC.html)
and thus local_c_perm utilize a conditional permutation approach to
approximate a reference distribution where each observation `i` is held
fixed, randomly samples neighbors, and calculated the local C statistic
for that tuple (`ci`). This is repeated `nsim` times. From the
simulations 3 different types of p-values are calculated—all of which
have their potential flaws. So be *extra judicious* with using p-values
to make conclusions.

- `p_ci`: utilizes the sample mean and standard deviation. The p-value
  is then calculated using
  [`pnorm()`](https://rdrr.io/r/stats/Normal.html)–assuming a normal
  distribution which isn't always true.

- `p_ci_sim`: uses the rank of the observed statistic.

- `p_folded_sim`: follows the pysal implementation where p-values are in
  the range of \[0, 0.5\]. This excludes 1/2 of all p-values and should
  be used with caution.

## References

Anselin, L. (1995), Local Indicators of Spatial Association—LISA.
Geographical Analysis, 27: 93-115.
[doi:10.1111/j.1538-4632.1995.tb00338.x](https://doi.org/10.1111/j.1538-4632.1995.tb00338.x)

Anselin, L. (2019), A Local Indicator of Multivariate Spatial
Association: Extending Geary's c. Geogr Anal, 51: 133-150.
[doi:10.1111/gean.12164](https://doi.org/10.1111/gean.12164)

## Author

Josiah Parry, <josiah.parry@gmail.com>

## Examples

``` r

local_c_perm(guerry_nb$crime_pers, guerry_nb$nb, guerry_nb$wt)
#>             ci        cluster      e_ci    var_ci        z_ci       p_ci
#> 1  0.987725319      High-High 2.5302015 1.4992710 -1.25973265 0.20776582
#> 2  0.838345145      High-High 1.7386999 0.5914383 -1.17073588 0.24170496
#> 3  0.702801925      High-High 1.8621051 0.6528794 -1.43476423 0.15135429
#> 4  0.100402310        Low-Low 1.9210607 1.5619953 -1.45676196 0.14518210
#> 5  0.244977901        Low-Low 1.1500848 0.6981356 -1.08325313 0.27869608
#> 6  1.353310326        Low-Low 3.1855816 1.7139859 -1.39954272 0.16165030
#> 7  3.623634688      High-High 5.4245539 5.7201046 -0.75299527 0.45145276
#> 8  1.536196129        Low-Low 4.5135154 5.5528101 -1.26348184 0.20641607
#> 9  0.861013426 Other Positive 1.0012085 0.2591610 -0.27538989 0.78301671
#> 10 0.725268063        Low-Low 1.3100696 0.6449975 -0.72816444 0.46651293
#> 11 0.571428163        Low-Low 3.6245484 1.7457675 -2.31073804 0.02084733
#> 12 0.001151306        Low-Low 1.7714760 1.6741020 -1.36823901 0.17123728
#> 13 1.912088100 Other Positive 1.1447576 0.7037120  0.91471332 0.36034215
#> 14 1.140906230        Low-Low 1.1218034 0.3459832  0.03247657 0.97409200
#> 15 1.007912947       Negative 1.4407788 0.4940950 -0.61581194 0.53801864
#> 16 0.303683489 Other Positive 1.0317763 0.3147637 -1.29776023 0.19436974
#> 17 1.214635190      High-High 1.0545683 0.2342039  0.33075358 0.74083063
#> 18 1.920535316        Low-Low 1.4312576 0.5977627  0.63283578 0.52684089
#> 19 1.610359805      High-High 3.9023805 1.9477387 -1.64230264 0.10052730
#> 20 0.446973853      High-High 2.3870300 2.0097301 -1.36850197 0.17115501
#> 21 5.156798989      High-High 6.5389013 3.2889894 -0.76209473 0.44600347
#> 22 0.919215466       Negative 1.0350553 0.2066213 -0.25484169 0.79884537
#> 23 3.107530093 Other Positive 2.4380062 4.1283634  0.32951649 0.74176533
#> 24 0.230533117        Low-Low 1.6975244 1.0389532 -1.43922777 0.15008600
#> 25 1.349244911 Other Positive 1.4953061 0.6777055 -0.17742478 0.85917474
#> 26 1.067920103      High-High 1.0564530 0.2362492  0.02359218 0.98117791
#> 27 0.418377023      High-High 2.9531117 4.0422928 -1.26071996 0.20740976
#> 28 0.208541175        Low-Low 1.9141620 0.9707897 -1.73109097 0.08343554
#> 29 0.788634381        Low-Low 1.0204786 0.2824414 -0.43624673 0.66265773
#> 30 0.248262048        Low-Low 1.0549304 0.2957507 -1.48331061 0.13799190
#> 31 0.364299667      High-High 1.2898347 0.5986834 -1.19617372 0.23162878
#> 32 0.136623420        Low-Low 1.8892490 1.2783310 -1.55012774 0.12111086
#> 33 0.708692856      High-High 1.0896611 0.2620231 -0.74425029 0.45672507
#> 34 3.108893756      High-High 4.0051591 1.8712519 -0.65519482 0.51234233
#> 35 1.981523756 Other Positive 0.9766742 0.2972077  1.84319391 0.06530070
#> 36 0.922565985        Low-Low 1.0544186 0.2830862 -0.24781600 0.80427677
#> 37 1.044173246      High-High 1.8300265 0.8287265 -0.86324856 0.38800082
#> 38 0.332298593 Other Positive 1.0495041 0.4374009 -1.08443555 0.27817173
#> 39 0.940330887      High-High 1.0556557 0.2135067 -0.24958421 0.80290891
#> 40 1.904654615       Negative 2.1141878 0.6952451 -0.25129496 0.80158607
#> 41 0.936064027        Low-Low 1.2483153 0.5664930 -0.41486514 0.67824063
#> 42 0.628686757 Other Positive 0.9959316 0.3536202 -0.61757145 0.53685787
#> 43 0.390598000 Other Positive 1.0738022 0.2806409 -1.28965948 0.19716891
#> 44 2.689756388        Low-Low 4.9666815 3.1791078 -1.27701534 0.20159683
#> 45 1.278289774       Negative 1.1920025 0.2896560  0.16032663 0.87262379
#> 46 0.793948353        Low-Low 3.9557159 2.6032180 -1.95963269 0.05003874
#> 47 1.683658729      High-High 2.8175537 0.8708148 -1.21509414 0.22433019
#> 48 1.301187948      High-High 3.3126397 2.3573068 -1.31009130 0.19016495
#> 49 2.342671020 Other Positive 1.3741101 0.5623938  1.29153640 0.19651774
#> 50 0.832225668      High-High 1.7702214 0.6524277 -1.16127392 0.24553052
#> 51 0.296215805      High-High 2.4036991 1.2170990 -1.91029826 0.05609482
#> 52 2.268056379       Negative 1.9133776 0.9858641  0.35721252 0.72093271
#> 53 0.876680843      High-High 1.4065244 0.5125325 -0.74009411 0.45924289
#> 54 0.414701945      High-High 1.2260675 0.4932709 -1.15524434 0.24799038
#> 55 1.979054136        Low-Low 2.2719459 3.9061689 -0.14819433 0.88218940
#> 56 0.561171709      High-High 1.4808781 0.4657015 -1.34770660 0.17775277
#> 57 0.378238452      High-High 1.8229805 1.3437253 -1.24633578 0.21264115
#> 58 1.849165484      High-High 2.2955249 1.0050703 -0.44523210 0.65615203
#> 59 1.207382342      High-High 2.2373789 0.8762710 -1.10031376 0.27119544
#> 60 1.157149740      High-High 1.1457056 0.8938569  0.01210458 0.99034218
#> 61 1.848857476 Other Positive 1.1352981 0.3566800  1.19478778 0.23216995
#> 62 0.155525055        Low-Low 1.1814654 0.8093517 -1.14039021 0.25412377
#> 63 0.642205848        Low-Low 2.1269621 2.1891453 -1.00350112 0.31561914
#> 64 0.749419937        Low-Low 4.3791252 7.3750254 -1.33656350 0.18136516
#> 65 1.283938081        Low-Low 2.1890394 1.6726563 -0.69983210 0.48403216
#> 66 1.470817990        Low-Low 4.0503016 7.5556343 -0.93842013 0.34802855
#> 67 1.263739703 Other Positive 1.0391647 0.3884136  0.36034149 0.71859178
#> 68 0.964844188      High-High 1.1122214 0.2975052 -0.27019862 0.78700745
#> 69 0.338990398      High-High 2.3442681 0.8920578 -2.12313696 0.03374238
#> 70 1.924872243      High-High 4.6855852 2.1742435 -1.87226433 0.06117004
#> 71 0.659890869        Low-Low 1.6835069 2.4823600 -0.64968777 0.51589392
#> 72 2.135743520 Other Positive 1.0592632 0.6162752  1.37125668 0.17029496
#> 73 0.710008255       Negative 1.0754975 0.1966501 -0.82419029 0.40983144
#> 74 1.424041460        Low-Low 2.0722928 0.9946043 -0.65000737 0.51568746
#> 75 0.706001903 Other Positive 1.0876162 0.3633600 -0.63307633 0.52668380
#> 76 1.774528303      High-High 4.5140731 2.5103004 -1.72908188 0.08379444
#> 77 0.242422537        Low-Low 1.8883149 1.1128984 -1.56017630 0.11871823
#> 78 0.694451670        Low-Low 1.5172115 0.6462868 -1.02343536 0.30610205
#> 79 0.001874143        Low-Low 1.9442535 1.9214462 -1.40126388 0.16113518
#> 80 0.055356016        Low-Low 1.7597653 0.8905756 -1.80608633 0.07090486
#> 81 0.428128655      High-High 0.9913836 0.3489998 -0.95343790 0.34036825
#> 82 2.024633135 Other Positive 1.5417557 0.7493661  0.55781460 0.57697099
#> 83 2.497611820 Other Positive 1.2637478 0.5126094  1.72335218 0.08482486
#> 84 1.048741881        Low-Low 1.0220117 0.2484388  0.05362805 0.95723151
#> 85 1.026405574 Other Positive 1.0738224 0.3646807 -0.07851924 0.93741503
#>    p_ci_sim p_folded_sim  skewness    kurtosis
#> 1     0.176        0.088 0.5547185 -0.06279701
#> 2     0.188        0.094 0.5697743  0.03835036
#> 3     0.100        0.050 0.6337871  0.12030182
#> 4     0.008        0.004 0.8616578  0.30289555
#> 5     0.260        0.130 0.9581043  0.62918986
#> 6     0.116        0.058 0.5466322  0.20462670
#> 7     0.480        0.240 0.6296362  0.30378009
#> 8     0.136        0.068 0.8460502  0.66053909
#> 9     0.848        0.424 0.6365611  0.32384195
#> 10    0.496        0.248 0.9797378  1.38660001
#> 11    0.004        0.002 0.5788117  0.36588430
#> 12    0.004        0.002 0.7452809 -0.12068707
#> 13    0.368        0.184 0.9226400  0.43294904
#> 14    0.852        0.426 0.8140157  0.64142620
#> 15    0.592        0.296 0.7932099  0.58557136
#> 16    0.104        0.052 0.8545307  0.62920291
#> 17    0.700        0.350 0.5160029 -0.09860068
#> 18    0.524        0.262 0.6378879 -0.05971532
#> 19    0.052        0.026 0.4700876 -0.12102364
#> 20    0.140        0.070 0.7323359  0.46913854
#> 21    0.468        0.234 0.1104821 -0.22012809
#> 22    0.896        0.448 0.6131696  0.14063447
#> 23    0.628        0.314 0.9267789  0.20482444
#> 24    0.056        0.028 0.7988173  0.49499575
#> 25    0.968        0.484 0.8479828  0.72304437
#> 26    0.916        0.458 0.5901119  0.17261793
#> 27    0.144        0.072 0.7101465  0.05155841
#> 28    0.016        0.008 0.5881690 -0.21887991
#> 29    0.712        0.356 0.9897033  1.92269644
#> 30    0.052        0.026 0.7349385  0.13319099
#> 31    0.160        0.080 0.9134813  0.37217986
#> 32    0.008        0.004 0.6904486 -0.00560892
#> 33    0.524        0.262 0.6366330  0.31613341
#> 34    0.516        0.258 0.3993656 -0.06970491
#> 35    0.108        0.054 0.7745696  0.73151659
#> 36    0.908        0.454 0.8831444  1.30637241
#> 37    0.444        0.222 0.5864434  0.02285063
#> 38    0.276        0.138 0.7474730  0.17878478
#> 39    0.880        0.440 0.4282149 -0.42555541
#> 40    0.900        0.450 0.5599459  0.36536696
#> 41    0.820        0.410 0.9630359  0.91240120
#> 42    0.648        0.324 0.7166774  0.09156859
#> 43    0.144        0.072 0.6529311  0.23912892
#> 44    0.184        0.092 0.3986520 -0.22797854
#> 45    0.764        0.382 0.6814098  0.52975140
#> 46    0.012        0.006 0.4229850 -0.10871401
#> 47    0.208        0.104 0.4047752  0.07551500
#> 48    0.184        0.092 0.4229555 -0.02720629
#> 49    0.232        0.116 0.6741496  0.20991262
#> 50    0.200        0.100 0.7983370  0.68739202
#> 51    0.012        0.006 0.5790616  0.45552295
#> 52    0.664        0.332 0.6017639  0.04004606
#> 53    0.536        0.268 0.7582262  0.16082852
#> 54    0.216        0.108 0.7021160 -0.05614549
#> 55    0.928        0.464 1.0922717  0.74880114
#> 56    0.108        0.054 0.8511583  0.76400897
#> 57    0.120        0.060 0.8576174  0.33192122
#> 58    0.728        0.364 0.4988748  0.03415802
#> 59    0.276        0.138 0.5153176  0.55570840
#> 60    0.764        0.382 1.2334326  1.40118120
#> 61    0.268        0.134 0.5571515 -0.20651072
#> 62    0.176        0.088 1.1422501  1.43899246
#> 63    0.344        0.172 0.8431191  0.42036098
#> 64    0.100        0.050 0.8697918  0.96687956
#> 65    0.548        0.274 0.7055437  0.59846921
#> 66    0.400        0.200 0.7951423  0.12010944
#> 67    0.644        0.322 0.6578707 -0.03568370
#> 68    0.892        0.446 0.5268665 -0.21384765
#> 69    0.008        0.004 0.6113943  0.03335488
#> 70    0.044        0.022 0.2990709  0.07142324
#> 71    0.664        0.332 1.3168408  1.53523412
#> 72    0.204        0.102 1.1830468  2.06570055
#> 73    0.480        0.240 0.6516212  0.13604109
#> 74    0.608        0.304 0.3311255 -0.49036773
#> 75    0.596        0.298 0.8049037  0.77875110
#> 76    0.068        0.034 0.2864614 -0.01220005
#> 77    0.036        0.018 0.7397824  0.59545091
#> 78    0.316        0.158 0.5288488 -0.39998419
#> 79    0.008        0.004 0.7591347 -0.08177678
#> 80    0.004        0.002 0.6974548  0.54730854
#> 81    0.352        0.176 0.8392758  0.47595712
#> 82    0.528        0.264 0.7824387  0.71186709
#> 83    0.136        0.068 0.7182784  0.06797578
#> 84    0.828        0.414 0.6337278  0.07381572
#> 85    0.956        0.478 0.8143583  0.60778857
```
