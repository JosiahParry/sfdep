# Emerging Hot Spot Analysis

Emerging Hot Spot Analysis identifies trends in spatial clustering over
a period of time. Emerging hot spot analysis combines the Getis-Ord Gi\*
statistic with the Mann-Kendall trend test to determine if there is a
temporal trend associated with local clustering of hot and cold spots.

## Usage

``` r
emerging_hotspot_analysis(
  x,
  .var,
  k = 1,
  include_gi = FALSE,
  nb_col = NULL,
  wt_col = NULL,
  nsim = 199,
  threshold = 0.01,
  ...
)
```

## Arguments

- x:

  a spacetime object and must be a spacetime cube see details for more.

- .var:

  a numeric vector in the spacetime cube with no missing values.

- k:

  default `1`. The number of time lags to include in the neighborhood
  for calculating the local Gi\*. See details for more.

- include_gi:

  default `FALSE`. If `TRUE`, includes the local Gi\* calculations in
  the attribute `gi_star`.

- nb_col:

  Optional. Default `NULL`. The name of the column in the `geometry`
  context of `x` containing spatial neighbors. If `NULL`, Queen's
  contiguity neighbors are identified.

- wt_col:

  Optional. Default `NULL`. The name of the column in the `geometry`
  context of `x` containing spatial weights. If `NULL`, row standardized
  weights are used.

- nsim:

  default `199`. The number of simulations to run in calculating the
  simulated p-value for the local Gi\*.

- threshold:

  default `0.01`. The significance threshold to use.

- ...:

  unused.

## Value

Returns a data.frame.

## Details

### How Emerging Hot Spot Analysis Works

Emerging Hot Spot Analysis is a somewhat simple process. It works by
first calculating the Gi\* statistic for each location in each time
period (time-slice). Next, for each location across all time-periods,
the Mann-Kendall trend test is done to identify any temporal trend in
Gi\* values over all time periods. Additionally, each location is
classified into one of seventeen categories based on [ESRI's emerging
hot spot classification
criteria](https://pro.arcgis.com/en/pro-app/2.8/tool-reference/space-time-pattern-mining/learnmoreemerging.htm).

The Mann-Kendall trend test is done using
[`Kendall::MannKendall()`](https://rdrr.io/pkg/Kendall/man/MannKendall.html).
`Kendall` is not installed with sfdep and should be installed prior to
use.

### Using your own neighbors and weights

If you would like to use your own neighbors and weights, they must be
created in the `geometry` context of a spacetime object. The arguments
`nb_col` and `wt_col` must both be populated in order to use your own
neighbor and weights definitions.

### Time lagged neighbors

In addition to identifying neighbors in space, emerging hotspot analysis
also incorporates the same observations from `k` periods ago-called a
time lag. If the time lag k is 1 and the unit of time is month, the
neighbors for the calculation of Gi\* would include the spatial
neighbors' values at time `t` and the same spatial neighbors' values at
time `t-1`. If `k = 2`, it would include `t`, `t-1`, and `t-2`.

### Missing values

Presently, there is no method of missing value handling. If there are
missing values, the emerging hot spot analysis will fail. Be sure to
fill or omit time-slices with missing values *prior* to using emerging
hot spot analysis.

## See also

[How Emerging Hot Spot Analysis
works](https://pro.arcgis.com/en/pro-app/2.8/tool-reference/space-time-pattern-mining/learnmoreemerging.htm),
[Emerging Hot Spot Analysis (Space Time Pattern
Mining)](https://pro.arcgis.com/en/pro-app/2.8/tool-reference/space-time-pattern-mining/emerginghotspots.htm),
and the video [Spatial Data Mining II: A Deep Dive into Space-Time
Analysis](https://www.youtube.com/watch?v=0aV6HHwJuo4&t=3848s&ab_channel=EsriEvents)
by ESRI.

## Examples

``` r

if (requireNamespace("Kendall")) {
df_fp <- system.file("extdata", "bos-ecometric.csv", package = "sfdep")
geo_fp <- system.file("extdata", "bos-ecometric.geojson", package = "sfdep")

# read in data
df <- read.csv(df_fp, colClasses = c("character", "character", "integer", "double", "Date"))
geo <- sf::st_read(geo_fp)

# Create spacetime object called `bos`
bos <- spacetime(df, geo,
                 .loc_col = ".region_id",
                 .time_col = "time_period")


# conduct EHSA
ehsa <- emerging_hotspot_analysis(
  x = bos,
  .var = "value",
  k = 1,
  nsim = 9
)

ehsa
}
#> Loading required namespace: Kendall
#> Reading layer `bos-ecometric' from data source 
#>   `/home/runner/work/_temp/Library/sfdep/extdata/bos-ecometric.geojson' 
#>   using driver `GeoJSON'
#> Simple feature collection with 168 features and 1 field
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -71.19115 ymin: 42.22788 xmax: -70.99445 ymax: 42.3974
#> Geodetic CRS:  NAD83
#>        location         tau     p_value      classification
#> 1   25025010405  0.11111111 0.720514774 no pattern detected
#> 2   25025010404 -0.33333334 0.210497677 no pattern detected
#> 3   25025010801 -0.20000000 0.474274337 no pattern detected
#> 4   25025010702 -0.60000002 0.020044670 no pattern detected
#> 5   25025010204 -0.46666667 0.073638275 no pattern detected
#> 6   25025010802 -0.33333334 0.210497677 no pattern detected
#> 7   25025010104 -0.33333334 0.210497677 no pattern detected
#> 8   25025000703 -0.60000002 0.020044670 no pattern detected
#> 9   25025000504 -0.33333334 0.210497677 no pattern detected
#> 10  25025000704 -0.42222223 0.107404657 no pattern detected
#> 11  25025010103  0.02222222 1.000000000 no pattern detected
#> 12  25025000803 -0.37777779 0.152406260 no pattern detected
#> 13  25025120201 -0.33333334 0.210497677 no pattern detected
#> 14  25025120104  0.46666667 0.073638320 no pattern detected
#> 15  25025110607  0.28888890 0.283130884 no pattern detected
#> 16  25025000302  0.37777779 0.152406216 no pattern detected
#> 17  25025000301  0.37777779 0.152406216 no pattern detected
#> 18  25025140400  0.06666667 0.858027697 no pattern detected
#> 19  25025140300  0.37777779 0.152406216 no pattern detected
#> 20  25025140201  0.46666667 0.073638320 no pattern detected
#> 21  25025140202  0.15555556 0.591505051 no pattern detected
#> 22  25025140102 -0.24444444 0.371093333 no pattern detected
#> 23  25025130402  0.46666667 0.073638320 no pattern detected
#> 24  25025130300  0.33333334 0.210497618 no pattern detected
#> 25  25025130200  0.33333334 0.210497618 no pattern detected
#> 26  25025130100 -0.20000000 0.474274337 no pattern detected
#> 27  25025120700  0.37777779 0.152406216 no pattern detected
#> 28  25025120600  0.60000002 0.020044684 no pattern detected
#> 29  25025120500  0.55555558 0.031823158 no pattern detected
#> 30  25025120400  0.55555558 0.031823158 no pattern detected
#> 31  25025110601  0.55555558 0.031823158 no pattern detected
#> 32  25025110502  0.42222223 0.107404709 no pattern detected
#> 33  25025110501 -0.02222222 1.000000000 no pattern detected
#> 34  25025110401 -0.20000000 0.474274337 no pattern detected
#> 35  25025101102  0.02222222 1.000000000 no pattern detected
#> 36  25025101101  0.37777779 0.152406216 no pattern detected
#> 37  25025101002  0.24444444 0.371093273 no pattern detected
#> 38  25025101001  0.46666667 0.073638320 no pattern detected
#> 39  25025100900  0.51111114 0.049098015 no pattern detected
#> 40  25025100800  0.46666667 0.073638320 no pattern detected
#> 41  25025100601  0.46666667 0.073638320 no pattern detected
#> 42  25025100500  0.37777779 0.152406216 no pattern detected
#> 43  25025100400 -0.24444444 0.371093333 no pattern detected
#> 44  25025100300 -0.28888890 0.283130825 no pattern detected
#> 45  25025100200 -0.15555556 0.591505051 no pattern detected
#> 46  25025100100 -0.06666667 0.858027637 no pattern detected
#> 47  25025092400 -0.15555556 0.591505051 no pattern detected
#> 48  25025092300  0.15555556 0.591505051 no pattern detected
#> 49  25025092200  0.06666667 0.858027697 no pattern detected
#> 50  25025092000  0.06666667 0.858027697 no pattern detected
#> 51  25025091900 -0.02222222 1.000000000 no pattern detected
#> 52  25025091800 -0.02222222 1.000000000 no pattern detected
#> 53  25025091700 -0.33333334 0.210497677 no pattern detected
#> 54  25025091600 -0.15555556 0.591505051 no pattern detected
#> 55  25025981100 -0.15555556 0.591505051 no pattern detected
#> 56  25025140105  0.02222222 1.000000000 no pattern detected
#> 57  25025120105  0.02222222 1.000000000 no pattern detected
#> 58  25025120301 -0.11111111 0.720514774 no pattern detected
#> 59  25025071201  0.02222222 1.000000000 no pattern detected
#> 60  25025091001 -0.06666667 0.858027637 no pattern detected
#> 61  25025091500 -0.24444444 0.371093333 no pattern detected
#> 62  25025091400  0.15555556 0.591505051 no pattern detected
#> 63  25025091300  0.46666667 0.073638320 no pattern detected
#> 64  25025091200  0.33333334 0.210497618 no pattern detected
#> 65  25025091100  0.77777779 0.002357483 no pattern detected
#> 66  25025090700  0.64444447 0.012266040 no pattern detected
#> 67  25025090600  0.60000002 0.020044684 no pattern detected
#> 68  25025090400  0.60000002 0.020044684 no pattern detected
#> 69  25025090300  0.73333335 0.004207492 no pattern detected
#> 70  25025090200  0.73333335 0.004207492 no pattern detected
#> 71  25025040801  0.46666667 0.073638320 no pattern detected
#> 72  25025010203  0.60000002 0.020044684 no pattern detected
#> 73  25025110403  0.60000002 0.020044684 no pattern detected
#> 74  25025110201  0.60000002 0.020044684 no pattern detected
#> 75  25025090100  0.51111114 0.049098015 no pattern detected
#> 76  25025082100  0.06666667 0.858027697 no pattern detected
#> 77  25025082000  0.37777779 0.152406216 no pattern detected
#> 78  25025081900  0.24444444 0.371093273 no pattern detected
#> 79  25025081800  0.42222223 0.107404709 no pattern detected
#> 80  25025081700  0.64444447 0.012266040 no pattern detected
#> 81  25025081500  0.68888891 0.007290363 no pattern detected
#> 82  25025081400  0.64444447 0.012266040 no pattern detected
#> 83  25025081300  0.64444447 0.012266040 no pattern detected
#> 84  25025110103  0.51111114 0.049098015 no pattern detected
#> 85  25025110301  0.60000002 0.020044684 no pattern detected
#> 86  25025140106  0.24444444 0.371093273 no pattern detected
#> 87  25025010701  0.42222223 0.107404709 no pattern detected
#> 88  25025010408  0.33333334 0.210497618 no pattern detected
#> 89  25025000503  0.64444447 0.012266040 no pattern detected
#> 90  25025081200  0.82222223 0.001282215 no pattern detected
#> 91  25025081100  0.82222223 0.001282215 no pattern detected
#> 92  25025080900  0.73333335 0.004207492 no pattern detected
#> 93  25025080801  0.82222223 0.001282215 no pattern detected
#> 94  25025080601  0.46666667 0.073638320 no pattern detected
#> 95  25025080500  0.51111114 0.049098015 no pattern detected
#> 96  25025080401  0.20000000 0.474274397 no pattern detected
#> 97  25025080300  0.20000000 0.474274397 no pattern detected
#> 98  25025071101 -0.20000000 0.474274337 no pattern detected
#> 99  25025070900 -0.06666667 0.858027637 no pattern detected
#> 100 25025140107  0.46666667 0.073638320 no pattern detected
#> 101 25025130404  0.15555556 0.591505051 no pattern detected
#> 102 25025130406  0.20000000 0.474274397 no pattern detected
#> 103 25025120103  0.11111111 0.720514774 no pattern detected
#> 104 25025100700  0.64444447 0.012266040 no pattern detected
#> 105 25025100603  0.82222223 0.001282215 no pattern detected
#> 106 25025092101  0.11111111 0.720514774 no pattern detected
#> 107 25025061101  0.64444447 0.012266040 no pattern detected
#> 108 25025070800  0.60000002 0.020044684 no pattern detected
#> 109 25025070700  0.55555558 0.031823158 no pattern detected
#> 110 25025070600  0.60000002 0.020044684 no pattern detected
#> 111 25025070500  0.37777779 0.152406216 no pattern detected
#> 112 25025070300  0.55555558 0.031823158 no pattern detected
#> 113 25025070200  0.46666667 0.073638320 no pattern detected
#> 114 25025070101  0.51111114 0.049098015 no pattern detected
#> 115 25025061200  0.37777779 0.152406216 no pattern detected
#> 116 25025061000  0.33333334 0.210497618 no pattern detected
#> 117 25025060800  0.64444447 0.012266040 no pattern detected
#> 118 25025060700  0.37777779 0.152406216 no pattern detected
#> 119 25025080100  0.55555558 0.031823158 no pattern detected
#> 120 25025060301  0.55555558 0.031823158 no pattern detected
#> 121 25025090901  0.37777779 0.152406216 no pattern detected
#> 122 25025060101  0.20000000 0.474274397 no pattern detected
#> 123 25025060600  0.15555556 0.591505051 no pattern detected
#> 124 25025060400  0.20000000 0.474274397 no pattern detected
#> 125 25025060200  0.20000000 0.474274397 no pattern detected
#> 126 25025050700  0.37777779 0.152406216 no pattern detected
#> 127 25025050600 -0.51111114 0.049097981 no pattern detected
#> 128 25025050500 -0.42222223 0.107404657 no pattern detected
#> 129 25025050400 -0.51111114 0.049097981 no pattern detected
#> 130 25025050300 -0.37777779 0.152406260 no pattern detected
#> 131 25025050200  0.24444444 0.371093273 no pattern detected
#> 132 25025040300  0.02222222 1.000000000 no pattern detected
#> 133 25025040200  0.24444444 0.371093273 no pattern detected
#> 134 25025030500  0.06666667 0.858027697 no pattern detected
#> 135 25025051101  0.20000000 0.474274397 no pattern detected
#> 136 25025051000 -0.37777779 0.152406260 no pattern detected
#> 137 25025030400  0.11111111 0.720514774 no pattern detected
#> 138 25025030300 -0.02222222 1.000000000 no pattern detected
#> 139 25025030200  0.06666667 0.858027697 no pattern detected
#> 140 25025030100  0.33333334 0.210497618 no pattern detected
#> 141 25025020200 -0.02222222 1.000000000 no pattern detected
#> 142 25025010600  0.11111111 0.720514774 no pattern detected
#> 143 25025010500 -0.33333334 0.210497677 no pattern detected
#> 144 25025010300 -0.06666667 0.858027637 no pattern detected
#> 145 25025000802  0.20000000 0.474274397 no pattern detected
#> 146 25025000701 -0.20000000 0.474274337 no pattern detected
#> 147 25025050101 -0.02222222 1.000000000 no pattern detected
#> 148 25025050901 -0.51111114 0.049097981 no pattern detected
#> 149 25025060501 -0.02222222 1.000000000 no pattern detected
#> 150 25025040100 -0.11111111 0.720514774 no pattern detected
#> 151 25025040600  0.60000002 0.020044684 no pattern detected
#> 152 25025000602  0.73333335 0.004207492 no pattern detected
#> 153 25025000601  0.77777779 0.002357483 no pattern detected
#> 154 25025000502 -0.24444444 0.371093333 no pattern detected
#> 155 25025000402 -0.33333334 0.210497677 no pattern detected
#> 156 25025000401 -0.28888890 0.283130825 no pattern detected
#> 157 25025000202 -0.20000000 0.474274337 no pattern detected
#> 158 25025000201 -0.28888890 0.283130825 no pattern detected
#> 159 25025040401 -0.02222222 1.000000000 no pattern detected
#> 160 25025020303  0.42222223 0.107404709 no pattern detected
#> 161 25025070402  0.46666667 0.073638320 no pattern detected
#> 162 25025020302  0.15555556 0.591505051 no pattern detected
#> 163 25025020301  0.37777779 0.152406216 no pattern detected
#> 164 25025020101  0.42222223 0.107404709 no pattern detected
#> 165 25025081001  0.42222223 0.107404709 no pattern detected
#> 166 25025010403  0.33333334 0.210497618 no pattern detected
#> 167 25025000100  0.33333334 0.210497618 no pattern detected
#> 168 25025051200  0.28888890 0.283130884 no pattern detected
```
