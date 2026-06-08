# Comparison to spdep and pysal

## Global Statistics

Measures of global spatial autocorrelation.

| name              | impl                    | pysal              |
|:------------------|:------------------------|:-------------------|
| global_moran      | spdep::moran()          | esda.Moran()       |
| global_moran_perm | spdep::moran.mc()       | esda.Moran()       |
| global_moran_test | spdep::moran.test()     | esda.Moran()       |
| global_moran_bv   | internal                | esda.Moran_BV()    |
| global_c          | spdep::geary()          | esda.Geary()       |
| global_c_perm     | spdep::geary.mc()       | esda.Geary()       |
| global_c_test     | spdep::geary.mc()       | esda.Geary()       |
| global_g_test     | spdep::globalG.test()   | esda.G()           |
| global_jc_perm    | spdep::joincount.mc()   | esda.Join_Counts() |
| global_jc_test    | spdep::joincount.test() | esda.Join_Counts() |

## Local Indicators of Spatial Association

Measures of local spatial autocorrelation.

| name | impl | pysal |
|:---|:---|:---|
| local_c | spdep::localC() | esda.Geary_Local() |
| local_c_perm | spdep::localC_perm() | esda.Geary_Local(), esda.Geary_Local_MV() |
| local_g | spdep::localG() | esda.G_local().Gs |
| local_gstar | spdep::localG() | esda.G_local(star = True) |
| local_g_perm | spdep::localG_perm() | esda.G_Local() |
| local_gstar_perm | spdep::localG_perm | esda.G_Local() |
| local_jc_uni | internal | esda.Join_Counts_Local() |
| local_jc_bv | internal | esda.Join_Counts_Local_BV() |
| losh | spdep::LOSH() | esda.LOSH() |
| losh_perm | spdep::LOSH.mc() | esda.LOSH() |
| local_moran | spdep::localmoran_perm() | esda.Moran_Local() |
| local_moran_bv | internal | esda.Moran_Local_BV() |
| nb_match_test | internal |  |

### Not implemented

Functions present in Pysal that are not implemented in sfdep.

| function                            |
|:------------------------------------|
| esda.adbscan.ADBSCAN                |
| esda.Gamma                          |
| esda.Join_Counts_Local_MV           |
| esda.LOSH(inference = “chi-square”) |
| esda.Smaup                          |
| esda.Moran_Rate                     |
| esda.Moran_Local_Rate               |
| esda.boundary_silhouette            |
| esda.path_silhouette                |
| esda.silhouettes.nearest_label      |
