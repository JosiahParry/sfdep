#' rstudio::conf(2022L) application
#'
#' Topics:
#'
#' doing spatial analysis in R using the tidyverse
#' i learned oon ArcGIS
#' transitioned to QGIS because arc wasnt free
#' tried in R but SP wasn't useable for me. Used s4 objects which are still weird to be
#' right before grad school sf became popular and changed the game for me
#'
#' My professor Geoff Boeing introduced us to spatial statistics (in the lattice perspective) things like spatial autocorrelation etc using PySal. Pysal is cool. I like it. But its not R
#'
#' Outline:
#'
#' - My spatial background
#' - the R spatial econometrics challenge
#' - spdep is not friendly for tidyverse
#'   - naturally i want to do things in a mutate call / pipe
#'   - not compatible
#' - sfdep:
#'   - base R compliant
#'
#'
#' Abstract:
#'
#' R has come quite a long way to enable spatial analysis over the past few years.
#' Packages such as sf have made spatial analysis and mapping easier for many. However,
#' adoption of R for spatial statistics and econometrics has been limited. Many spatial analysts,
#' researchers, and practitioners lean on Python libraries such as pysal.
#'
#' In this talk I briefly discuss my journey through spatial analysis and introduce a new package sfdep which provides a tidy interface to spatial statistics. sfdep is an interface to the underutilized spdep package as well as implements other common spatial statistics. sfdep provides functionality equivalent to that of Pysal's exploratory spatial data analysis library.
