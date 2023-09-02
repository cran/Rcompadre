## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setupDarwin, include=FALSE, eval = Sys.info()[["sysname"]] == "Darwin"----
# The following line seems to be required by pkgdown::build_site() on my machine,
# but causes build to break with R-CMD-CHECK on GH
knitr::opts_chunk$set(dev = "png", dev.args = list(type = "cairo-png"))

## ----eval=FALSE---------------------------------------------------------------
#  install.packages("rcrossref")

## -----------------------------------------------------------------------------
library(Rcompadre)
library(rcrossref)

## ----load example COMPADRE data, eval=TRUE------------------------------------
data(Comadre)

## -----------------------------------------------------------------------------
Comadre <- subset(Comadre, Family == "Ursidae")

## -----------------------------------------------------------------------------
Comadre$Authors
Comadre$YearPublication
Comadre$DOI_ISBN

## -----------------------------------------------------------------------------
cr_cn(unique(Comadre$DOI_ISBN), format = "text", style = "apa")

