## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setupDarwin, include=FALSE, eval = Sys.info()[["sysname"]] == "Darwin"----
# Prefer cairo-png for pkgdown on macOS when available, but fall back quietly
# when XQuartz/Cairo is not installed.
png_probe <- tempfile(fileext = ".png")
can_use_cairo_png <- isTRUE(tryCatch(
  {
    grDevices::png(filename = png_probe, type = "cairo-png")
    grDevices::dev.off()
    TRUE
  },
  error = function(...) FALSE,
  warning = function(...) FALSE
))
unlink(png_probe)

if (can_use_cairo_png) {
  knitr::opts_chunk$set(dev = "png", dev.args = list(type = "cairo-png"))
}

## ----eval=FALSE---------------------------------------------------------------
# install.packages("rcrossref")

## -----------------------------------------------------------------------------
library(Rcompadre)
if (requireNamespace("rcrossref", quietly = TRUE)) {
  library(rcrossref)
}

## ----load example COMPADRE data, eval=TRUE------------------------------------
data(Comadre)

## -----------------------------------------------------------------------------
Comadre <- subset(Comadre, Family == "Ursidae")

## -----------------------------------------------------------------------------
Comadre$Authors
Comadre$YearPublication
Comadre$DOI_ISBN

## ----eval = requireNamespace("rcrossref", quietly = TRUE)---------------------
cr_cn(unique(Comadre$DOI_ISBN), format = "text", style = "apa")

