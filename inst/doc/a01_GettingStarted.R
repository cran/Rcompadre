## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setupDarwin, include=FALSE, eval = Sys.info()[["sysname"]] == "Darwin"----
#The following line seems to be required by pkgdown::build_site() on my machine, but causes build to break with R-CMD-CHECK on GH
knitr::opts_chunk$set(dev = "png", dev.args = list(type = "cairo-png"))

## ---- eval = TRUE,echo=FALSE--------------------------------------------------
library(Rcompadre)

## ----fake load the data, eval=FALSE-------------------------------------------
#  load("COMPADRE_v.4.0.1.RData")
#  compadre <- as_cdb(compadre)

## ----fake fetch data with cdb_fetch, eval=FALSE-------------------------------
#  Compadre <- cdb_fetch('compadre')

## ----load example COMPADRE data, eval=TRUE------------------------------------
data(Compadre)

## ----Summarise the database---------------------------------------------------
summary(Compadre)
Compadre

## ----Version of database------------------------------------------------------
VersionData(Compadre)

## ----Names--------------------------------------------------------------------
names(Compadre)

## ----Tables_and_Histograms----------------------------------------------------
table(Compadre$DicotMonoc)
hist(Compadre$StudyDuration,main = "StudyDuration")
plot(Compadre$Lon,Compadre$Lat,main = "Location")

## ----Check species------------------------------------------------------------
cdb_check_species(Compadre, "Succisa pratensis")

## ----Check species 2----------------------------------------------------------
spList <- c("Succisa pratensis", "Onodrim ent", "Aster amellus")
cdb_check_species(Compadre, spList)

## ----Check species3-----------------------------------------------------------
compadre_succisa<- cdb_check_species(Compadre, "Succisa pratensis", return_db = TRUE)
compadre_succisa

## ----get matrices 1-----------------------------------------------------------
matA(compadre_succisa)

## ----get matrices 2-----------------------------------------------------------
x <- matA(compadre_succisa)
x[[1]]

## ----get matrices 3-----------------------------------------------------------
classInfo <- matrixClass(compadre_succisa)
classInfo[[1]]
classInfo[[1]]$MatrixClassAuthor

## ----subset 1-----------------------------------------------------------------
x <- subset(Compadre, DicotMonoc == "Eudicot")
x

## ----subset 2-----------------------------------------------------------------
x <- subset(Compadre,DicotMonoc == "Eudicot" & 
              Country %in% c("USA", "CAN") & 
              MatrixDimension > 2)

## ----compare------------------------------------------------------------------
cdb_compare(Compadre,x)

## ----cdb_flag-----------------------------------------------------------------
Compadre_flagged <- cdb_flag(Compadre)

## ----subset flagged-----------------------------------------------------------
x <- subset(Compadre_flagged, check_NA_A == FALSE & check_ergodic == TRUE)

## ----matrix_calculations------------------------------------------------------
lambdaVals <- sapply(matA(x), popdemo::eigs, what="lambda")
summary(lambdaVals)
hist(lambdaVals, main = "Lambda values")

