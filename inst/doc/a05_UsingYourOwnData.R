## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(Rcompadre)
library(dplyr)

## -----------------------------------------------------------------------------
nMat <- 20
mort1 <- runif(nMat, 0, 1)
u1 <- runif(nMat, 0, 1 - mort1)
u2 <- 1 - mort1 - u1
mort2 <- runif(nMat, 0, 1)
u3 <- runif(nMat, 0, 1 - mort2)
u4 <- 1 - mort2 - u3

Uvals <- cbind(u1, u2, u3, u4)
Fvals <- rgamma(nMat, rep(1:4, each = 5))
Avals <- Uvals
Avals[, 3] <- Avals[, 3] + Fvals

Alist <- lapply(as.list(as.data.frame(t(Avals))), matrix,
  byrow = FALSE,
  nrow = 2, ncol = 2
)

## -----------------------------------------------------------------------------
meta <- data.frame(idNum = 1:20, shapeParam = rep(1:4, each = 5))

x <- cdb_build_cdb(mat_a = Alist, metadata = meta)
x

## -----------------------------------------------------------------------------
matA(x)[1]

## -----------------------------------------------------------------------------
x %>%
  filter(shapeParam > 2)

## -----------------------------------------------------------------------------
matrixClass(x)[1]

## -----------------------------------------------------------------------------
(stageDescriptor <- data.frame(
  MatrixClassOrganized = rep("active", 2),
  MatrixClassAuthor = c("small", "large")
))

## -----------------------------------------------------------------------------
stageDesc <- list()
stageDesc[1:20] <- list(stageDescriptor)

## -----------------------------------------------------------------------------
y <- cdb_build_cdb(
  mat_a = Alist, metadata = meta, stages = stageDesc,
  version = "Matrices Rock!"
)

## -----------------------------------------------------------------------------
matrixClass(y)[5]
MatrixClassAuthor(y)[5]

## -----------------------------------------------------------------------------
Version(y)
DateCreated(y)

## ----eval=FALSE---------------------------------------------------------------
#  save(y, "myMatrixDatabase.Rdata")

