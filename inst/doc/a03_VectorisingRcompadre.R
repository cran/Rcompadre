## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setupDarwin, include=FALSE, eval = Sys.info()[["sysname"]] == "Darwin"----
#The following line seems to be required by pkgdown::build_site() on my machine, but causes build to break with R-CMD-CHECK on GH
knitr::opts_chunk$set(dev = "png", dev.args = list(type = "cairo-png"))

## -----------------------------------------------------------------------------
library(Rcompadre)
library(popbio)
data(Compadre)

## -----------------------------------------------------------------------------
Compadre$matA <- matA(Compadre)

## -----------------------------------------------------------------------------
is.vector(Compadre$matA)   # it really is a vector
is.list(Compadre$matA)     # and also a list
length(Compadre$matA)      # with 150 matrices
Compadre$matA[1:3]         # here are the first three

## -----------------------------------------------------------------------------
Compadre$dim <- numeric(nrow(Compadre))  # create empty vector to store output
Compadre$dim[1] <- nrow(Compadre$matA[[1]])  # nrow matrix 1
Compadre$dim[2] <- nrow(Compadre$matA[[2]])  # nrow matrix 2
Compadre$dim[3] <- nrow(Compadre$matA[[3]])  # nrow matrix 3
# ... all the way to 150

## -----------------------------------------------------------------------------
# create empty vector to store output
Compadre$dim <- numeric(nrow(Compadre))

# loop through all rows of Compadre
for(i in seq_len(nrow(Compadre))) {
  Compadre$dim[i] <- nrow(Compadre$matA[[i]])
}

## -----------------------------------------------------------------------------
Compadre$dim <- sapply(Compadre$matA, nrow)

## -----------------------------------------------------------------------------
# function to determine whether matrix 'mat' has any stages with no transitions
NullStages <- function(mat) any(colSums(mat) == 0)

# apply function to every element of A
Compadre$null_stages <- sapply(Compadre$matA, NullStages)

## ---- eval=FALSE--------------------------------------------------------------
#  NullStages(Compadre$matA[[1]]) # apply function to single element

## -----------------------------------------------------------------------------
Compadre$null_stages <- sapply(matA(Compadre), NullStages)

## -----------------------------------------------------------------------------
# create new columns matA, matU, matF, matC, MatrixClassAuthor, etc..
CompUnnest <- cdb_unnest(Compadre)

## -----------------------------------------------------------------------------
# apply NullStages to every matA
CompUnnest$null_stages <- sapply(CompUnnest$matA, NullStages)

# count number of dormant stages in every MatrixClassOrganized
NumberDormant <- function(stages) length(which(stages == "dorm"))
CompUnnest$n_dormant <- sapply(CompUnnest$MatrixClassOrganized, NumberDormant)

## -----------------------------------------------------------------------------
sapply(CompUnnest$matA[1:6], nrow)
vapply(CompUnnest$matA[1:6], nrow, numeric(1)) # must specify output type

## -----------------------------------------------------------------------------
lapply(CompUnnest$matU[1:4], function(m) colSums(m))

## -----------------------------------------------------------------------------
# function to calculate life expectancy
lifeExpectancy <- function(matU, startLife) {
  N <- solve(diag(nrow(matU)) - matU)
  return(colSums(N)[startLife])
}

# get index of first active stage class with mpm_first_active()
CompUnnest$start_life <- mpm_first_active(CompUnnest)

# vectorise lifeExpectancy over matU and start_life
mapply(lifeExpectancy,             # function
       CompUnnest$matU[1:6],       # first argument to vectorise over
       CompUnnest$start_life[1:6]) # second argument to vectorise over

## ----error=TRUE---------------------------------------------------------------
# works for a single matrix
lambda(CompUnnest$matA[[1]])

# but fails when applied to all matrices because a few have missing values
CompUnnest$lambda <- sapply(CompUnnest$matA, lambda)

## -----------------------------------------------------------------------------
# add column 'check_NA_A', indicating whether matA contains missing values (T/F)
CompFlag <- cdb_flag(CompUnnest, checks = "check_NA_A")

# remove rows where matA contains missing values
CompSub <- subset(CompFlag, check_NA_A == FALSE)

# apply lambda() to every remaining matA
CompSub$lambda <- sapply(CompSub$matA, lambda)

## -----------------------------------------------------------------------------
# identify rows with no missing values in matA
no_missing <- which(CompFlag$check_NA_A == FALSE)

# create placeholder column for lambda
CompFlag$lambda <- NA

# apply lambda() to all matA with no missing values
CompFlag$lambda[no_missing] <- sapply(CompFlag$matA[no_missing], lambda)

## -----------------------------------------------------------------------------
lambda2 <- function(mat) {
  # check mat for missing values: if TRUE return NA, else return lambda(mat)
  ifelse(any(is.na(mat)), NA, lambda(mat))     
}

CompUnnest$lambda <- sapply(CompUnnest$matA, lambda2)

## -----------------------------------------------------------------------------
lambda3 <- function(mat) {
  # try lambda(mat): if error return NA
  tryCatch(lambda(mat), error = function(err) NA)
} 

CompUnnest$lambda <- sapply(CompUnnest$matA, lambda3)

