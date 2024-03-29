#' Calculate a mean over a list of matrices or CompadreMat objects
#'
#' @description
#' Calculates an element-wise mean over a list of matrices or CompadreMat
#' objects of constant dimension. 
#' 
#' The difference between function \code{mat_mean}) and (\code{mpm_mean} is that
#' \code{mat_mean} takes input as a list of matrices (e.g., a list of **A**
#' matrices) while \code{mat_mean} takes input as a list of `CompadreMat` objects and
#' thus calculates the mean matrices for both the **A** matrix and its
#' submatrices (**U**, **F**, **C**).
#'
#' @param x A list of matrices or, for \code{mpm_mean} a list of `CompadreMat` objects,
#'   all of the same dimension
#' @param na.rm Logical indicating whether missing values should be excluded
#'   (see \emph{Details}). Defaults to \code{FALSE}.
#'
#' @details
#' If \code{na.rm == TRUE}, missing values are ignored in the calculation of the
#' mean matrix. If \code{na.rm == TRUE} and a given element is \code{NA} in
#' \emph{every} matrix within \code{x}, the value returned for that element will
#' be \code{0}.
#'
#' @return A matrix (\code{mat_mean}) or a CompadreMat object (\code{mpm_mean}).
#'
#' @author Patrick Barks <patrick.barks@@gmail.com>
#' @author Owen R. Jones <jones@@biology.sdu.dk>
#'
#' @family data management
#'
#' @name mpm_mean
#'
#' @examples
#' # there are four rows for species 'Haplopappus_radiatus' in Compadre
#' mpms <- Compadre$mat[Compadre$SpeciesAuthor == "Haplopappus_radiatus"]
#' 
#' #The object mpms is a list, containing compadre objects
#' class(mpms)
#' class(mpms[[1]])
#' 
#' mpm_mean(mpms)
#'
#' # extract list of matA and take mean
#' mats <- matA(mpms)
#' mat_mean(mats)
NULL



#' @rdname mpm_mean
#' @export
mat_mean <- function(x, na.rm = FALSE) {
  if(!inherits(x,"list")){
    stop("x must be a list of matrices")
  }
  if(!inherits(x[[1]], "matrix")){
    stop("x must be a list of matrices")
  }
  
  n_row <- vapply(x, nrow, numeric(1))
  n_col <- vapply(x, ncol, numeric(1))
  if (length(unique(n_row)) != 1 || length(unique(n_col)) != 1) {
    stop("All matrices in list must be of same dimension")
  }
  if (na.rm) x <- lapply(x, zero_NA)
  n <- length(x)
  return(Reduce("+", x) / n)
}


# utility to zero out any NA in a list of matrices
zero_NA <- function(m) {
  m[is.na(m)] <- 0
  return(m)
}


#' @rdname mpm_mean
#' @importFrom methods new
#' @export
mpm_mean <- function(x, na.rm = FALSE) {
  if(!inherits(x, "list")){
    stop("x must be a list of CompadreMat objects")
  }
  if(!inherits(x[[1]], "CompadreMat")){
    stop("x must be a list of CompadreMat objects")
  }
  
  #Use lapply to get matrices, stages when x is a list of compadre objects
  matA <- lapply(x, function(m) m@matA)
  matU <- lapply(x, function(m) m@matU)
  matF <- lapply(x, function(m) m@matF)
  matC <- lapply(x, function(m) m@matC)
  stage_org <- lapply(x, function(m) m@matrixClass$MatrixClassOrganized)
  stage_aut <- lapply(x, function(m) m@matrixClass$MatrixClassAuthor)

  stage_org_col <- vapply(stage_org, paste, collapse = " ", "")
  stage_aut_col <- vapply(stage_aut, paste, collapse = " ", "")
  if (length(unique(stage_org_col)) != 1L) {
    warning(
      "CompadreMat objects in given list do not all have the same ",
      "MatrixClassOrganized. Returning MatrixClassOrganized from ",
      "first list element"
    )
  }
  if (length(unique(stage_aut_col)) != 1L) {
    warning(
      "CompadreMat objects in given list do not all have the same ",
      "MatrixClassAuthor. Returning MatrixClassAuthor from first ",
      "list element"
    )
  }

  meanA <- mat_mean(matA, na.rm = na.rm)
  meanU <- mat_mean(matU, na.rm = na.rm)
  meanF <- mat_mean(matF, na.rm = na.rm)
  meanC <- mat_mean(matC, na.rm = na.rm)

  new("CompadreMat",
    matA = meanA,
    matU = meanU,
    matF = meanF,
    matC = meanC,
    matrixClass = x[[1]]@matrixClass
  )
}
