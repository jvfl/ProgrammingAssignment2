## File defines functions capable of creating an special matrix that can cache 
## it's inverse and an function that computes and caches such inverse.

#' Creates a special matrix that can cache it's own inverse.
#' @param cacheMatrix The matrix whose inverse will be cached.
#' @return A list of functions to manipulate the cached matrix contents and it's
#' inverse.
makeCacheMatrix <- function(cacheMatrix = matrix()) {
  
  cacheMatrixInverse <- NULL
  
  setMatrix <- function(newMatrix) {
    cacheMatrix <<- newMatrix
    cacheMatrixInverse <<- NULL
  }
  
  getMatrix <- function() cacheMatrix
  
  setInverse <- function(matrixInverse) cacheMatrixInverse <<- matrixInverse
  
  getInverse <- function() cacheMatrixInverse
  
  list(setMatrix = setMatrix, 
       getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}

#' Computes and stores the inverse of a cacheMatrix.
#' @param cacheMatrix A special matrix created with the makeCacheMatrix function.
#' @return The cacheMatrix inverse, who'll be stored in the cacheMatrix itself.
cacheSolve <- function(cacheMatrix, ...) {
  
  cacheMatrixInverse <- cacheMatrix$getInverse()
  
  if(!is.null(cacheMatrixInverse)) {
    message("Getting cached data")
    return(cacheMatrixInverse)
  }
  
  data <- cacheMatrix$getMatrix()
  
  cacheMatrixInverse <- solve(data, ...)
  cacheMatrix$setInverse(cacheMatrixInverse)
  
  cacheMatrixInverse
}
