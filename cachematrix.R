##20170216 Programming Assignment 2: Lexical Scoping 
##Assignment: Caching the Inverse of a Matrix
##The objective of the funtion is to gain benefit from caching the inverse of a matrix rather
##than compute it repeatedly

##This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(mtrx = matrix()) {
  invrs <- NULL
  set <- function(y) {
    mtrx <<- y
    invrs <<- NULL
  }
  get <- function() mtrx
  setInverse <- function(solve) invrs <<- solve
  getInverse <- function() invrs
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(mtrx, ...) {
  ## Return a matrix that is the inverse of 'x'
  invrs <- mtrx$getInverse()
  if (!is.null(invrs)) {
    message("getting cached data")
    return(invrs)
  }
  data <- mtrx$get()
  invrs <- solve(data, ...)
  mtrx$setInverse(invrs)
  invrs 
}
