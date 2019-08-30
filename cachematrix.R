## Module to calculate the inverse of a matrix and cache it.

## This functon create a matrix object and caches its inverse
## Return Type is a list containing:
##  1. set the value of the matrix
##  2. get the value of the matrix
##  3. set the value of the inverse
##  4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() return(x)
  setinv <- function(inv) inverse <<- inv
  getinv <- function() return(inverse)
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## This function computes the inverse of matrix returned by
## the makeCacheMatrix method
cacheSolve <- function(x, ...) {
  inverse <- x$getinv()
  if (!is.null(inverse)) {
    message("fetching cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinv(inverse)
  return(inverse)
}