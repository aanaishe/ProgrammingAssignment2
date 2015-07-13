## Matrix inversion is usually a costly computation. In cases such as this,
## it is beneficial to cache the result, rather than to calculate it  
## repeatedly.The functions below will create a matrix that can cache
## its inverse; then, calculate the inverse of said matrix or retrieve
## the inverse from the cache, if it has already been calculated (and
## the matrix hasn't been changed).

## This function creates a special "matrix" object that can cache its
## inverse.

makeCacheMatrix <- function(x = matrix()) {
  invx <- NULL
  set <-function(y) {
    x <<- y
    invx <<- NULL
}
get <- function() x
setInv <- function(inverse) invx <<- inverse
getInv <- function() invx
list(set=set, 
     get=get,
     setInv=setInv,
     getInv=getInv)
}

## This function calculates the inverse of the matrix returned from 
## makeCacheMatrix (above). If the inverse has already been calculated
## (and the matrix has not been changed), then cacheSolve will retrieve
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invx <- x$getInv()
  if(!is.null(invx)) {
    message("getting cached data")
    return(invx)
  }
  data <- x$get()
  invx <- solve(data, ...)
  x$setInv(invx)
  invx
}
