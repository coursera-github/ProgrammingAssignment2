## Functions for computation of matrix inverse, with caching

## Usage:
# m <- makeCacheMatrix(matrix(c(5,23,8,-12,0,6,1,3,6), nrow=3, ncol=3))
# m$get()
# m$set(matrix(c(5,23,8,-12,0,6,1,3,7), nrow=3, ncol=3))
# cacheSolve(m)
# m$getinv()
# cacheSolve(m)
# m$getinv()

## makeCacheMatrix
# Given param x as a single matrix, provides a list of functions,
# including set, get, setinv, and getinv.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## cacheSolve
# Return a matrix that is the inverse of 'x'.
# Additional arguments to solve can be supplied through the dot args (...)

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
