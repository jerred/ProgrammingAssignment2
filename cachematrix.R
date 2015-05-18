## This file contains two functions: one for creating a "matrix" container,
## which includes accessors for the matrix itself and its inverse, as well as
## another function to calculate, store, and retrieve the cached inverse.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## It creates a special "vector", which is really a list containing 4 functions
## that:
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) set the value of the matrix's inverse
## 4) get the value of the matrix's inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve calculates the inverse of the special "matrix" created with
## makeCacheMatrix, and caches the inverse.  If the mean has already been
## calculated, the cached value is returned and the calculation is skipped.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return (inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
