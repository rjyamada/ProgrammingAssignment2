## cachematrix.R
## This file contains a pair of functions, makeCacheMatrix
## and cacheSolve, that caches the result of inverse matrix
## computation.  Caching can help improve code performance
## by avoiding repeatedly performing expensive operations.

## makeCacheMatrix creates a special "matrix" with following
## functions:
## 1. set the value of the vector
## 2. get the value of the vector
## 3. set the value of the solve
## 4. get the value of the solve

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
          x <<- y
          s <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) s <<- solve
  getSolve <- function() s
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}

## cacheSolve calculates the inverse of the special "matrix"
## created by makeCacheMatrix. However, it first checks to see
## if the inverse has already been calculated. If so, it gets
## the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix and sets
## the value of the matrix in the cache via the setSolve function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getSolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setSolve(s)
  s
}
