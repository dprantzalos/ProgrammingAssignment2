## Matrix inversion is usually a costly computation and there may be
## some benefit to caching the inverse of a matrix rather than computing
## it repeatedly. The following functions will compute the inverse of a
## matrix and cache the result for reuse.
##
## Example usage:
## source("cachematrix.R")
## m <- makeCacheMatrix(matrix(1:4, 2))
## m$get() # before inverse
## cacheSolve(m) # first time, no cache
## cacheSolve(m) # second time, cached data
##
## This code is based on Programming Assignment #2 from the Coursera
## R Programming MOOC. Further details are available at:
## https://github.com/rdpeng/ProgrammingAssignment2
##

## Using R scoping rules, you can cache the inverse of a matrix and avoid
## re-computing costs. This function creates a special "matrix", which is
## really a list containing a function to:
##
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the cached matrix
## 4. Get the value of the cached matrix
##
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setcache <- function(matrix) m <<- matrix
  getcache <- function() m
  list(set = set, get = get,
       setcache = setcache,
       getcache = getcache)
}

## This function computes the inverse of the special "matrix" created
## with the above function using the solve function in R. However, it
## first checks to see if the inverse matrix has already been calculated.
## If so, it gets the matrix from the cache and skips the computation.
## Otherwise, it calculates the inverse of the square matrix and sets
## the value of the matrix in the cache via the setcache function.
##
## Note that per assignment, this function assumes that the matrix
## supplied is always invertible.
##
cacheSolve <- function(x, ...) {
  m <- x$getcache()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## Return a matrix that is the inverse of 'x'
  data <- x$get()
  m <- solve(data, ...)
  x$setcache(m)
  m
}
