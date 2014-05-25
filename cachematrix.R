## File: cachematrix.R
## Description: 
##  This file allows the user to create a matrix, and the 
##  inverse of that matrix is cache-able.  
##
## Rational: 
##  For a very large matrix, it may take too long to compute the inverse,
##  especially if the inverse has to be computed repeatedly (e.g. in a loop).
##  If the contents of a matrix are not changing, it may make sense to cache
##  the inverse of the matrix so that when it is needed again, it can be looked
##  up in the cache rather than recomputed.
##
## Example Usage:
##  amatrix <- matrix(c(4, 3, 3, 2), nrow=2, ncol=2)
##  mc <- makeCacheMatrix(amatrix)
##  mcinverse <- cacheSolve(mc)


## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Calculates the inverse of the special "matrix" created with 
## the makeCacheMatrix function. However, it first checks to see if the
## inverse has already been calculated. If so, it gets the inverse from
## the cache and skips the computation. Otherwise, it calculates the inverse of
## the matrix and sets the value of the inverse
## in the cache via the setinverse function

cacheSolve <- function(x, ...) {
  
  m <- x$getinverse()
  ## if m is not null, then return the stored inverse, and display message
  ## stating that the cached matrix is being returned.
  if(!is.null(m)) {
    message("getting cached matrix")
    return(m)
  }

  ## if we've made it this far, then use the matrix data provided,
  ## get the inverse of that data using 'solve', store the inverse,
  ## and return the inverse. 
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  
  ## Return a matrix that is the inverse of 'x'
  m
}
