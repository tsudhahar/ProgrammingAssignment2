# Assignment: Caching the Inverse of a Matrix
# Purpose: Matrix inversion is usually a costly computation.
# There may be some benefit to caching the inverse of a matrix rather than compute it repeatedly 
# Solution: Write a pair of functions that cache the inverse of a matrix
# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

# makeCacheMatrix:
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  # 1. set the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # 2. get the value of the matrix
  get <- function() x
  # 3. set the value of inverse of the matrix
  setinverse <- function(inverse) inv <<- inverse
  # 4. get the value of inverse of the matrix
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}




# cacheSolve: This function assumes that the matrix is always invertible.
# The following function calculates the "inverse" of the matrix. However, it first checks if
# the inverse has already been calculated. If so, it gets the matrix and skips the
# computation. Otherwise, it computes the inverse, sets the value in the cache via
# setinverse function.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("Getting from cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

## dry run to demo the cache:
## > var1 = rbind(c(1, -1/4), c(-1/4, 1))
## > vCacheMatrix = makeCacheMatrix(var1)
## > m$get()
##       [,1]  [,2]
## [1,]  1.00 -0.25
## [2,] -0.25  1.00

## No cache in the first run
## > cacheSolve(vCacheMatrix)
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667

## Retrieving from the cache in the second run
## > cacheSolve(vCacheMatrix)
## getting cached data.
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
## > 
