## makeCacheMatrix and cacheSolve calculate the inverse matrix for a given invertible
## square matrix x and cache it, checking if the inverse matrix has already been stored in cache.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  setmatrix <- function(y) {
    x <<- y
    inverse <<- solve(x)
  }
  getmatrix <- function() x
  setinverse <- function(solve) inverse <<- solve
  getinverse <- function() inverse
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cachesolve retrieves the inverse from
## the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$getmatrix()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
