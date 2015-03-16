## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  cache <- NULL
  setMatrix <- function(y) {
    x <<- y
    cache <<- NULL
  }
  getMatrix <- function() x
  cacheInverse <- function(inv) {
    cache <<- inv
  }
  getInverse <- function() cache
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       cacheInverse = cacheInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverted <- x$getInverse()
  if(!is.null(inverted)) {
    message("getting cached data")
    return(inverted)
  }
  data <- x$getMatrix()
  inv <- solve(data)
  x$cacheInverse(inv)
  inv
}
