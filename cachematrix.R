## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  cacheMatrix <- NULL
  setMatrix <- function(y) {
    x <<- y
    cacheMatrix <<- NULL
  }
  getMatrix <- function() x
  setCache <- function(inverse) cacheMatrix <<- inverse
  getCache <- function() cacheMatrix
  list(setMatrix = setMatrix,
       getMatrix = getMatrix,
       setCache = setCache,
       getCache = getCache)
}

cacheSolve <- function(x, ...) {
  cacheMatrix <- x$getCache()
  if (!is.null(cacheMatrix)) {
    message("loading cache matrix...")
    return(cacheMatrix)
  }
  else {
    dMatrix <- x$getMatrix()
    cacheMatrix <- solve(dMatrix, ...)
    x$setCache(cacheMatrix)
    return(cacheMatrix)
  }
}
