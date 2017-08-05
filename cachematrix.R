## These functions are used to store a matrix, and solve and cache its inverse, within one object

## Creates a 'matrix' object which can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Takes a makeCacheMatrix object, computes the inverse of the matrix and caches it
## If makeCacheMatrix object already holds a cached inverse, returns the cached inverse

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)) {
    message("Getting cached data...")
    return(i)
  }
  
  data <- x$get()
  i <- solve(data, ...)
  
  x$setInverse(i)
  i
}
