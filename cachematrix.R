## This script provides two functions: the first (makeCacheMatrix) creates a matrix object which can cache its inverse.
## The second function (cacheSolve) computes the inverse of the matrix object returned by the first function, but first checks
## to see if the inverse has already been cached. If so, the cached value is returned, saving computational resources.

## makeCacheMatrix is a function which creates a special 'matrix' object which can cache its inverse

makeCacheMatrix <- function (x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## cacheSolve computes the inverse of the special matrix created by the above function.
## If the inverse has alreay been calculated, then cacheSolve should retrieve from cache.

cacheSolve <- function (x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}