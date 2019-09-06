## Here is an implementation of cahing of inverted matrix

## makeCacheMatrix function creates a cache object which
## stores the matrix and its inversion and returns these matrices

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inv_arg) inv <<- inv_arg
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve function expects a cached object returned by the makeCacheMatrix function,
## tries to get cached value and if it does not exists,
## inverts the matrix and puts it into the cache to get values from cache for subsequent calls

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
