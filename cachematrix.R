## The functions declared here can be used to
##cache the inverse of an invertible matrix

## The makeCachematrix creates a list object which contains the list of functions
## to set and retrieve the matrix object and its inverses.

makeCacheMatrix <- function(m = matrix()) {
  inv <- NULL
  set <- function(y) {
    m <<- y
    inv <<- NULL
  }
  get <- function() m
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## The cachesolve funciton retrives the cached inverse, if not found it computes the inverse
## and creates a cache.

cacheSolve <- function(x) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  return(inv)
  
}

