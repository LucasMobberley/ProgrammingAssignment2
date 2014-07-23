## Take invertable square matrix input. Cache matrix and set m to NULL.
## Check if m is NULL, if true caclulate and cache inverse of input, if
## false retrun cached value of m.

## Takes a square invertable matrix and creates an object whose inverse can
## be cached.

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



## Checks for a cached value of m.  If m == NULL the inverse of x is
## calculated else the cached value of m is returned.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
