## These functions can be used to create a special matrix object that can have
## its inverse cached. Because taking an inverse is computationally expensive,
## caching the result will save a lot of time for larger matrices.

## Creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolved <- function(solved) s <<- solved
  getsolved <- function() s
  list(set = set, get = get,
       setsolved = setsolved,
       getsolved = getsolved)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated, a cached result is returned.
cacheSolve <- function(x, ...) {
  s <- x$getsolved()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolved(s)
  s
}
