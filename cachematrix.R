## Caches the inverse of a matrix and can use the cache for further computations.

## This function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  a <- NULL
  set <- function(y) {
    x <<- y
    a <<- NULL
  }
  get <- function() x
  setinv <- function(solve) a <<- solve
  getinv <- function() a
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the matrix by taking the object returned by makeCacheMatrix as its arguments.
## If the inverse has already been calculated, then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  a <- x$getinv()
  if(!is.null(a)) {
    message("getting cached data")
    return(a)
  }
  data <- x$get()
  a <- solve(data, ...)
  x$setinv(a)
  a
        ## Return a matrix that is the inverse of 'x'
}

