## The two functions will ultimately cache the inverse of a matrix, assuming that the matrix given will be invertible.

## The makeCacheMatrix creates a matrix "object," which allows the following function to inverse the given matrix.

makeCacheMatrix <- function(x = matrix()) {
  Inv <- NULL
  set <- function(j) {
    x <<- j
    Inv <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) Inv <<- inverse
  getInv <- function() Inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## The cacheSolve function computes the inverse of the given matrix, the output of the makeCacheMatrix function. If the inverse has already been calculated, then the cacheSolve should retrieve the existing inverse from the cache.

cacheSolve <- function(x, ...) {
  Inv <- x$getInv()
  if(!is.null(Inv)) {
    message("getting cached data")
    return(Inv)
  }
  data <- x$get()
  Inv <- solve(data, ...)
  x$setInv(Inv)
  Inv
}
