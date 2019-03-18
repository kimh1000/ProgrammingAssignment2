## Two functions are included in this file.  The first one assigns values to objects in an environment that is different from 
## the current environment and passes those objects to the second function as a list.

## This first function creates the objects in a different environment from the current environment using the <<- assignment operator

makeCacheMatrix <- function(x = matrix()) {
    im <-  NULL
  set <- function (y) {
          x <<- y
          im <<- NULL
  }
  get <-  function () x
  setinv <-  function(invm) im <<- invm
  getinv <- function() im
  list(set=set, get=get, setinv=setinv, getinv=getinv)


## Second function below takes the output of makeCacheMatrix, checks to see if the inverse matrix exists and if not, calculates and
## return its using the solve() funtion

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  im = x$getinv()
  if (!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  m.data <- x$get()
  im <- solve(m.data, ...)
  x$setinv(im)
  return(im)
}
