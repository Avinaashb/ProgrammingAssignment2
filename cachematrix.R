## makeCacheMatrix-- This function creates a special "matrix" 
##object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
  minv <- NULL
  set <- function(y) {
    x <<- y
    minv <<- NULL
   }
  get <- function() x
  setminv <- function(computedminv) minv <<- computedminv
  getminv <- function() minv
  list(set = set, get = get,
       setminv = setminv,
       getminv = getminv)
}


## cacheSolve -- This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve 
## will retrieve the inverse from the cache.



cacheSolve <- function(x, ...) {
    minv <- x$getminv()
  ##check to see if the data already exists 
  ##if so this will return the existing data already stored as minv
  if(!is.null(minv)) {
    message("using data from cache")
    return(minv)
  }
  #solves for the data and sets it as minv
  data <- x$get()
  minv <- solve(data, ...)
  x$setminv(minv)
  minv
}