##
## Create a matrix and cache its inverse
##
## Assumes the matrix is invertible so no checks for this are made
## 

##
## makeCacheMatrix
##
## Description:  
## Creates a special "matrix" object that can cache its inverse
##
## Notes:
## set         set the values of the matrix
## get         get the values of the matrix
## setinverse  set the value of the inverse
## getinverse  get the value of the inverse
##
## History:
## Initial version Feb 2016 (DPF) 
##
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


## cachesolve
##
## Description:
## Computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed) cachesolve skips 
## the calculation and retrieves the inverse from the cache.
## The inverse of the matrix is calculated using the solve() function
##
## Notes:
##
## History:
## Initial Version Feb 2016 (DPF)
##
cachesolve <- function(x, ...) {
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

