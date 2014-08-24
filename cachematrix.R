## The makeCacheMatrix function is creating a "matrix" object that can cache its
## iniverse matrix while the cacheSolve function computes the inverse of the "matrix" 
## returned above.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated
##(and the matrix has not changed), then cacheSolve should retrieve 
##the inverse from the cache.

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  } ## Return a matrix that is the inverse of 'x'
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
