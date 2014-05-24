## The following function creates a special "matrix", 
## object that can cache its inverse.
## It is return a list containing the functions
## - set the value of the matrix
## - get the value of the matrix
## - set the value of the inverse
## - get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  # initialize the stored inverse value to NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # set the value of the matrix
  get <- function() x
  # get the value of the matrix
  setinverse <- function(inverse) inv <<- inverse
  # set the inverse
  getinverse <- function() inv
  # get the inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
 }


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated 
## (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    # check whether the inverse has been cached or not
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
  }
  data <- x$get()
  # get the matrix into data
  inv <- solve(data, ...)
  # compute the inverse
  x$setinverse(inv)
  # cache the inverse
  inv
 }
