# makeCacheMatrix creates a list containing a function to
# 1. set,get the value of the matrix
# 2. set,get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}
# The following function returns the inverse of the matrix.it checks if the inverse is already 
# calculated. if the inverse value s not null, return the previously existing value otherwise calculate and return the inverse

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv #returning the inverse
}