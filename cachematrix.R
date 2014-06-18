## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  # if the matrix isn't square load the MASS package
  # and use Moore-Penrose generalize inverse. 
  if ( dim(data)[1] != dim(data)[2])
  {
    library(MASS)
    m <- ginv(data, ...)
  }
  # otherwise use solve 
  else
  {
    m <- solve(data, ...)
  }
  x$setinverse(m)
  m
}
