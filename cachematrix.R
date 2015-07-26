## Karl D. Riddett for Coursera R - July Class
## These functions are for Programming Assignment 2
## Due 7/26/15

## This function takes in a matrix x, stores it into "m" for caching
##  Calculates the inverse and returns it

makeCacheMatrix <- function(x = matrix()) {
  ## initialize M
  m <- NULL
  
  ##cache
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## set up for lexical scoping
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
  ## Check if exists
    if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## Get the matrix data
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
