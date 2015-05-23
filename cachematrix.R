## function makeCacheMatrix creates a list containing four functions:
## 1. set() - set the matrix 
## 2. get(x) - get the matrix x
## 3. setinverse(inv) - set the inverse
## 4. getinverse() - get the inverse

## our assumption is that x will always be an invertibile matrix

makeCacheMatrix <- function(x = matrix()) {
  
  inverse <- NULL
  
  # '<<-' is an operator used to assign a value to an object in an environment
  # that is different from the current environment
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}



## the input for cacheSolve function is the output of the 
## previous makeCacheMatrix function (list of four functions)

cacheSolve <- function(x, ...) {
  # get the inverse from cache
  inverse <- x$getinverse()
  
  # if the inverse has been calculated, return it
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  # else
  # get the matrix, calculate the inverse
  data <- x$get()
  inverse <- solve(data, ...)
  
  # cache the inverse
  x$setinverse(inverse)
  inverse
  
}
