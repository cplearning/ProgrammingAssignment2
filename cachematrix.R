## File created by cplearning on 2017-10-31
## The functions calculate the inverse of a matrix and cache teh results for the next use

## makeCacheMatrix creates a list of functions that store and retrieve the matrix 'x'. 
## When used with the companion function cachSolve to calculate the matrix inverse, 
## it supports storing and retrieving the value of the inverse 

makeCacheMatrix <- function(x = matrix()) {

  dim_x <- dim(x)
  
  if(dim_x[1] != dim_x[2]) {
    message("sorry, not a square matrix")
    return(NULL)}
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve receives as input 'x', a list of functions created with makeCacheMatrix and calculates and stores
## the inverse of the matrix in x. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}
