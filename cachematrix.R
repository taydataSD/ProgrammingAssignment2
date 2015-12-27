## This function below will effectively create a matrix as well as solve and cache the same matrix. 
## Using this function "cacheSolve" we can call the original function from its previously 
## coded solution. 

## makeCacheMatrix specifically should output 4 items in a list when called. The functions were 
## mimicked from the examples given in the project discription. The functions nested within 
## makeCacheMatrix are get, set, getInverse, and setInverse. This function can be tested by calling
## makeCacheMatrix, you need not call any of the nested functions. This will cache the inverse of 
## the origina matrix

makeCacheMatrix <- function(x=matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}



## The first function above creates the matrix to be cached. The function below will then
## calculate a matrix that is the inverse of x. This solves the object that was created by the first
## function and will return a matrix that is the inverse of 'x'. 

cacheSolve <- function(x, ...) {
  solve <- x$getInverse()
  if(!is.null(solve)) {
    message ("was solved using cache")
    return(solve)
  }
  mat <- x$get()
  solve <- solve(mat, ...)
  x$setInverse(solve)
  solve
}

