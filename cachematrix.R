## A pair of functions that create a cache of a matrix and subsequently
## store and return the inverse.

## This function creates the matrix which will be used for its
## inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv
  list(set = set, 
       get = get, 
       setInverse = SetInverse, 
       getInverse = getInverse)
}


## This function solves the earlier matrix and produces the inverse matrix.

cacheSolve <- function(x, ...) {
     inv <- x$getInverse()
     if(!is.null(inv)){
       return(inv)
     }
     data <- x$get()
     inv <- solve(data)
     x$setInverse(inv)
     inv
}
