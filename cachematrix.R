## My solution to Programming Assignment 2 in R Programming course:
## a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  # define set function
  set <- function(y){
    x <<- y
    m <-- NULL
  }
  
  # define get function
  get <- function(){
    x
  }
  
  # define setInverse function
  setInverse <- function(inv){
    inverse <<- inv
  }
  
  # define getInverse function
  getInverse <- function(){
    inverse
  }
  
  # list all defined methods
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inverseMatrix <- x$getInverse()
  
  # if the inverse of x is already calculated - return it
  if(!is.null(inverseMatrix)) {
    message("inverse matrix is already calculated - getting cached data")
    return(inverseMatrix)
  }
  
  # if the inverse matrix is not yet calculated, calculate it and return value
  message("calculating inverse of matrix")
  originalMatrix = x$get()
  inverseMatrix <- solve(originalMatrix)
  x$setInverse(inverseMatrix)
  inverseMatrix
}


#A <- matrix(c(2,5,1,3), nrows=2, ncols=2, byrow=TRUE)
#MA <-

