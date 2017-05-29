## This file contains two functions that will aid in caching and 
## retrieving a matrix and its inverse into and from a different enviroment
## than in which functions are described. Matrix inversion is usually a costly 
## computation and there may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly when matrix value is not changing over time.

## makeCacheMatrixfuction creates a special matrix and stores
## the matix and its inverse into cached enviroment and returns 
## a list of fuctions to set and retreive the matrix and its inverse

makeCacheMatrix <- function(x = matrix()){
  inverse_of_sample_matrix <- NULL
  set <- function(y){
    x <<- y
    inverse_of_sample_matrix <<- NULL
  }
  get <- function(){x}
  setInverse <- function(inv_of_sample_mat){inverse_of_sample_matrix <<- inv_of_sample_mat}
  getInverse <- function(){inverse_of_sample_matrix}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## The following cacheSolve function calculates the inverse of the special matrix 
## created with the above function. However, it first checks to see if the inverse
## has already been calculated. If so, it gets the inverse from the cache and skips
## the computation. Otherwise, it calculates the inverse of the matrix and sets the
## value of the inverse in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
  ## Input: list of functions returned by func makeCacheMatrix 
  ## Returns a matrix that is the inverse of stored matrix.
  inverse <- x$getInverse()
  if (!is.null(inverse)) {
    ## Inverse of matrix exists in cache. Get from Cache
    message("getting cached data")
    return(inverse)
  }
  storedMatrix <- x$get()
  inverse <- solve(storedMatrix)
  x$setInverse(inverse, ...)
  inverse
}
