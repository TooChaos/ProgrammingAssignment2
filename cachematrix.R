## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# a funciton create a matrix with the cache of its inverse
# This function create a special "matrix", which contains a function to 
# set the value of the matrix
# get the value of the matrix
# set the inverse of the matrix
# get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse_mat <- matrix()
  
  set <- function(y) {
    x <<-y
    inverse_mat <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inv_mat) {
    inverse_mat <<- inv_mat
  }
  
  getInverse <- function() {inverse_mat}
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}


## Write a short comment describing this function
# find the inverse of the matrix: if it is cached, return the 
# cached, if not, calculate the inverse and return.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inverse <- x$getInverse()
  
  if (inverse == matrix()) {
    inverse <- solve(x$get(), ...)
    x$setInverse(inverse)
  }
  else {
    inverse <- x$getInverse()
  }
  
  inverse
    
}
