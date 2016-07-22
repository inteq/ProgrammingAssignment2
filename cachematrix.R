## Put comments here that give an overall description of what your
## functions do

## makeCacheMatix creates a special "matrix", which is really a list containing functions that:
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of the matrix
## get the value of the inverse of the matrix

makeCacheMatrix <- function(mtrx = matrix()) {
  inv <- NULL
  set <- function(a_matrix) {
    mtrx<<- a_matrix
    inv<<- NULL
  }
  get <- function() { 
    mtrx
  }
  setInverse <- function(inverse) {
    inv <<- inverse
  }
    
  getInverse <- function() {
    inv
  }
      
  list(set=set,get=get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## cacheSolve takes a special "matrix" and then returns its existing
## calculated inverse or calculates the inverse and then caches the result
## before returning it to the caller 
cacheSolve <- function(mtrx, ...) {
  ## Return a matrix that is the inverse of 'mtrx'
  inv <- mtrx$getInverse()
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }

  data <- mtrx$get()
  message("calculating inverse")
  inv <- solve(data, ...)
  message("caching inverse")
  mtrx$setInverse(inv)
  inv
}
