# running  two functions 'makeCacheMatrix' & 'cacheSolve' in order to
#Cache the the Inverse of a Matrix

## The first function is makeCacheMatrix. This function creates a special "matrix" object that can cache its inverse.
### the outcome will be a list ( a sepcial matrix) that catche its inverse with four elemets
### set: set the value of the matrix
### get: get the value of the matrix
### setinvmtx: set the value of the invmtx
### getinvmtx: get the value of the invmtx
makeCacheMatrix <- function(x = numeric() ) {
  invmtx <- NULL
  set <- function(y) {
    x <<- y
    invmtx <<- NULL
  }
  get <- function() x
  setinvmtx <- function(solve) invmtx <<- solve
  getinvmtx <- function() invmtx
  list(set = set, get = get,
       setinvmtx = setinvmtx,
       getinvmtx = getinvmtx)
}


##  The cacheSolve: computes the inverse of the special "matrix" returned by makeCacheMatrix
#above. If the inverse has already been calculated (and the matrix has not changed), 
#then cacheSolve should retrieve the inverse from the cache.

#Note:Computing the inverse of a square matrix can be done with the solve function in R. 
#For example, if X is a square invertible matrix, then solve(X) returns its inverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invmtx <- x$getinvmtx()
  if(!is.null(invmtx)) {
    message("getting cached data")
    return(invmtx)
  }
  data <- x$get()
  invmtx <- solve(data, ...)
  x$setinvmtx(invmtx)
  invmtx
}       


