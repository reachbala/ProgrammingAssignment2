## https://github.com/reachbala/ProgrammingAssignment2.git
## Programming Assignment 2 : Lexical Scoping

## The function makeCacheMatrix creates a special "matrix" 
## object that can cache its inverse
## We can set and get the value of the matrix
## We can set and get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  k <- NULL
  set <- function(y) {
    x <<- y
    k <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) k <<- inverse
  getinv <- function() k
  list(
    set = set,
    get = get,
    setinv = setinv,
    getinv = getinv)
  
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.
## Computing the inverse of a square matrix can be done with the solve function in R.
## For example, if X is a square invertible matrix, then solve(X) returns its inverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  k <- x$getinv()
  if(!is.null(k)) {
    message("getting cached data")
    return(k)
  }
  data <- x$get()
  k <- solve(data, ...)
  x$setinv(k)
  k
}

