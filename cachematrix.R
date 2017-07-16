## The following functions together operate on square matrices
## in order to capture and invert it. 

## makeCacheMatrix() operates by calling the function as an
## assignment, for example:
##    test_matrix$set(): sets the values of test_matrix
##    test_matrix$get(): returns the value of the matrix
##    test_matrix$setinverse(): calculates the inverse of 
##      test_matrix
##    test_matrix$getinverse(): returns the previously
##      calculated inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    if (sqrt(length(y))%%1==0){
      x <<- matrix(y, nrow =sqrt(length(y)), ncol = sqrt(length(y)))
      m <<- NULL
    } else
      print("Matrix would not be square, please enter a list of length 1, 4, 9, 16, etc.")
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve() calculates the value of the provided matrix,
##  this matrix must be created using the makeCacheMatrix()
##  function

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)){
    message("Getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}