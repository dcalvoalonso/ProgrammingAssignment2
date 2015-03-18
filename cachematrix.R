makeCacheMatrix <- function(x = matrix()) {
  #  This function creates a special "matrix" object that can cache its inverse
  #
  # Args:
  #   x: Matrix that is used to initialize the special matrix object
  #
  # Returns:
  #   A list containing a functions to:
  #   1. Set the value of the matrix
  #   2. Get the value of the matrix
  #   3. Get the value of the inverse matrix
  #   4. Set the value of the inverse matrix
  
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


cacheSolve <- function(x, ...) {
  #  This function returns a matrix that is the inverse of 'x'. It the inverse has been calculated, we return the cached object
  #
  # Args:
  #   x: Special matrix object that caches the inverse matrix
  #
  # Returns:
  #   Inverse matrix of x
  ## 
  
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
