## These functions are designed to demonstrate the scoping functionality of R by
## setting variables outside their normal context.

# Returns a list of functions to interact with a special matrix object
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


# Calculates the inverse of a matrix, and caches the calculation for later use. 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)){
    message("retreiving from cache")
    i
  }
  else {
    data <- x$get()
    i <- solve(data)
    x$setinv(i)
    i
  }
}
