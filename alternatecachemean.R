require (MASS)
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  ## sets placeholder for future values
  set <- function(y = matrix()) {
    x <<- y
    inv <<- NULL
  }   ## defines the function to set a matrix x to a new matrix y and sets inverse as null
  get <- function()x  ## returns the matrix x
  setinverse <- function(inv) inv <<- inverse  ##sets the inverse of matrix to inv
  getinverse <- function()inv     ##returns the inverse
 list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}
cacheSolve <- function(x, ...) {
  inv <-x$getinverse()
  if(!is.null(inv)) {
    message ("getting cached data")
    return (inv)
    
    data <- x$get()
    inv <- ginv(data, ...)
    x$setinverse(inv)
    return (inv)
  }
}
