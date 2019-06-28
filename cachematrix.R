## Overall comments: I used the example functions cacheing the mean of the vector into the assignment 

## In the makeCachematrix function, you first
##Set the value of the matrix
##get the value of the matrix
##set the value of the inverse
##get the value of theinverse

makeCacheMatrix <- function(x = matrix()) {
  
if(ncol(x)==nrow(x)){
  inv <- NULL                                   ## sets placeholder for future values
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }                                            ## defines the function to set a matrix x to a new matrix y and sets inverse as null
  get <- function()x                             ## returns the matrix x
  setinverse <- function(inverse) inv <<- inverse      ##sets the inverse of matrix to inv
  getinverse <- function() inv                  ##returns the inverse
  list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}
else {
  print("Please try again with an invertible matrix")
}  
}

## The following function calculates the inverse of a matrix, first it checks if this has already 
## been done 
       

cacheSolve <- function(x, ...) {
  inv <-x$getinverse()
  if(!is.null(inv)) {
    message ("getting cached inverted matrix")
    return(inv)
    
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    return(inv)
  }
}
