## Overall comments: I used the example functions cacheing the mean of the vector into the assignment 

## In the makeCachematrix function, you first
##Set the value of the matrix
##get the value of the matrix
##set the value of the inverse
##get the value of theinverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    X <<- y
    inv <<- NULL
  }
get <- function()x
setinverse <- function(solve) inv <<- solve
getinverse <- function () inv
list (set = set, get =get,setinverse=setinverse,getinverse=getinverse)

}

## The following function calculates the inverse of a matrix, first it checks if this has already 
## been done 
       

cacheSolve <- function(x, ...) {
  inv <-x$getinverse()
  if(!is.null(inv)) {
    message ("getting cached data")
    return (inv)
    
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
  }
                                
}


