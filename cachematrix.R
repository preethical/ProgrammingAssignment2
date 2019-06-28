## Overall comments: I used the example functions from cacheing the mean of the vector 
##into this assignment. It can be done with the function solve or ginv (from the MASS Packages)

##The cachematrix.R file contains two functions, makecachematrix and cachesolve. 
##The first function in the file, creates an R object that stores 
##a matrix and its inverse. The second function, cachesolve requires an argument 
##that is returned by makecachematrix so as to return the inverse of the matrix from the cache
##that is stored in the makecachematrix's environment.

## In the makeCachematrix function, you first
##Set the value of the matrix
##get the value of the matrix
##set the value of the inverse
##get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  
  if (ncol(x)==nrow(x)) {
    inv <- NULL                                   ## sets placeholder for future values
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }                                            ## defines the function to set a matrix x to a new matrix y and sets inverse as null
    get <- function() x                             ## returns the matrix x
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
       

cacheSolve <- function(x, ...) {  ##takes the output of the makecache matrix
  inv <-x$getinverse()
  if(!is.null(inv)) {             ## if the inverse has already been calculated it takes it from cache
    message ("getting cached inverted matrix")
    return(inv)
  }
  else {
    data <- x$get()
    inv <- solve(data, ...)    ##if not it calculates the inverse here using the solve function
    x$setinverse(inv)
    return(inv)
  }
}
