## This function stores a matrix object and its inverse.
## It has 4 functions to get and set the matrix and the inverse.
## get() returns the matrix
## set(x) saves the matrix x in the object
## getinverse() returns the inverse if one is set
## setinverse(i) saves i as the inverse 
install.packages("MASS")
library(MASS)
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function takes the matrix object created in makeCacheMatrix and if an 
## inverse is already stored it returns that inverse
## If no inverse is stored then it calculates the inverse of the 
## matrix stored in x, stores it and returns it

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached inverse")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}
