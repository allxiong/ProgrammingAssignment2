###############################################################
# JHU Data Science R programming week 3 assigment
# Author: Allie Xiong
###############################################################

##############################################################
# Function name: makeCacheMatrix
# This function creates a special "matrix" object that can cache its inverse.
# This function does the following:
# 1 set the value of the matrix
# 2 get the value of the matrix
# 3 set the value of the Inverse 
# 4 get the value of the Inverse 
#
###############################################################

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


#########################################################################
# cacheSolve: This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. It first checks to see if the inverse 
# has already been calculated. If the inverse has already been 
# calculated (and the matrix has not changed), then the cachesolve 
# should retrieve the inverse from the cache. Otherwise, it calculates 
# the inverse of the matrix and sets the value of the inverse in the cache 
# via the setinverse function.
#########################################################################

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inverse)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}