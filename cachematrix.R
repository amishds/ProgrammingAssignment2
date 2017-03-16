## This file contains functions to save computation time
## to inverse matrix when we need inversion of matrix multiple times.

## Write a short comment describing this function
## This function takes matrix as an input and
## provides various functions to set/get matrix itself
## as well as setinverse/getinverse to return inverse of the 
## matrix which is already computed.
makeCacheMatrix <- function(x = matrix()) {
   inverse <- NULL

   set <- function (y) { 
      x <<- y
      inverse <<- NULL
   }

   get <- function () x 

   getinverse <- function() inverse 
   setinverse <- function(my_invserse) inverse <<- my_inverse

   list(set = set, get = get, 
        setinverse = setinverse, getinverse = getinverse)

}


## Write a short comment describing this function
# This function is used to retrieve
cacheSolve <- function(x, ...) {

   ## Return a matrix that is the inverse of 'x'
   inverse <- x$getinverse()
   if(!is.null(inverse)){
      return(inverse)
   }

   ## If inverse of the matrix is not calculated,
   ## then compute the same and cache it.
   data <- x$get()
   inverse <- solve(data)
   x$setinverse(inverse) 
   inverse
}

