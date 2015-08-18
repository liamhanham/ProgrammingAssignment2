## Matrix inversion is usually a costly computation and there 
## may be some benefit to caching the inverse of a matrix rather 
## than compute it repeatedly. The functions below wil cache the 
## inverse of a matrix.

## The 'makeCacheMatrix' creates a function that gives us the tools
## to cache a matrix by adding the functions of set, get, setinverse,
## and getinverse to the matrix. These functions will be used by the 
## cacheSolve function to determine if there is a version of called
## matrix, 'newGlobalMatrix', in cache.

makeCacheMatrix <- function(newMatrix = matrix()) {

     inverse <- NULL
     set <- function(newMatrix) {
          newMatrix <<- newMatrix
          inverse <<- NULL
     }
     get <- function() newMatrix
     setinverse <- function(inverseOfMatrix) inverse <<- inverseOfMatrix
     getinverse <- function() inverse
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
     
}

## Write a short comment describing this function

cacheSolve <- function(matrix, ...) {

     inverse <- matrix$getinverse()
     if(!is.null(inverse)) {
          message("getting cached data")
          return(inverse)
     }
     data <- matrix$get()
     inverse <- solve(data, ...)
     matrix$setinverse(inverse)
     inverse

}
