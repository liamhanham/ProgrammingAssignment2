## Matrix inversion is usually a costly computation and there 
## may be some benefit to caching the inverse of a matrix rather 
## than compute it repeatedly. The functions below wil cache the 
## inverse of a matrix.

## The 'makeCacheMatrix' creates a set of functions that gives us the tools
## to cache a matrix by adding the functions of set, get, setinverse,
## and getinverse to the matrix. These functions will be used by the 
## cacheSolve function to determine if there is a version of called
## matrix, 'newGlobalMatrix', in cache. This is made works because of lexical
## scoping.

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

## The cacheSolve function looks in memory, using the getinverse function to
## if we have cached a result of the inverse of the passed 'matrix' in memory. 
## If we have not then we calculate the inverse and store the result in memory.

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
