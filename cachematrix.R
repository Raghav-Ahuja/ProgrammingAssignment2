## The function caches the inverse of a matrix to avoid recomputation. If it doesn't found the cache in one go, it computes it and saves it for future utility.
## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL
   set <- function(y) {
   x <<- y
   inv <<- NULL
   }
   get <- function()x
   setinv <- function(inverse) inv <<- inverse
   getinv <- function() inv
   list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}
   


# This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
