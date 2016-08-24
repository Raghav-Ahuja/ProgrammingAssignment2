## The function caches the inverse of a matrix to avoid recomputation. If it doesn't found the cache in one go, it computes it and saves it for future utility.

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
   


## Write a short comment describing this function

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
