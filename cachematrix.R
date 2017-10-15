## Matrix inversion is usually a costly computation
## This pair of functions caches the inverse of a matrix, so it is no compute repeatedly


makeCacheMatrix <- function(x = matrix()) {
## Create a special "matrix" object that can cache its inverse

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



cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
## If it has already been calculated, the result is retrieve from the cache
  
  i <- x$getinverse()
  
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}