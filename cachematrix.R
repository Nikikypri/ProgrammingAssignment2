## Matrix inversion is usually a costly computation.  Rather than computing it repeatedly, you can
## take advantage of the scoping rules of the R language and how they can be manipulated to preserve 
## state inside of an R object.  This way the inverse of the matrix can be looked up in the cache rather than recomputed

## This function creates a special "matrix" object and caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
  invmat <- NULL
  set <- function(y){
    x <<- y
    invmat <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) invmat <<- solve
  getinverse <-function() invmat
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve retrieves 
## the inverse from the cache.  Otherwise it computes the invers and caches it.

cacheSolve <- function(x, ...) {
  invmat <-x$getinverse()
  ## Check if the inverse has been calculated and retrieve it from the cache with a message.
  if(!is.null(invmat)) {
    message("getting cached data")
    return(invmat)
  }
  data <- x$get()
  invmat <- solve(data,...)
  x$setinverse(invmat)
  invmat
}