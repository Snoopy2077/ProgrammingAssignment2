## These two function let you compute the inverse of a matrix
## If there is a cached result for the matrix supplied as an argument, then no new computation will occur

## This creates a special matrix and returns a list of set and get functions for
## the original matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## this function queries the cache if an inverse for this function already exists and
## computes the inverse if not

cacheSolve <- function(x, ...) {
  print(x)
  m <- x[["getinverse"]]()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}