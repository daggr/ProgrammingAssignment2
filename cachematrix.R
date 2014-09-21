## Answers to R programming assignment 2

## Function to create a special matrix which can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list( set = set, get = get, setinv = setinv, getinv = getinv)
}

## Function to return a matrix inverse.  If inverse of x has 
## already been computed a cached value will be returned.
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if( !is.null(inv) ) {
    return(inv)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinv(inverse)
  inverse
}
