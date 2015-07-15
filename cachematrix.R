## Finding the inverse of a matrix is a costly operation.
## That is why we will store it in the cache, so it can be
## looked up later.
## Scoping rules of R will help us with that.

## makeCacheMatrix allows to 
## 1. set/get a matrix
## 2. set/get the inverse of it

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse_value) inv <<- inverse_value
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve checks if the inverse already exists in cache
## if not, the function calculates "inv" and stores it
## using  the "setinv" function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
   
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
