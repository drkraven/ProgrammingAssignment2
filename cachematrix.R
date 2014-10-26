## These functions create a special "matrix" object that can computes and caches its inverse.

##creates a special "matrix", which contains functions to:
## set the value of the matrix
## get the value of the matrix
## set the inverse of the matrix
## get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inv  <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## Computes the inverse of the special "matrix" returned by makeCacheMatrix if there is no inverse cached
cacheSolve <- function(x, ...) {
  
  invM<- x$getinv()
  if(!is.null(invM)) {
    ##"getting cached data"
    return(invM)
  }
  M <- x$get()
  invM <- solve(M)
  
  ## set the newly calcuated inverser of matrix
  x$setinv(invM)
  ## Return a matrix that is the inverse of 'x'
  invM
}
