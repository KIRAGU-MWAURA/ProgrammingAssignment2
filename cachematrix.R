## Script creates two functions makeCacheMatrix and cacheSolve function
## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  Inver <- NULL ## Initializes an inverse matrix as null
  set <- function(y) {
    x <<- y
    Inver <<- NULL
  }
  get <- function() x ## Creates a matrix x
  setInverse <- function (inverse) Inver <<- inverse
  getInverse <- function() Inver  ## Retracts inverse of the matrix
  list (set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}

## cacheSolve function computes the inverse of the special "matrix" created by the makeC

##  If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {     ## Return a matrix that is the inverse of 'x'
  Inver <- x$getInver ()
  if (!is.null(Inver)) {         ## Determines if inverse is NULL
    message("Getting cached data")
    return(Inver)
  }
  interested_data <- x$get()
  Inver <- solve(interested_data, ...) ##computes the inverse value
  x$setInverse(Inver)
  Inver   ## Returns a matrix that is an inverse of matrix 'x'
}
