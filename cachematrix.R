## Put comments here that give an overall description of what your
## functions do:
##---The function is designed to cache the inverse of a invertible matrix.--

## Write a short comment describing this function: ---Caching the inverse matrix instead of computing it repeatedly will be beneficial
## The following function generates a new "matrix" object that can cache its inverse.----

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Write a short comment describing this function:
## --This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above.It first checks if
## the inverse has already been computed. If so, it gets the result.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
## Test Result
##> mymatrix <- makeCacheMatrix(matrix(1:4, 2, 2))
##> mymatrix$get()
##[,1] [,2]
##[1,]    1    3
##[2,]    2    4
##> cacheSolve(mymatrix)
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5