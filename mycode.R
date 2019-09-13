## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This particular function creates the matrix which can be inversely cached

makecachematrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## Write a short comment describing this function
## This function inversely caches the matrix (x)

cachesolve <- function(x, ...) {
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinverse(inv)
  inv
}


#testing purposes, setting mat1 as matrix and then inversing said matrix
r=rnorm(100)
mat1=matrix(r,nrow=10,ncol=10)
mat1
testing <- makecachematrix(mat1)
cachesolve(testing)