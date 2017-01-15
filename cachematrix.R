## makeCachematrix sets or reads a matrix or inverse of a matrix
## Cachesolve returns inverse of matrix created with makeCachematrix if it exists
## otherwise cachesolve calculates the inverse of a matrix


## Creates a matrix object whose original or inverse can be retrieved or set
## x$get will tell you current value of matrix; x$set will set value of matrix x
## x$getinv: return current value of x's inverse; x$setinv will invert x

makeCacheMatrix <- function(x = matrix()) {
 
  invmatrix = NULL
  set = function(y) {
    x <<- y
    invmatrix <<- NULL
  }
  get = function() x
  setinv = function(inverse) invmatrix <<- inverse 
  getinv = function() invmatrix
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}



## Outputs inverse of a special matrix if it is not already in cache
## input is a matrix created using makeCacheMatrix prior to using cacheSolve

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  cachedinv = x$getinv()
  
  # get inverse from cache if it already exists
  if (!is.null(cachedinv)){
    message("retrieving inverse from cache")
    return(cachedinv)
  }
  
  # else, invert the passed matrix x
  passedmatrix = x$get()
  cachedinv = solve(passedmatrix, ...)
  
  x$setinv(cachedinv)
  
  # return inverse of matrix x
  return(cachedinv)
}
