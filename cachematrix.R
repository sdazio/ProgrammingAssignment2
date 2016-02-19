#
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## The function makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  ## An object to store the inverse matrix  
  invMatrix = NULL
  set = function(y) {
    x <<- y
    invMatrix <<- NULL
  }
  get = function() x
  setinv = function(inv) invMatrix <<- inv 
  getinv = function() invMatrix
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## cacheSolve: This function computes the inverse of the special "matrix" 
##             returned by makeCacheMatrix above. 
##             If the inverse has already been calculated (and the matrix has not changed), 
##             then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv = x$getinv()
  
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  mat = x$get()
  inv = solve(mat, ...)
  x$setinv(inv)
  return(inv)
}