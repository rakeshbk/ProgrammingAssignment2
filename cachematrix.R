## makeCacheMatrix - create a matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
# x: matrix input
# return list to get, set matrix and inverse
  
  inv = NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get = function() x
  setinv = function(inverse) inv <<- inverse
  getinv = function() inv
  list(set=set, get = get, setinv = setinv, getinv = getinv)

}


## cacheSolve - returns inverse of original matrix input to makeCacheMatrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv = x$getinv()
  
  # Inverse already done?
  
  if (!is.null(inv)){
    message("returning cache data")
    return(inv)
    
  }
  
  # Calculate inverse
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  x$setinv(inv)
  
  return(inv)
}

