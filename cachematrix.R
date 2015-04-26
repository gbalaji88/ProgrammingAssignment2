## The cachematrix script generates two functions that compute and store the inverse
## of a matrix. If the user attempts to calculate the inverse of the matrix for the 
## second time, instead of recalculating the inverse again, the stored value of the 
## inverse is displayed. If the user attempts to calculate the inverse of a different
## matrix, the stored value of the inverse is updated.

## The makeCacheMatrix stores a list of functions: set, get, setinv and getinv. The output of
## this function can be used to call the four functions mentioned above to set or get the input 
## matrix or the inverse of the matrix. In order to do this the user would have to use the "$" sign
## following the output from the makeCacheMatrix function. Lexical scoping is used (<<-) to set 
## a new value of the matrix or the inverse to change the value of the variable in a parent 
## environment rather than the function in which the variable is being defined.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
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


## The cacheSolve function is used as an interface that calculates the inverse of a given matrix
## after checking the cache to see if the inverse of the given matrix already exists. If the inverse
## exists from a previous call, cached data is extracted using the "getinv" function. Alternatively,
## if the inverse does not exist (= NULL), the "get" function is used to extract the value of the 
## matrix and the inverse of the new matrix is calculated and its value is updated using "setinv".

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
