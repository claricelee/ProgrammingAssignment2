## 

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        ## Initializing s
  s <- NULL
  set <- function(z) {
    x <<- z
        ## Clearing any values of s that has been cached
    s <<- NULL
  }
        ## Defines getter for x
  get <- function() x
        ## Defines setter for solve s
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This function computes the inverse of the special 
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve()
        ## Retrieves value of s in cached data, if s is present
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
        ## is !is.null(s) is FALSE, cacheSolve gets from input object,
        ## calculates a solve(), uses setsolve() to set solve in input object,
        ## then returns the value to the parent environment
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
  
}