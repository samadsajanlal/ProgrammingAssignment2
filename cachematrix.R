## Put comments here that give an overall description of what your
## functions do
# these functions will first store the matrix fed into the "makeCacheMatrix" function
# then, it will calculate the inverse. if the stored matrix has not changed, it does not
# do new calculations after the first time
## Write a short comment describing this function
# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## Write a short comment describing this function
# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        # m contains the matrix that is the inverse of 'x'
  m <- x$getmatrix()
  if(!is.null(m)) { #is the matrix cached? return that instead
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}
