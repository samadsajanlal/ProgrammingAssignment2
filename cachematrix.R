## Put comments here that give an overall description of what your
## functions do
# these functions will first store the matrix fed into the "makeCacheMatrix" function
# then, it will calculate the inverse. if the stored matrix has not changed, it does not
# do new calculations after the first time
## Write a short comment describing this function
# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL # set the variable holding our matrix to null
  set <- function(y) { # the set function, when called, will set our matrix to the input
    x <<- y
    m <<- NULL
  }
  get <- function() x # return our input matrix
  setmatrix <- function(solve) m <<- solve # create an inverse of the matrix and set it
  getmatrix <- function() m # return the inverse matrix
  # return a list of functions we can use outside of this function to set and get
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
  m <- x$getmatrix() # get the inverse matrix
  if(!is.null(m)) { #is the matrix cached? return that instead
    message("getting cached data") #output to console
    return(m) # return the cached inverse matrix
  }
  data <- x$get() # inverse matrix was not cached, so lets get our input matrix
  m <- solve(data, ...) # inverse the input
  x$setmatrix(m) # set x = the inverse matrix
  m # return the inverse matrix
}
