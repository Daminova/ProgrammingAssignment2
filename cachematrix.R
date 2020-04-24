## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  # Define function to set the value of the matrix. It also clears the old
  # inverse from the cache
  set <- function(y) {
    x <<- y    # Set the value
    m <<- NULL # Clear the cache
  }
  # Define function to get the value of the matrix
  get <- function() x
  # Define function to set the inverse. This is only used by getinverse() when
  # there is no cached inverse
  setInverse <- function(inverse) m <<- inverse
  # Define function to get the inverse
  getInverse <- function() m
  
  # Return a list with the above four functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    ## @x: output of makeCacheMatrix()
    ## return: inverse of the original matrix input to makeCacheMatrix()
  inv = x$getinv()
  
  # if the inverse has already been calculated
  if (!is.null(inv)){
    # get it from the cache and skips the computation. 
    message("getting cached data")
    return(inv)
  }
  
  # otherwise, calculates the inverse 
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  # sets the value of the inverse in the cache via the setinv function.
  x$setinv(inv)
  
  return(inv)
}
