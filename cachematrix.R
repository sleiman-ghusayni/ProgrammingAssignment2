## Matrix inversion is usually a costly computation and there may be some benefit to 
## caching the inverse of a matrix rather than compute it repeatedly. 
## makeCacheMatrix and cacheSolve functions cache the inverse of a matrix.



## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## return list containing  functions of a special "matrix"
  
  m <- NULL
  
  ## set the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## get the value of the matrix
  get <- function() x
  
  ## set the value of the solve
  setsolve <- function(solve) m <<- solve
  
  ## get the value of the solve
  getsolve <- function() m
  
  ## return list containing  functions
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x
   
   ##gets the inverse from the cache if the solve has already been calculated
    m <- x$getsolve()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    
    ## Computing the inverse of a square matrix with the solve function
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}

