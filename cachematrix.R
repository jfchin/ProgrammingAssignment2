## Creates a special "matrix" object that can cache its inverse

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  
  ## function that changes the matrix stored in the main function
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## function that returns the matrix x stored in the main function
  get <- function() x
  
  setinv <- function(data) m <<- data
  
  getinv <- function() m
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## Computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinv()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## 
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
  
}
