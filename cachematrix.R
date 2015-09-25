## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## Gets the solution from earlier calculation
  m <- x$getsolve()
  ## Checks if the solution exists
  if(!is.null(m)) {
    ## If solution exists, pass message and return solution
    message("getting cached data")
    return(m)
  }
  ## (Else) get the matrix from the makeCacheMatrix function
  data <- x$get()
  ## Invert the matrix and store in m
  m <- solve(data, ...)
  ## Pass m to makeCacheMatrix function to store solution in cache
  x$setsolve(m)
  ## Return m
  m
}
