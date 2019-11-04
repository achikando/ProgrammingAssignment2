## Put comments here that give an overall description of what your
## functions do

## this function takes in matrix argument 
## (i.e. matrix(c(2, -1, 0, -1, 2, -1, 0, -1, 2), 3) to create a special matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## this function uses "solve()"to compute, caches and return inverse of 
## the special matrix created above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
