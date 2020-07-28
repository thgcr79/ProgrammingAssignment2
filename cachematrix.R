## caches the inverse of a matrix
## there are 4 functions
##1)get the matrix
##2)set the inverse of the matrix
##3)get the inverse of the matrix

makecachematrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse<- function(inverse) {inv <<-inverse}
  getinverse <- function() {inv}
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## returns the inverse of the matrix from the cache if the cache is empty calculates the inverse using the solve function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}