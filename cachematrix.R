# a square matrix is defined in order the inversion of this matrix to be find

# in this function we define the matrix that we want to inverse

makeCacheMatrix <- function(x = matrix()) {
  a <- NULL
  
  set <- function(y){
    x <<- y
    a <<- NULL
  }
  
  get <- function() x
  setsolve <- function(solve) a <<- solve
  getsolve <- function() a
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve) 

}


## this function inverts the matrix that have been defined above.

cacheSolve <- function(x, ...) {
  a <- x$getsolve()
  if(!is.null(a)) {
    message("getting cached data")
    return(a)
  }
  data <- x$get()
  a <- solve(data, ...)
  x$setsolve(a)
  a
}
