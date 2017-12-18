#A list of four functions: get, set, setinverse and getinverse
#SetInverse allows the inverse to be set if it has been calculated
# The inverse remains at the initial NULL value if SetInverse is not called
#Set reads in the standard matrix and stores its value
# It also sets the inverse to NULL because any previous value it contained is no longer valid
#Get and GetInverse return the values of the matrix and the inverse respectively

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(calculatedInverse) inverse <<- calculatedInverse
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#Calls getInverse to see whether the inverse has previously been calculated and cached
#Returns that value if it exists or calls Solve and SetInverse if not
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
